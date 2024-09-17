;; Mainnet trait implementation
 (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

;; Testnet trait implementation
;; (use-trait nft-trait 'ST30VRDPRK19QWV21JW945ZQK4QYA69TD7F8T2SHH.nft-trait.nft-trait)

(define-non-fungible-token wstxmap uint)

;; Constants
(define-constant DEPLOYER tx-sender)

(define-constant ERR-NO-MORE-NFTS u100)
(define-constant ERR-ALREADY-MINTED u101)
(define-constant ERR-WRONG-MAP u102)
(define-constant ERR-CONTRACT-INITIALIZED u103)
(define-constant ERR-NOT-AUTHORIZED u104)
(define-constant ERR-INVALID-USER u105)
(define-constant ERR-LISTING u106)
(define-constant ERR-WRONG-COMMISSION u107)
(define-constant ERR-NOT-FOUND u108)
(define-constant ERR-UNWRAP u109)
(define-constant ERR-MINT-LIMIT u110)
(define-constant ERR-METADATA-FROZEN u111)
(define-constant ERR-SUPPLY-FROZEN u112)
(define-constant ERR-NOT-ACTIVE u113)
(define-constant ERR-NO-MORE-MINTS u114)
(define-constant ERR-INVALID-PERCENTAGE u115)

;; Nakamoto Block will exist only as unique NFT. not possible to bridge back
(define-constant ERR-NAKAMOTO-BLOCK u200)

;; Internal variables
(define-data-var mint-limit uint u166166)

;; last-id is used only to keep track of the total amount of minted nfts
(define-data-var last-id uint u1)

(define-data-var protocol-address principal tx-sender)
(define-data-var minter principal tx-sender)
(define-data-var ipfs-root (string-ascii 80) "https://stxmap.co/api/metadata/")
(define-data-var metadata-frozen bool false)
(define-data-var supply-frozen bool false)
(define-data-var unwrap-cost uint u100000)

;; Registry Active Maps
(define-map active-maps uint bool)
(define-map wrapped-maps uint bool)

(define-public (bridge-many (recipients (list 500 { to: principal, id: uint })))
  (fold check-err (map bridge-single recipients) (ok true))
)

(define-private (check-err (result (response bool uint)) (prior (response bool uint)))
  (match prior ok-value result err-value (err err-value))
)

(define-private (bridge-single (recipient { to: principal, id: uint }))

(begin
  (asserts! (is-eq tx-sender (var-get minter))  (err ERR-INVALID-USER))
  (bridge-map (get id recipient) (get to recipient))
))

(define-private (bridge-map (id uint) (to principal) )

  (let 
  (
    (last-nft-id (var-get last-id))
    (enabled (asserts! (<= last-nft-id (var-get mint-limit)) (err ERR-NO-MORE-NFTS)))
    (not-minted (asserts! (is-eq (is-wrapped id) false) (err ERR-ALREADY-MINTED)))
    (current-balance (get-balance to))
  )
  (print {expr: "mint", current-supply: last-nft-id, token-id: id, recipient: to})
  (begin
  (asserts! (is-eq tx-sender (var-get minter)) (err ERR-INVALID-USER))
  (asserts! (<= id (var-get mint-limit)) (err ERR-MINT-LIMIT))
  (map-set active-maps id true)
  (map-set wrapped-maps id true)
  (map-set token-count to (+ current-balance u1))
  (var-set last-id (+ last-nft-id u1))
  (nft-mint? wstxmap id to)
  )
))

(define-public (redeem-many (maps (list 500 { id: uint, memo: (buff 34)})))
  (fold check-err (map redeem-single maps) (ok true))
)

(define-private (redeem-single (maps { id: uint, memo: (buff 34)}))

(begin
  (asserts! (is-eq tx-sender (var-get minter))  (err ERR-INVALID-USER))
  (redeem-stx20 (get id maps) (get memo maps))
))

;; redeem function will send the original stx20 inscription to the current nft owner
(define-public (redeem-stx20 (id uint) (memo (buff 34)))
  (let (
        (sender tx-sender)
        (contract-vault (as-contract tx-sender))
        ;; function to compare id to memo
        (idstring (int-to-ascii id))
        (idmap (concat idstring ".stxmap"))
        (mapbuff (unwrap! (to-consensus-buff? idmap) (err ERR-UNWRAP) ))
        (maplen (len mapbuff))
        (mapstring (unwrap! (slice? mapbuff u1 maplen) (err ERR-UNWRAP) ))
        (memobuff (unwrap! (to-consensus-buff? memo) (err ERR-UNWRAP) ))
        (memolen (len memobuff))
        (memostring (unwrap! (slice? memobuff u1 memolen) (err ERR-UNWRAP) ))
      )
    (asserts! (is-eq memostring mapstring) (err ERR-WRONG-MAP))
    (asserts! (is-owner id tx-sender) (err ERR-NOT-AUTHORIZED))
    (asserts! (and  
              (not (is-eq id (var-get mint-limit))) 
              (is-eq (var-get supply-frozen) true)
              ) 
    (err ERR-NAKAMOTO-BLOCK))
    (map-set active-maps id false)
    (try! (stx-transfer? (var-get unwrap-cost) tx-sender (var-get protocol-address)))
    (try! (transfer id sender contract-vault))
    (try! (as-contract (stx-transfer-memo? u1 tx-sender sender memo)))
    (print {a: "stx20-redeem", id: id, map: idmap})
    (ok true)))

;; only the Minter can send the nft to the new owner
(define-public (unlock-nft (id uint) (to principal)) 
(begin 
  (asserts! (is-eq (var-get minter) tx-sender) (err u103))
  (try! (as-contract (transfer id tx-sender to)))
  (print {a: "unlock-nft", id: id})
  (ok true)
))

(define-public (burn (token-id uint))
  (begin 
    (asserts! (is-owner token-id tx-sender) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-eq (is-active token-id) true) (err ERR-NOT-ACTIVE))
    (map-set active-maps token-id false)
    (if (is-none (map-get? market token-id))
    (nft-burn? wstxmap token-id tx-sender)
    (begin
    (try! (unlist-in-ustx token-id))
    (nft-burn? wstxmap token-id tx-sender)
    ))))

(define-public (set-unwrap-cost (price uint))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-INVALID-USER))
    (ok (var-set unwrap-cost price))))

(define-public (set-minter-address (minter-address principal))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-INVALID-USER))
    (ok (var-set minter minter-address))))

(define-public (set-protocol-address (address principal))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-INVALID-USER))
    (ok (var-set protocol-address address))))

(define-public (set-mint-limit (limit uint))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-INVALID-USER))
    (asserts! (not (var-get supply-frozen)) (err ERR-SUPPLY-FROZEN))
    (ok (var-set mint-limit limit))))

(define-private (is-owner (token-id uint) (user principal))
    (is-eq user (unwrap! (nft-get-owner? wstxmap token-id) false)))

(define-public (set-base-uri (new-base-uri (string-ascii 80)))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-NOT-AUTHORIZED))
    (asserts! (not (var-get metadata-frozen)) (err ERR-METADATA-FROZEN))
    (print { notification: "token-metadata-update", payload: { token-class: "nft", contract-id: (as-contract tx-sender) }})
    (var-set ipfs-root new-base-uri)
    (ok true)))

(define-public (freeze-metadata)
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-NOT-AUTHORIZED))
    (var-set metadata-frozen true)
    (ok true)))

;; once the Nakamoto block will be picked up, total supply will be frozen
;; this block will be minted to the DEPLOYER address and publicily raffled on our website
(define-public (freeze-supply)
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-NOT-AUTHORIZED))
    (var-set supply-frozen true)
    (try! (nft-mint? wstxmap (var-get mint-limit) DEPLOYER))
    (ok true)))

;; Non-custodial SIP-009 transfer function
(define-public (transfer (id uint) (sender principal) (recipient principal))
  (begin
    (if (is-listed id)
    (unlist id sender recipient)
    (trnsfr id sender recipient)
    )
  )
  )

;; read-only functions
(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? wstxmap token-id)))

(define-read-only (get-last-token-id)
  (ok (- (var-get last-id) u1)))

(define-read-only (get-token-uri (token-id uint))
  (ok (some (concat (var-get ipfs-root) "{id}"))))

(define-read-only (get-protocol-address)
  (ok (var-get protocol-address)))

(define-read-only (get-mint-limit)
  (ok (var-get mint-limit)))

;; Checks if the stxmap is wrapped
(define-read-only (is-wrapped (id uint))
  (default-to false (map-get? wrapped-maps id))
)
;; Checks if the stxmap is wrapped and active. After unwrapping or burning the status changes to not active
(define-read-only (is-active (id uint))
  (default-to false (map-get? active-maps id))
)

(define-read-only (is-supply-frozen)
  (ok (var-get supply-frozen))
)

(define-read-only (is-metadata-frozen)
  (ok (var-get metadata-frozen))
)

(define-data-var license-uri (string-ascii 80) "https://arweave.net/zmc1WTspIhFyVY82bwfAIcIExLFH5lUcHHUN0wXg4W8/0")
(define-data-var license-name (string-ascii 40) "PUBLIC")

(define-read-only (get-license-uri)
  (ok (var-get license-uri)))
  
(define-read-only (get-license-name)
  (ok (var-get license-name)))
  
(define-public (set-license-uri (uri (string-ascii 80)))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-NOT-AUTHORIZED))
    (ok (var-set license-uri uri))))
    
(define-public (set-license-name (name (string-ascii 40)))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-NOT-AUTHORIZED))
    (ok (var-set license-name name))))

;; Non-custodial marketplace extras
;; Mainnet trait implementation
 (use-trait commission-trait 'SP3D6PV2ACBPEKYJTCMH7HEN02KP87QSP8KTEH335.commission-trait.commission)

;; Testnet trait implementation
;; (use-trait commission-trait 'ST30VRDPRK19QWV21JW945ZQK4QYA69TD7F8T2SHH.commission-trait.commission)


(define-map token-count principal uint)
(define-map market uint {price: uint, commission: principal, royalty: uint})

(define-read-only (get-balance (account principal))
  (default-to u0
    (map-get? token-count account)))

(define-private (trnsfr (id uint) (sender principal) (recipient principal))
  (match (nft-transfer? wstxmap id sender recipient)
    success
      (let
        ((sender-balance (get-balance sender))
        (recipient-balance (get-balance recipient)))
          (map-set token-count
            sender
            (- sender-balance u1))
          (map-set token-count
            recipient
            (+ recipient-balance u1))
          (ok success))
    error (err error)))

(define-private (is-sender-owner (id uint))
  (let ((owner (unwrap! (nft-get-owner? wstxmap id) false)))
    (or (is-eq tx-sender owner) (is-eq contract-caller owner))))

(define-read-only (get-listing-in-ustx (id uint))
  (map-get? market id))

(define-public (list-in-ustx (id uint) (price uint) (comm-trait <commission-trait>))
  (let ((listing  {price: price, commission: (contract-of comm-trait), royalty: (var-get royalty-percent)}))
    (asserts! (is-sender-owner id) (err ERR-NOT-AUTHORIZED))
    (asserts! (is-eq (is-active id) true) (err ERR-NOT-ACTIVE))
    (map-set market id listing)
    (print (merge listing {a: "list-in-ustx", id: id}))
    (ok true)))

(define-public (unlist-in-ustx (id uint))
  (begin
    (asserts! (is-sender-owner id) (err ERR-NOT-AUTHORIZED))
    (map-delete market id)
    (print {a: "unlist-in-ustx", id: id})
    (ok true)))

(define-private (unlist (id uint) (sender principal) (recipient principal))
  (begin
    (map-delete market id)
    (print {a: "unlist-in-ustx", id: id})
    (trnsfr id sender recipient)))

(define-read-only (is-listed (id uint)) 
  (is-some (get price (map-get? market id)))
)

(define-public (buy-in-ustx (id uint) (comm-trait <commission-trait>))
  (let ((owner (unwrap! (nft-get-owner? wstxmap id) (err ERR-NOT-FOUND)))
      (listing (unwrap! (map-get? market id) (err ERR-LISTING)))
      (price (get price listing))
      (royalty (get royalty listing)))
    (asserts! (is-eq (contract-of comm-trait) (get commission listing)) (err ERR-WRONG-COMMISSION))
    (try! (stx-transfer? price tx-sender owner))
    (try! (pay-royalty price royalty))
    (try! (contract-call? comm-trait pay id price))
    (try! (trnsfr id owner tx-sender))
    (map-delete market id)
    (print {a: "buy-in-ustx", id: id})
    (ok true)))
    
(define-data-var royalty-percent uint u100)

(define-read-only (get-royalty-percent)
  (ok (var-get royalty-percent)))

(define-public (set-royalty-percent (royalty uint))
  (begin
    (asserts! (or (is-eq tx-sender (var-get protocol-address)) (is-eq tx-sender DEPLOYER)) (err ERR-INVALID-USER))
    (asserts! (and (>= royalty u0) (<= royalty u1000)) (err ERR-INVALID-PERCENTAGE))
    (ok (var-set royalty-percent royalty))))

(define-private (pay-royalty (price uint) (royalty uint))
  (let (
    (royalty-amount (/ (* price royalty) u10000))
  )
  (if (and (> royalty-amount u0) (not (is-eq tx-sender (var-get protocol-address))))
    (try! (stx-transfer? royalty-amount tx-sender (var-get protocol-address)))
    (print false)
  )
  (ok true)))

;; implementation of bulk functions
;; bulk buy/sweep

(define-public (buy-many (maps (list 100 { commission: <commission-trait>, id: uint })))
  (fold check-err (map buy-single maps) (ok true))
)
(define-private (buy-single (maps { commission: <commission-trait>, id: uint }))

(begin
  (buy-in-ustx (get id maps) (get commission maps))
))

;; bulk listing
(define-public (list-many (maps (list 100 { commission: <commission-trait>, price: uint, id: uint })))
  (fold check-err (map list-single maps) (ok true))
)

(define-private (list-single (maps { commission: <commission-trait>, price: uint, id: uint }))

(begin
  (list-in-ustx (get id maps) (get price maps) (get commission maps))
))

;; bulk unlist
(define-public (unlist-many (maps (list 100 { id: uint })))
  (fold check-err (map unlist-single maps) (ok true))
)

(define-private (unlist-single (maps { id: uint }))

(begin
  (unlist-in-ustx (get id maps))
))

;; bulk transfer
(define-public (transfer-many (maps (list 100 { id: uint, to: principal })))
  (fold check-err (map transfer-single maps) (ok true))
)

(define-private (transfer-single (maps { id: uint, to: principal }))

(begin
  (transfer (get id maps) tx-sender (get to maps))
))
