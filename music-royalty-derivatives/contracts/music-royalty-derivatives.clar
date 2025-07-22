;; Music Royalty Derivatives Smart Contract
;; Trade future music earnings as financial instruments

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_DERIVATIVE_NOT_FOUND (err u103))
(define-constant ERR_DERIVATIVE_EXPIRED (err u104))
(define-constant ERR_INVALID_DURATION (err u105))
(define-constant ERR_ALREADY_CLAIMED (err u106))
(define-constant ERR_NOT_MATURE (err u107))
(define-constant ERR_INVALID_PRICE (err u108))
(define-constant ERR_TRADE_NOT_FOUND (err u109))
(define-constant ERR_CANNOT_TRADE_OWN (err u110))

;; Data structures
(define-map derivatives
  { derivative-id: uint }
  {
    artist: principal,
    song-title: (string-ascii 100),
    total-supply: uint,
    price-per-unit: uint,
    maturity-block: uint,
    total-earnings: uint,
    claimed: bool,
    created-at: uint
  }
)

(define-map derivative-holdings
  { holder: principal, derivative-id: uint }
  { units-held: uint }
)

(define-map trade-orders
  { order-id: uint }
  {
    seller: principal,
    derivative-id: uint,
    units-for-sale: uint,
    price-per-unit: uint,
    active: bool,
    created-at: uint
  }
)

(define-map artist-royalties
  { artist: principal }
  { total-deposited: uint, total-claimed: uint }
)

;; Data variables
(define-data-var next-derivative-id uint u1)
(define-data-var next-order-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% = 250 basis points

;; Events
(define-map derivative-events
  { event-id: uint }
  {
    event-type: (string-ascii 20),
    derivative-id: uint,
    user: principal,
    amount: uint,
    timestamp: uint
  }
)
(define-data-var next-event-id uint u1)

;; Helper function to emit events
(define-private (emit-event (event-type (string-ascii 20)) (derivative-id uint) (user principal) (amount uint))
  (let ((event-id (var-get next-event-id)))
    (map-set derivative-events
      { event-id: event-id }
      {
        event-type: event-type,
        derivative-id: derivative-id,
        user: user,
        amount: amount,
        timestamp: block-height
      }
    )
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

;; Create a new music royalty derivative
(define-public (create-derivative 
  (song-title (string-ascii 100))
  (total-supply uint)
  (price-per-unit uint)
  (duration-blocks uint))
  (let (
    (derivative-id (var-get next-derivative-id))
    (maturity-block (+ block-height duration-blocks))
  )
    (asserts! (> total-supply u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-unit u0) ERR_INVALID_PRICE)
    (asserts! (> duration-blocks u144) ERR_INVALID_DURATION) ;; Min 1 day (144 blocks)
    
    (map-set derivatives
      { derivative-id: derivative-id }
      {
        artist: tx-sender,
        song-title: song-title,
        total-supply: total-supply,
        price-per-unit: price-per-unit,
        maturity-block: maturity-block,
        total-earnings: u0,
        claimed: false,
        created-at: block-height
      }
    )
    
    ;; Artist holds all units initially
    (map-set derivative-holdings
      { holder: tx-sender, derivative-id: derivative-id }
      { units-held: total-supply }
    )
    
    (var-set next-derivative-id (+ derivative-id u1))
    (unwrap-panic (emit-event "CREATE" derivative-id tx-sender total-supply))
    (ok derivative-id)
  )
)

;; Purchase derivative units directly from artist
(define-public (purchase-derivative (derivative-id uint) (units uint))
  (let (
    (derivative (unwrap! (map-get? derivatives { derivative-id: derivative-id }) ERR_DERIVATIVE_NOT_FOUND))
    (artist (get artist derivative))
    (price-per-unit (get price-per-unit derivative))
    (total-cost (* units price-per-unit))
    (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
    (artist-payment (- total-cost platform-fee))
    (artist-holdings (default-to { units-held: u0 } 
      (map-get? derivative-holdings { holder: artist, derivative-id: derivative-id })))
    (buyer-holdings (default-to { units-held: u0 } 
      (map-get? derivative-holdings { holder: tx-sender, derivative-id: derivative-id })))
  )
    (asserts! (> units u0) ERR_INVALID_AMOUNT)
    (asserts! (<= units (get units-held artist-holdings)) ERR_INSUFFICIENT_BALANCE)
    (asserts! (< block-height (get maturity-block derivative)) ERR_DERIVATIVE_EXPIRED)
    
    ;; Transfer STX from buyer to artist and platform
    (try! (stx-transfer? artist-payment tx-sender artist))
    (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
    
    ;; Update holdings
    (map-set derivative-holdings
      { holder: artist, derivative-id: derivative-id }
      { units-held: (- (get units-held artist-holdings) units) }
    )
    
    (map-set derivative-holdings
      { holder: tx-sender, derivative-id: derivative-id }
      { units-held: (+ (get units-held buyer-holdings) units) }
    )
    
    (unwrap-panic (emit-event "PURCHASE" derivative-id tx-sender units))
    (ok true)
  )
)

;; Create a trade order
(define-public (create-trade-order (derivative-id uint) (units uint) (price-per-unit uint))
  (let (
    (order-id (var-get next-order-id))
    (holder-info (unwrap! 
      (map-get? derivative-holdings { holder: tx-sender, derivative-id: derivative-id }) 
      ERR_INSUFFICIENT_BALANCE))
  )
    (asserts! (> units u0) ERR_INVALID_AMOUNT)
    (asserts! (> price-per-unit u0) ERR_INVALID_PRICE)
    (asserts! (<= units (get units-held holder-info)) ERR_INSUFFICIENT_BALANCE)
    (asserts! (is-some (map-get? derivatives { derivative-id: derivative-id })) ERR_DERIVATIVE_NOT_FOUND)
    
    (map-set trade-orders
      { order-id: order-id }
      {
        seller: tx-sender,
        derivative-id: derivative-id,
        units-for-sale: units,
        price-per-unit: price-per-unit,
        active: true,
        created-at: block-height
      }
    )
    
    (var-set next-order-id (+ order-id u1))
    (ok order-id)
  )
)

;; Execute a trade order
(define-public (execute-trade-order (order-id uint) (units-to-buy uint))
  (let (
    (order (unwrap! (map-get? trade-orders { order-id: order-id }) ERR_TRADE_NOT_FOUND))
    (seller (get seller order))
    (derivative-id (get derivative-id order))
    (available-units (get units-for-sale order))
    (price-per-unit (get price-per-unit order))
    (total-cost (* units-to-buy price-per-unit))
    (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
    (seller-payment (- total-cost platform-fee))
    (seller-holdings (unwrap! 
      (map-get? derivative-holdings { holder: seller, derivative-id: derivative-id }) 
      ERR_INSUFFICIENT_BALANCE))
    (buyer-holdings (default-to { units-held: u0 } 
      (map-get? derivative-holdings { holder: tx-sender, derivative-id: derivative-id })))
  )
    (asserts! (get active order) ERR_TRADE_NOT_FOUND)
    (asserts! (not (is-eq tx-sender seller)) ERR_CANNOT_TRADE_OWN)
    (asserts! (> units-to-buy u0) ERR_INVALID_AMOUNT)
    (asserts! (<= units-to-buy available-units) ERR_INSUFFICIENT_BALANCE)
    (asserts! (<= units-to-buy (get units-held seller-holdings)) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer payment
    (try! (stx-transfer? seller-payment tx-sender seller))
    (try! (stx-transfer? platform-fee tx-sender CONTRACT_OWNER))
    
    ;; Update holdings
    (map-set derivative-holdings
      { holder: seller, derivative-id: derivative-id }
      { units-held: (- (get units-held seller-holdings) units-to-buy) }
    )
    
    (map-set derivative-holdings
      { holder: tx-sender, derivative-id: derivative-id }
      { units-held: (+ (get units-held buyer-holdings) units-to-buy) }
    )
    
    ;; Update or close order
    (if (is-eq units-to-buy available-units)
      (map-set trade-orders { order-id: order-id } (merge order { active: false }))
      (map-set trade-orders { order-id: order-id } 
        (merge order { units-for-sale: (- available-units units-to-buy) }))
    )
    
    (unwrap-panic (emit-event "TRADE" derivative-id tx-sender units-to-buy))
    (ok true)
  )
)

;; Artist deposits royalty earnings
(define-public (deposit-royalties (derivative-id uint) (amount uint))
  (let (
    (derivative (unwrap! (map-get? derivatives { derivative-id: derivative-id }) ERR_DERIVATIVE_NOT_FOUND))
    (artist (get artist derivative))
  )
    (asserts! (is-eq tx-sender artist) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update derivative earnings
    (map-set derivatives
      { derivative-id: derivative-id }
      (merge derivative { total-earnings: (+ (get total-earnings derivative) amount) })
    )
    
    ;; Update artist royalty tracking
    (let ((artist-royalty (default-to { total-deposited: u0, total-claimed: u0 } 
                          (map-get? artist-royalties { artist: artist }))))
      (map-set artist-royalties
        { artist: artist }
        (merge artist-royalty { total-deposited: (+ (get total-deposited artist-royalty) amount) })
      )
    )
    
    (unwrap-panic (emit-event "DEPOSIT" derivative-id tx-sender amount))
    (ok true)
  )
)

;; Claim proportional earnings after maturity
(define-public (claim-earnings (derivative-id uint))
  (let (
    (derivative (unwrap! (map-get? derivatives { derivative-id: derivative-id }) ERR_DERIVATIVE_NOT_FOUND))
    (holder-info (unwrap! 
      (map-get? derivative-holdings { holder: tx-sender, derivative-id: derivative-id }) 
      ERR_INSUFFICIENT_BALANCE))
    (units-held (get units-held holder-info))
    (total-supply (get total-supply derivative))
    (total-earnings (get total-earnings derivative))
    (proportional-earnings (/ (* total-earnings units-held) total-supply))
  )
    (asserts! (>= block-height (get maturity-block derivative)) ERR_NOT_MATURE)
    (asserts! (> units-held u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> proportional-earnings u0) ERR_INVALID_AMOUNT)
    
    ;; Transfer earnings to holder
    (try! (as-contract (stx-transfer? proportional-earnings tx-sender tx-sender)))
    
    ;; Clear holder's position
    (map-delete derivative-holdings { holder: tx-sender, derivative-id: derivative-id })
    
    (unwrap-panic (emit-event "CLAIM" derivative-id tx-sender proportional-earnings))
    (ok proportional-earnings)
  )
)

;; Read-only functions
(define-read-only (get-derivative (derivative-id uint))
  (map-get? derivatives { derivative-id: derivative-id })
)

(define-read-only (get-holdings (holder principal) (derivative-id uint))
  (default-to { units-held: u0 } 
    (map-get? derivative-holdings { holder: holder, derivative-id: derivative-id }))
)

(define-read-only (get-trade-order (order-id uint))
  (map-get? trade-orders { order-id: order-id })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-artist-royalties (artist principal))
  (default-to { total-deposited: u0, total-claimed: u0 } 
    (map-get? artist-royalties { artist: artist }))
)

;; Admin functions
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u1000) ERR_INVALID_AMOUNT) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (ok true)
  )
)