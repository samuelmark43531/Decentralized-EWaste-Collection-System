(define-constant ERR-INVALID-LOCATION (err u200))
(define-constant ERR-INVALID-MULTIPLIER (err u201))
(define-constant ERR-LOCATION-NOT-FOUND (err u202))

(define-data-var base-reward-rate uint u10)
(define-data-var max-multiplier uint u300)
(define-data-var min-multiplier uint u50)
(define-data-var price-admin principal tx-sender)

(define-map location-demand
    (string-ascii 50)
    {
        pending-pickups: uint,
        completed-pickups: uint,
        active-collectors: uint,
        total-weight: uint,
        average-completion-time: uint,
        price-multiplier: uint,
        last-updated: uint
    }
)

(define-map location-history
    {location: (string-ascii 50), height: uint}
    {
        demand-score: uint,
        supply-score: uint,
        price-multiplier: uint
    }
)

(define-public (register-location (location (string-ascii 50)))
    (begin
        (map-set location-demand location {
            pending-pickups: u0,
            completed-pickups: u0,
            active-collectors: u0,
            total-weight: u0,
            average-completion-time: u0,
            price-multiplier: u100,
            last-updated: stacks-block-height
        })
        (ok true)
    )
)

(define-public (schedule-pickup-dynamic (location (string-ascii 50)) (estimated-weight uint))
    (let
        (
            (location-data (unwrap! (map-get? location-demand location) ERR-LOCATION-NOT-FOUND))
            (current-multiplier (get price-multiplier location-data))
            (base-reward (* estimated-weight (var-get base-reward-rate)))
            (dynamic-reward (/ (* base-reward current-multiplier) u100))
        )
        (map-set location-demand location (merge location-data {
            pending-pickups: (+ (get pending-pickups location-data) u1),
            last-updated: stacks-block-height
        }))
        (try! (update-location-pricing location))
        (ok dynamic-reward)
    )
)

(define-public (complete-pickup-dynamic (location (string-ascii 50)) (weight uint) (completion-time uint))
    (let
        (
            (location-data (unwrap! (map-get? location-demand location) ERR-LOCATION-NOT-FOUND))
            (new-pending (if (> (get pending-pickups location-data) u0)
                            (- (get pending-pickups location-data) u1)
                            u0))
            (new-completed (+ (get completed-pickups location-data) u1))
            (new-total-weight (+ (get total-weight location-data) weight))
            (current-avg-time (get average-completion-time location-data))
            (new-avg-time (if (is-eq (get completed-pickups location-data) u0)
                             completion-time
                             (/ (+ (* current-avg-time (get completed-pickups location-data)) completion-time)
                                new-completed)))
        )
        (map-set location-demand location (merge location-data {
            pending-pickups: new-pending,
            completed-pickups: new-completed,
            total-weight: new-total-weight,
            average-completion-time: new-avg-time,
            last-updated: stacks-block-height
        }))
        (try! (update-location-pricing location))
        (ok true)
    )
)

(define-public (update-location-pricing (location (string-ascii 50)))
    (let
        (
            (location-data (unwrap! (map-get? location-demand location) ERR-LOCATION-NOT-FOUND))
            (demand-score (calculate-demand-score location-data))
            (supply-score (calculate-supply-score location-data))
            (new-multiplier (calculate-price-multiplier demand-score supply-score))
        )
        (map-set location-demand location (merge location-data {
            price-multiplier: new-multiplier,
            last-updated: stacks-block-height
        }))
        (map-set location-history {location: location, height: stacks-block-height} {
            demand-score: demand-score,
            supply-score: supply-score,
            price-multiplier: new-multiplier
        })
        (ok new-multiplier)
    )
)

(define-private (calculate-demand-score (location-data {pending-pickups: uint, completed-pickups: uint, active-collectors: uint, total-weight: uint, average-completion-time: uint, price-multiplier: uint, last-updated: uint}))
    (let
        (
            (pending-weight (get pending-pickups location-data))
            (completion-delay (if (> (get average-completion-time location-data) u100)
                                (- (get average-completion-time location-data) u100)
                                u0))
            (base-demand (+ (* pending-weight u20) (* completion-delay u10)))
        )
        (if (> base-demand u1000) u1000 base-demand)
    )
)

(define-private (calculate-supply-score (location-data {pending-pickups: uint, completed-pickups: uint, active-collectors: uint, total-weight: uint, average-completion-time: uint, price-multiplier: uint, last-updated: uint}))
    (let
        (
            (active-collectors (get active-collectors location-data))
            (completion-rate (if (> (get completed-pickups location-data) u0)
                               (/ (* (get completed-pickups location-data) u100)
                                  (+ (get completed-pickups location-data) (get pending-pickups location-data)))
                               u0))
            (base-supply (+ (* active-collectors u50) (* completion-rate u5)))
        )
        (if (> base-supply u1000) u1000 base-supply)
    )
)

(define-private (calculate-price-multiplier (demand-score uint) (supply-score uint))
    (let
        (
            (ratio (if (> supply-score u0)
                      (/ (* demand-score u100) supply-score)
                      u200))
            (raw-multiplier (+ u50 (/ ratio u2)))
        )
        (if (> raw-multiplier (var-get max-multiplier))
            (var-get max-multiplier)
            (if (< raw-multiplier (var-get min-multiplier))
                (var-get min-multiplier)
                raw-multiplier))
    )
)

(define-public (update-collector-presence (location (string-ascii 50)) (is-active bool))
    (let
        (
            (location-data (unwrap! (map-get? location-demand location) ERR-LOCATION-NOT-FOUND))
            (current-collectors (get active-collectors location-data))
            (new-collectors (if is-active
                               (+ current-collectors u1)
                               (if (> current-collectors u0)
                                   (- current-collectors u1)
                                   u0)))
        )
        (map-set location-demand location (merge location-data {
            active-collectors: new-collectors,
            last-updated: stacks-block-height
        }))
        (try! (update-location-pricing location))
        (ok true)
    )
)

(define-public (batch-update-locations (locations (list 10 (string-ascii 50))))
    (let
        (
            (update-results (map update-single-location-safe locations))
        )
        (ok update-results)
    )
)

(define-private (update-single-location-safe (location (string-ascii 50)))
    (match (map-get? location-demand location)
        location-data (unwrap-panic (update-location-pricing location))
        u0
    )
)

(define-read-only (get-location-pricing (location (string-ascii 50)))
    (map-get? location-demand location)
)

(define-read-only (get-dynamic-reward (location (string-ascii 50)) (weight uint))
    (match (map-get? location-demand location)
        location-data
        (let
            (
                (base-reward (* weight (var-get base-reward-rate)))
                (multiplier (get price-multiplier location-data))
            )
            (ok (/ (* base-reward multiplier) u100))
        )
        ERR-LOCATION-NOT-FOUND
    )
)

(define-read-only (get-location-history (location (string-ascii 50)) (height uint))
    (map-get? location-history {location: location, height: height})
)

(define-read-only (get-pricing-stats (location (string-ascii 50)))
    (match (map-get? location-demand location)
        location-data
        (ok {
            current-multiplier: (get price-multiplier location-data),
            demand-level: (if (> (get pending-pickups location-data) u5) "HIGH" 
                             (if (> (get pending-pickups location-data) u2) "MEDIUM" "LOW")),
            supply-level: (if (> (get active-collectors location-data) u3) "HIGH"
                             (if (> (get active-collectors location-data) u1) "MEDIUM" "LOW")),
            avg-completion-time: (get average-completion-time location-data)
        })
        ERR-LOCATION-NOT-FOUND
    )
)

(define-public (set-pricing-bounds (new-min uint) (new-max uint))
    (begin
        (asserts! (is-eq tx-sender (var-get price-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (and (> new-min u0) (< new-min new-max)) ERR-INVALID-MULTIPLIER)
        (var-set min-multiplier new-min)
        (var-set max-multiplier new-max)
        (ok true)
    )
)

(define-public (set-base-reward-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get price-admin)) ERR-NOT-AUTHORIZED)
        (var-set base-reward-rate new-rate)
        (ok true)
    )
)

(define-public (transfer-pricing-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get price-admin)) ERR-NOT-AUTHORIZED)
        (var-set price-admin new-admin)
        (ok true)
    )
)

(define-constant ERR-NOT-AUTHORIZED (err u100))

;; Carbon Credit & Environmental Impact System
(define-constant ERR-INVALID-MATERIAL (err u300))
(define-constant ERR-INVALID-CARBON-AMOUNT (err u301))
(define-constant ERR-INSUFFICIENT-CREDITS (err u302))
(define-constant ERR-CREDIT-NOT-FOUND (err u303))
(define-constant ERR-INVALID-VERIFICATION (err u304))

;; Material carbon factors per kg (in carbon credits * 1000 for precision)
(define-data-var smartphones-factor uint u2500)    ;; 2.5 credits per kg
(define-data-var laptops-factor uint u3000)        ;; 3.0 credits per kg
(define-data-var monitors-factor uint u1500)       ;; 1.5 credits per kg
(define-data-var batteries-factor uint u4000)      ;; 4.0 credits per kg
(define-data-var cables-factor uint u500)          ;; 0.5 credits per kg
(define-data-var misc-electronics-factor uint u1000) ;; 1.0 credits per kg

(define-data-var carbon-admin principal tx-sender)
(define-data-var total-carbon-credits uint u0)
(define-data-var credit-counter uint u0)

;; Track environmental impact per pickup
(define-map pickup-environmental-data
    uint ;; pickup-id
    {
        material-type: (string-ascii 20),
        weight: uint,
        carbon-credits-earned: uint,
        co2-offset: uint,        ;; in grams
        verified: bool,
        verification-timestamp: uint
    }
)

;; Track user carbon credits and environmental stats
(define-map user-carbon-profile
    principal
    {
        total-credits-earned: uint,
        total-credits-spent: uint,
        total-co2-offset: uint,
        verified-pickups: uint,
        environmental-score: uint,
        last-activity: uint
    }
)

;; Individual carbon credit tokens for trading
(define-map carbon-credits
    uint ;; credit-id
    {
        owner: principal,
        pickup-id: uint,
        amount: uint,
        material-source: (string-ascii 20),
        issue-date: uint,
        verified: bool,
        transferable: bool
    }
)

;; Material verification data
(define-map material-verification
    {pickup-id: uint, verifier: principal}
    {
        material-confirmed: (string-ascii 20),
        weight-confirmed: uint,
        verification-notes: (optional (string-ascii 100)),
        verification-date: uint,
        confidence-score: uint  ;; 1-100
    }
)

;; Calculate carbon credits based on material type and weight
(define-private (calculate-carbon-credits (material-type (string-ascii 20)) (weight uint))
    (let
        (
            (factor (if (is-eq material-type "smartphones")
                       (var-get smartphones-factor)
                       (if (is-eq material-type "laptops")
                           (var-get laptops-factor)
                           (if (is-eq material-type "monitors")
                               (var-get monitors-factor)
                               (if (is-eq material-type "batteries")
                                   (var-get batteries-factor)
                                   (if (is-eq material-type "cables")
                                       (var-get cables-factor)
                                       (var-get misc-electronics-factor)))))))
        )
        ;; Credits = (weight * factor) / 1000 for precision
        (/ (* weight factor) u1000)
    )
)

;; Register environmental impact for a pickup
(define-public (register-environmental-impact (pickup-id uint) (material-type (string-ascii 20)) (weight uint))
    (let
        (
            (calculated-credits (calculate-carbon-credits material-type weight))
            (co2-offset (* weight u500)) ;; Estimated 500g CO2 per kg of e-waste
        )
        ;; Validate material type
        (asserts! (or (is-eq material-type "smartphones")
                      (is-eq material-type "laptops") 
                      (is-eq material-type "monitors")
                      (is-eq material-type "batteries")
                      (is-eq material-type "cables")
                      (is-eq material-type "misc-electronics")) ERR-INVALID-MATERIAL)
        
        ;; Store environmental data
        (map-set pickup-environmental-data pickup-id {
            material-type: material-type,
            weight: weight,
            carbon-credits-earned: calculated-credits,
            co2-offset: co2-offset,
            verified: false,
            verification-timestamp: u0
        })
        
        ;; Update user carbon profile
        (update-user-carbon-profile tx-sender calculated-credits co2-offset)
        
        ;; Update global stats
        (var-set total-carbon-credits (+ (var-get total-carbon-credits) calculated-credits))
        
        (ok calculated-credits)
    )
)

;; Issue tradeable carbon credit tokens
(define-public (issue-carbon-credit-token (pickup-id uint))
    (let
        (
            (env-data (unwrap! (map-get? pickup-environmental-data pickup-id) ERR-CREDIT-NOT-FOUND))
            (credit-id (+ (var-get credit-counter) u1))
        )
        ;; Only issue if verified
        (asserts! (get verified env-data) ERR-INVALID-VERIFICATION)
        
        ;; Create transferable credit token
        (map-set carbon-credits credit-id {
            owner: tx-sender,
            pickup-id: pickup-id,
            amount: (get carbon-credits-earned env-data),
            material-source: (get material-type env-data),
            issue-date: stacks-block-height,
            verified: true,
            transferable: true
        })
        
        (var-set credit-counter credit-id)
        (ok credit-id)
    )
)

;; Transfer carbon credit tokens between users
(define-public (transfer-carbon-credit (credit-id uint) (recipient principal))
    (let
        (
            (credit (unwrap! (map-get? carbon-credits credit-id) ERR-CREDIT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get owner credit)) ERR-NOT-AUTHORIZED)
        (asserts! (get transferable credit) ERR-INVALID-CARBON-AMOUNT)
        
        ;; Update credit owner
        (map-set carbon-credits credit-id (merge credit {
            owner: recipient
        }))
        
        ;; Update sender profile (decrease credits)
        (let
            (
                (sender-profile (get-user-carbon-profile-safe tx-sender))
            )
            (map-set user-carbon-profile tx-sender (merge sender-profile {
                total-credits-spent: (+ (get total-credits-spent sender-profile) (get amount credit))
            }))
        )
        
        ;; Update recipient profile (increase credits)
        (let
            (
                (recipient-profile (get-user-carbon-profile-safe recipient))
            )
            (map-set user-carbon-profile recipient (merge recipient-profile {
                total-credits-earned: (+ (get total-credits-earned recipient-profile) (get amount credit)),
                last-activity: stacks-block-height
            }))
        )
        
        (ok true)
    )
)

;; Verify material type and weight by authorized verifiers
(define-public (verify-material (pickup-id uint) (confirmed-material (string-ascii 20)) (confirmed-weight uint) (notes (optional (string-ascii 100))) (confidence uint))
    (let
        (
            (env-data (unwrap! (map-get? pickup-environmental-data pickup-id) ERR-CREDIT-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (var-get carbon-admin)) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= confidence u1) (<= confidence u100)) ERR-INVALID-VERIFICATION)
        
        ;; Store verification data
        (map-set material-verification {pickup-id: pickup-id, verifier: tx-sender} {
            material-confirmed: confirmed-material,
            weight-confirmed: confirmed-weight,
            verification-notes: notes,
            verification-date: stacks-block-height,
            confidence-score: confidence
        })
        
        ;; Mark as verified if confidence is high enough
        (if (>= confidence u80)
            (begin
                (map-set pickup-environmental-data pickup-id (merge env-data {
                    verified: true,
                    verification-timestamp: stacks-block-height
                }))
                ;; Update user verified pickup count
                (let
                    (
                        (user-profile (get-user-carbon-profile-safe tx-sender))
                    )
                    (map-set user-carbon-profile tx-sender (merge user-profile {
                        verified-pickups: (+ (get verified-pickups user-profile) u1)
                    }))
                )
            )
            true
        )
        
        (ok true)
    )
)

;; Helper function to get user carbon profile with defaults
(define-private (get-user-carbon-profile-safe (user principal))
    (default-to {
        total-credits-earned: u0,
        total-credits-spent: u0,
        total-co2-offset: u0,
        verified-pickups: u0,
        environmental-score: u0,
        last-activity: u0
    } (map-get? user-carbon-profile user))
)

;; Update user carbon profile
(define-private (update-user-carbon-profile (user principal) (credits uint) (co2-offset uint))
    (let
        (
            (current-profile (get-user-carbon-profile-safe user))
            (new-score (calculate-environmental-score 
                           (+ (get total-credits-earned current-profile) credits)
                           (+ (get verified-pickups current-profile) u1)
                           (+ (get total-co2-offset current-profile) co2-offset)))
        )
        (map-set user-carbon-profile user {
            total-credits-earned: (+ (get total-credits-earned current-profile) credits),
            total-credits-spent: (get total-credits-spent current-profile),
            total-co2-offset: (+ (get total-co2-offset current-profile) co2-offset),
            verified-pickups: (get verified-pickups current-profile),
            environmental-score: new-score,
            last-activity: stacks-block-height
        })
    )
)

;; Calculate environmental impact score
(define-private (calculate-environmental-score (total-credits uint) (verified-pickups uint) (co2-offset uint))
    (let
        (
            (credit-score (/ total-credits u10))
            (pickup-score (* verified-pickups u20))
            (offset-score (/ co2-offset u1000))
        )
        (+ credit-score pickup-score offset-score)
    )
)

;; Read-only functions
(define-read-only (get-pickup-environmental-data (pickup-id uint))
    (map-get? pickup-environmental-data pickup-id)
)

(define-read-only (get-user-carbon-profile (user principal))
    (map-get? user-carbon-profile user)
)

(define-read-only (get-carbon-credit (credit-id uint))
    (map-get? carbon-credits credit-id)
)

(define-read-only (get-material-verification (pickup-id uint) (verifier principal))
    (map-get? material-verification {pickup-id: pickup-id, verifier: verifier})
)

(define-read-only (get-total-platform-impact)
    (ok {
        total-credits: (var-get total-carbon-credits),
        total-credits-issued: (var-get credit-counter)
    })
)

;; Admin functions
(define-public (update-carbon-factors (material (string-ascii 20)) (new-factor uint))
    (begin
        (asserts! (is-eq tx-sender (var-get carbon-admin)) ERR-NOT-AUTHORIZED)
        (if (is-eq material "smartphones")
            (var-set smartphones-factor new-factor)
            (if (is-eq material "laptops")
                (var-set laptops-factor new-factor)
                (if (is-eq material "monitors")
                    (var-set monitors-factor new-factor)
                    (if (is-eq material "batteries")
                        (var-set batteries-factor new-factor)
                        (if (is-eq material "cables")
                            (var-set cables-factor new-factor)
                            (var-set misc-electronics-factor new-factor))))))
        (ok true)
    )
)

(define-public (transfer-carbon-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get carbon-admin)) ERR-NOT-AUTHORIZED)
        (var-set carbon-admin new-admin)
        (ok true)
    )
)


