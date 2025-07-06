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
