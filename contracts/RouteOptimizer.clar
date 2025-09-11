;; Route Optimization System for E-Waste Collection
;; Helps collectors plan efficient routes and schedule multiple pickups

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_ROUTE_NOT_FOUND (err u301))
(define-constant ERR_INVALID_SCHEDULE (err u302))
(define-constant ERR_ROUTE_FULL (err u303))
(define-constant ERR_TIME_SLOT_TAKEN (err u304))
(define-constant ERR_INVALID_COORDINATES (err u305))
(define-constant ERR_PICKUP_ALREADY_SCHEDULED (err u306))

;; Route configuration
(define-constant MAX_STOPS_PER_ROUTE u8)
(define-constant MAX_ROUTE_DURATION u480) ;; ~8 hours in blocks
(define-constant MIN_TIME_BETWEEN_STOPS u6) ;; ~1 hour minimum

;; Data variables
(define-data-var next-route-id uint u1)
(define-data-var route-creation-fee uint u100000) ;; fee to create route
(define-data-var total-routes-created uint u0)

;; Map collector routes with pickup scheduling
(define-map collector-routes uint
    {
        collector: principal,
        route-name: (string-ascii 50),
        start-location: (string-ascii 100),
        created-at: uint,
        scheduled-date: uint,
        estimated-duration: uint,
        total-stops: uint,
        route-status: (string-ascii 20),
        total-estimated-weight: uint,
        estimated-earnings: uint
    }
)

;; Individual route stops with coordinates and timing
(define-map route-stops uint
    {
        route-id: uint,
        stop-order: uint,
        pickup-id: uint,
        location: (string-ascii 100),
        estimated-time: uint,
        estimated-weight: uint,
        priority-level: uint, ;; 1-5, 5 being highest
        completion-status: (string-ascii 20)
    }
)

;; Track collector's active routes
(define-map collector-active-routes principal (list 5 uint))

;; Time slot reservations to prevent conflicts
(define-map time-slot-reservations
    { collector: principal, date-block: uint, time-slot: uint }
    {
        route-id: uint,
        reserved: bool,
        location-area: (string-ascii 100)
    }
)

;; Route performance tracking
(define-map route-performance uint
    {
        planned-duration: uint,
        actual-duration: uint,
        planned-weight: uint,
        actual-weight: uint,
        efficiency-score: uint,
        fuel-cost-estimate: uint,
        completion-percentage: uint
    }
)

;; Create optimized collection route
(define-public (create-collection-route 
    (route-name (string-ascii 50))
    (start-location (string-ascii 100))
    (scheduled-date uint))
    (let (
        (route-id (var-get next-route-id))
        (collector tx-sender)
        (creation-fee (var-get route-creation-fee))
    )
        ;; Validate inputs
        (asserts! (> scheduled-date stacks-block-height) ERR_INVALID_SCHEDULE)
        (asserts! (>= (stx-get-balance collector) creation-fee) (err u307))
        
        ;; Charge route creation fee
        (try! (stx-transfer? creation-fee collector (as-contract tx-sender)))
        
        ;; Create route
        (map-set collector-routes route-id
            {
                collector: collector,
                route-name: route-name,
                start-location: start-location,
                created-at: stacks-block-height,
                scheduled-date: scheduled-date,
                estimated-duration: u0,
                total-stops: u0,
                route-status: "PLANNING",
                total-estimated-weight: u0,
                estimated-earnings: u0
            }
        )
        
        ;; Add to collector's active routes
        (let (
            (current-routes (default-to (list) (map-get? collector-active-routes collector)))
        )
            (map-set collector-active-routes collector
                (unwrap! (as-max-len? (append current-routes route-id) u5) ERR_ROUTE_FULL)
            )
        )
        
        ;; Update counters
        (var-set next-route-id (+ route-id u1))
        (var-set total-routes-created (+ (var-get total-routes-created) u1))
        
        (ok route-id)
    )
)

;; Add pickup stop to route with optimization
(define-public (add-stop-to-route 
    (route-id uint)
    (pickup-id uint)
    (location (string-ascii 100))
    (estimated-weight uint)
    (priority-level uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) ERR_ROUTE_NOT_FOUND))
        (collector (get collector route))
        (current-stops (get total-stops route))
    )
        ;; Validate permissions and limits
        (asserts! (is-eq tx-sender collector) ERR_NOT_AUTHORIZED)
        (asserts! (< current-stops MAX_STOPS_PER_ROUTE) ERR_ROUTE_FULL)
        (asserts! (and (>= priority-level u1) (<= priority-level u5)) (err u308))
        (asserts! (is-eq (get route-status route) "PLANNING") (err u309))
        
        ;; Calculate optimal stop order and timing
        (let (
            (stop-order (+ current-stops u1))
            (estimated-time (calculate-stop-time route-id stop-order))
            (stop-id (generate-stop-id route-id stop-order))
        )
            ;; Add stop to route
            (map-set route-stops stop-id
                {
                    route-id: route-id,
                    stop-order: stop-order,
                    pickup-id: pickup-id,
                    location: location,
                    estimated-time: estimated-time,
                    estimated-weight: estimated-weight,
                    priority-level: priority-level,
                    completion-status: "SCHEDULED"
                }
            )
            
            ;; Update route totals
            (let (
                (new-total-weight (+ (get total-estimated-weight route) estimated-weight))
                (new-earnings (calculate-estimated-earnings new-total-weight))
            )
                (map-set collector-routes route-id
                    (merge route {
                        total-stops: (+ current-stops u1),
                        total-estimated-weight: new-total-weight,
                        estimated-earnings: new-earnings,
                        estimated-duration: (calculate-route-duration route-id)
                    })
                )
            )
            
            (ok stop-id)
        )
    )
)

;; Optimize route order based on location and priority
(define-public (optimize-route-order (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) ERR_ROUTE_NOT_FOUND))
        (collector (get collector route))
    )
        (asserts! (is-eq tx-sender collector) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get route-status route) "PLANNING") (err u309))
        (asserts! (> (get total-stops route) u1) (err u310))
        
        ;; Calculate optimized route efficiency
        (let (
            (efficiency-bonus (calculate-efficiency-bonus route-id))
            (optimized-duration (- (get estimated-duration route) efficiency-bonus))
        )
            ;; Update route with optimization
            (map-set collector-routes route-id
                (merge route {
                    estimated-duration: optimized-duration,
                    route-status: "OPTIMIZED"
                })
            )
            
            (ok efficiency-bonus)
        )
    )
)

;; Finalize and activate route for execution
(define-public (activate-route (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) ERR_ROUTE_NOT_FOUND))
        (collector (get collector route))
    )
        (asserts! (is-eq tx-sender collector) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get route-status route) "OPTIMIZED") (err u311))
        (asserts! (> (get total-stops route) u0) (err u312))
        
        ;; Reserve time slots for route execution
        (try! (reserve-route-time-slots route-id))
        
        ;; Activate route
        (map-set collector-routes route-id
            (merge route { route-status: "ACTIVE" })
        )
        
        (ok true)
    )
)

;; Mark stop as completed and update route progress
(define-public (complete-route-stop (route-id uint) (stop-order uint) (actual-weight uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) ERR_ROUTE_NOT_FOUND))
        (collector (get collector route))
        (stop-id (generate-stop-id route-id stop-order))
        (stop (unwrap! (map-get? route-stops stop-id) (err u313)))
    )
        (asserts! (is-eq tx-sender collector) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get route-status route) "ACTIVE") (err u314))
        
        ;; Update stop completion
        (map-set route-stops stop-id
            (merge stop { 
                completion-status: "COMPLETED",
                estimated-weight: actual-weight
            })
        )
        
        ;; Check if route is fully completed
        (let (
            (completion-percentage (calculate-route-completion route-id))
        )
            (if (>= completion-percentage u100)
                (begin
                    (map-set collector-routes route-id
                        (merge route { route-status: "COMPLETED" })
                    )
                    (finalize-route-performance route-id)
                )
                (ok false)
            )
        )
    )
)

;; Private helper functions
(define-private (calculate-stop-time (route-id uint) (stop-order uint))
    (if (is-eq stop-order u1)
        u0 ;; first stop starts immediately
        (* stop-order MIN_TIME_BETWEEN_STOPS)
    )
)

(define-private (generate-stop-id (route-id uint) (stop-order uint))
    (+ (* route-id u1000) stop-order)
)

(define-private (calculate-estimated-earnings (total-weight uint))
    (* total-weight u10) ;; base rate of 10 STX per unit weight
)

(define-private (calculate-route-duration (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) u0))
        (stops (get total-stops route))
    )
        (if (> stops u0)
            (+ (* stops MIN_TIME_BETWEEN_STOPS) u30) ;; base time + travel
            u0
        )
    )
)

(define-private (calculate-efficiency-bonus (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) u0))
        (stops (get total-stops route))
    )
        ;; More stops = better efficiency bonus
        (/ (* stops u10) u2)
    )
)

(define-private (reserve-route-time-slots (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) ERR_ROUTE_NOT_FOUND))
        (collector (get collector route))
        (scheduled-date (get scheduled-date route))
    )
        ;; Reserve primary time slot
        (map-set time-slot-reservations
            { collector: collector, date-block: scheduled-date, time-slot: u1 }
            {
                route-id: route-id,
                reserved: true,
                location-area: (get start-location route)
            }
        )
        (ok true)
    )
)

(define-private (calculate-route-completion (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) u0))
        (total-stops (get total-stops route))
    )
        (if (> total-stops u0)
            ;; Simple completion calculation - in production would check each stop
            (/ (* total-stops u100) total-stops) ;; simplified to 100%
            u0
        )
    )
)

(define-private (finalize-route-performance (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) ERR_ROUTE_NOT_FOUND))
        (efficiency-score (calculate-final-efficiency route-id))
    )
        (map-set route-performance route-id
            {
                planned-duration: (get estimated-duration route),
                actual-duration: (get estimated-duration route),
                planned-weight: (get total-estimated-weight route),
                actual-weight: (get total-estimated-weight route),
                efficiency-score: efficiency-score,
                fuel-cost-estimate: u500000, ;; estimated fuel cost
                completion-percentage: u100
            }
        )
        (ok true)
    )
)

(define-private (calculate-final-efficiency (route-id uint))
    (let (
        (route (unwrap! (map-get? collector-routes route-id) u0))
        (stops (get total-stops route))
        (weight (get total-estimated-weight route))
    )
        ;; Higher stops and weight = better efficiency
        (+ (* stops u20) (/ weight u10))
    )
)

;; Read-only functions
(define-read-only (get-collector-route (route-id uint))
    (map-get? collector-routes route-id)
)

(define-read-only (get-route-stop (route-id uint) (stop-order uint))
    (let (
        (stop-id (generate-stop-id route-id stop-order))
    )
        (map-get? route-stops stop-id)
    )
)

(define-read-only (get-collector-active-routes (collector principal))
    (default-to (list) (map-get? collector-active-routes collector))
)

(define-read-only (get-route-performance (route-id uint))
    (map-get? route-performance route-id)
)

(define-read-only (get-route-stats)
    {
        next-route-id: (var-get next-route-id),
        total-routes-created: (var-get total-routes-created),
        route-creation-fee: (var-get route-creation-fee),
        max-stops-per-route: MAX_STOPS_PER_ROUTE
    }
)

(define-read-only (is-time-slot-available (collector principal) (date-block uint) (time-slot uint))
    (is-none (map-get? time-slot-reservations 
        { collector: collector, date-block: date-block, time-slot: time-slot }))
)

;; Admin functions
(define-public (set-route-creation-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (var-set route-creation-fee new-fee)
        (ok true)
    )
)
