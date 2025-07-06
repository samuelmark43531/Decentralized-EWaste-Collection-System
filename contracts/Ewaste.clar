
;; title: Ewaste
;; version:
;; summary:
;; description:



(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PICKUP (err u101))
(define-constant ERR-ALREADY-CLAIMED (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INVALID-WEIGHT (err u104))

(define-data-var admin principal tx-sender)
(define-data-var pickup-counter uint u0)
(define-data-var reward-rate uint u10)

(define-map pickups
    uint 
    {
        requester: principal,
        location: (string-ascii 50),
        weight: uint,
        status: (string-ascii 20),
        reward: uint,
        collector: (optional principal),
        timestamp: uint
    }
)

(define-map user-stats
    principal
    {
        total-pickups: uint,
        total-weight: uint,
        total-rewards: uint
    }
)

(define-public (schedule-pickup (location (string-ascii 50)) (estimated-weight uint))
    (let
        (
            (pickup-id (+ (var-get pickup-counter) u1))
            (reward (* estimated-weight (var-get reward-rate)))
        )
        (try! (stx-transfer? reward tx-sender (as-contract tx-sender)))
        (map-set pickups pickup-id {
            requester: tx-sender,
            location: location,
            weight: estimated-weight,
            status: "PENDING",
            reward: reward,
            collector: none,
            timestamp: stacks-block-height
        })
        (var-set pickup-counter pickup-id)
        (ok pickup-id)
    )
)

(define-public (accept-pickup (pickup-id uint))
    (let
        (
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
        )
        (asserts! (is-eq (get status pickup) "PENDING") ERR-INVALID-STATUS)
        (ok (map-set pickups pickup-id (merge pickup {
            status: "ACCEPTED",
            collector: (some tx-sender)
        })))
    )
)

(define-public (complete-pickup (pickup-id uint) (actual-weight uint))
    (let
        (
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
            (collector (unwrap! (get collector pickup) ERR-NOT-AUTHORIZED))
        )
        (asserts! (is-eq tx-sender collector) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status pickup) "ACCEPTED") ERR-INVALID-STATUS)
        (asserts! (<= (- actual-weight (get weight pickup)) u5) ERR-INVALID-WEIGHT)
        
        (try! (as-contract (stx-transfer? (get reward pickup) tx-sender collector)))
        
        (map-set pickups pickup-id (merge pickup {
            status: "COMPLETED",
            weight: actual-weight
        }))
        
        (update-stats collector actual-weight (get reward pickup))
        (ok true)
    )
)

(define-private (update-stats (user principal) (weight uint) (reward uint))
    (let
        (
            (current-stats (default-to {
                total-pickups: u0,
                total-weight: u0,
                total-rewards: u0
            } (map-get? user-stats user)))
        )
        (map-set user-stats user {
            total-pickups: (+ (get total-pickups current-stats) u1),
            total-weight: (+ (get total-weight current-stats) weight),
            total-rewards: (+ (get total-rewards current-stats) reward)
        })
    )
)

(define-read-only (get-pickup (pickup-id uint))
    (map-get? pickups pickup-id)
)

(define-read-only (get-user-stats (user principal))
    (map-get? user-stats user)
)

(define-public (set-reward-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set reward-rate new-rate)
        (ok true)
    )
)

(define-public (transfer-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)
    )
)


(define-constant ERR-NO-ACTIVE-DISPUTE (err u105))
(define-constant ERR-DISPUTE-EXISTS (err u106))
(define-constant ERR-NOT-DISPUTE-PARTY (err u107))
(define-constant ERR-DISPUTE-RESOLVED (err u108))

(define-map disputes
    uint
    {
        pickup-id: uint,
        requester: principal,
        collector: principal,
        reason: (string-ascii 100),
        requester-evidence: (optional (string-ascii 200)),
        collector-evidence: (optional (string-ascii 200)),
        status: (string-ascii 20),
        resolution: (optional (string-ascii 100)),
        timestamp: uint
    }
)

(define-data-var dispute-counter uint u0)

;; Function to file a dispute
(define-public (file-dispute (pickup-id uint) (reason (string-ascii 100)))
    (let
        (
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
            (collector (unwrap! (get collector pickup) ERR-INVALID-PICKUP))
            (dispute-id (+ (var-get dispute-counter) u1))
        )
        ;; Only requester or collector can file a dispute
        (asserts! (or (is-eq tx-sender (get requester pickup)) 
                      (is-eq tx-sender collector)) 
                  ERR-NOT-AUTHORIZED)
        
        ;; Dispute can only be filed for ACCEPTED or COMPLETED pickups
        (asserts! (or (is-eq (get status pickup) "ACCEPTED") 
                      (is-eq (get status pickup) "COMPLETED")) 
                  ERR-INVALID-STATUS)
        
        ;; Create the dispute
        (map-set disputes dispute-id {
            pickup-id: pickup-id,
            requester: (get requester pickup),
            collector: collector,
            reason: reason,
            requester-evidence: none,
            collector-evidence: none,
            status: "OPEN",
            resolution: none,
            timestamp: stacks-block-height
        })
        
        ;; Update pickup status to DISPUTED
        (map-set pickups pickup-id (merge pickup {
            status: "DISPUTED"
        }))
        
        (var-set dispute-counter dispute-id)
        (ok dispute-id)
    )
)

;; Function to submit evidence
(define-public (submit-evidence (dispute-id uint) (evidence (string-ascii 200)))
    (let
        (
            (dispute (unwrap! (map-get? disputes dispute-id) ERR-NO-ACTIVE-DISPUTE))
        )
        ;; Only parties involved can submit evidence
        (asserts! (or (is-eq tx-sender (get requester dispute)) 
                      (is-eq tx-sender (get collector dispute))) 
                  ERR-NOT-DISPUTE-PARTY)
        
        ;; Dispute must be open
        (asserts! (is-eq (get status dispute) "OPEN") ERR-DISPUTE-RESOLVED)
        
        ;; Update the appropriate evidence field
        (if (is-eq tx-sender (get requester dispute))
            (map-set disputes dispute-id (merge dispute {
                requester-evidence: (some evidence)
            }))
            (map-set disputes dispute-id (merge dispute {
                collector-evidence: (some evidence)
            }))
        )
        
        (ok true)
    )
)

;; Function for admin to resolve dispute
(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 100)) (refund-percentage uint))
    (let
        (
            (dispute (unwrap! (map-get? disputes dispute-id) ERR-NO-ACTIVE-DISPUTE))
            (pickup-id (get pickup-id dispute))
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
            (refund-amount (/ (* (get reward pickup) refund-percentage) u100))
        )
        ;; Only admin can resolve disputes
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        
        ;; Dispute must be open
        (asserts! (is-eq (get status dispute) "OPEN") ERR-DISPUTE-RESOLVED)
        
        ;; If refund is needed, transfer from contract to requester
        (if (> refund-amount u0)
            (try! (as-contract (stx-transfer? refund-amount tx-sender (get requester dispute))))
            true
        )
        
        ;; Update dispute status
        (map-set disputes dispute-id (merge dispute {
            status: "RESOLVED",
            resolution: (some resolution)
        }))
        
        ;; Update pickup status
        (map-set pickups pickup-id (merge pickup {
            status: "RESOLVED"
        }))
        
        (ok true)
    )
)

;; Read-only function to get dispute details
(define-read-only (get-dispute (dispute-id uint))
    (map-get? disputes dispute-id)
)

(define-constant ERR-INSUFFICIENT-REPUTATION (err u109))
(define-constant ERR-INVALID-RATING (err u110))

(define-data-var min-reputation-threshold uint u50)

(define-map collector-reputation
    principal
    {
        total-score: uint,
        completed-pickups: uint,
        average-rating: uint,
        on-time-deliveries: uint,
        disputes-against: uint,
        reputation-level: (string-ascii 20)
    }
)

(define-map pickup-ratings
    uint
    {
        rated: bool,
        rating: uint,
        feedback: (optional (string-ascii 100))
    }
)

(define-public (rate-pickup (pickup-id uint) (rating uint) (feedback (optional (string-ascii 100))))
    (let
        (
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
            (collector (unwrap! (get collector pickup) ERR-INVALID-PICKUP))
            (existing-rating (map-get? pickup-ratings pickup-id))
        )
        (asserts! (is-eq tx-sender (get requester pickup)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status pickup) "COMPLETED") ERR-INVALID-STATUS)
        (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
        (asserts! (is-none existing-rating) ERR-ALREADY-CLAIMED)
        
        (map-set pickup-ratings pickup-id {
            rated: true,
            rating: rating,
            feedback: feedback
        })
        
        (update-collector-reputation collector rating)
        (ok true)
    )
)

(define-private (update-collector-reputation (collector principal) (new-rating uint))
    (let
        (
            (current-rep (default-to {
                total-score: u0,
                completed-pickups: u0,
                average-rating: u0,
                on-time-deliveries: u0,
                disputes-against: u0,
                reputation-level: "BRONZE"
            } (map-get? collector-reputation collector)))
            (new-total-score (+ (get total-score current-rep) new-rating))
            (new-completed (+ (get completed-pickups current-rep) u1))
            (new-average (/ new-total-score new-completed))
            (new-level (calculate-reputation-level new-average new-completed))
        )
        (map-set collector-reputation collector {
            total-score: new-total-score,
            completed-pickups: new-completed,
            average-rating: new-average,
            on-time-deliveries: (get on-time-deliveries current-rep),
            disputes-against: (get disputes-against current-rep),
            reputation-level: new-level
        })
    )
)

(define-private (calculate-reputation-level (average-rating uint) (completed-pickups uint))
    (if (and (>= average-rating u4) (>= completed-pickups u50))
        "GOLD"
        (if (and (>= average-rating u3) (>= completed-pickups u20))
            "SILVER"
            "BRONZE"
        )
    )
)

(define-public (accept-pickup-with-reputation-check (pickup-id uint))
    (let
        (
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
            (collector-rep (map-get? collector-reputation tx-sender))
        )
        (asserts! (is-eq (get status pickup) "PENDING") ERR-INVALID-STATUS)
        
        (match collector-rep
            rep (asserts! (>= (get average-rating rep) (var-get min-reputation-threshold)) ERR-INSUFFICIENT-REPUTATION)
            true
        )
        
        (ok (map-set pickups pickup-id (merge pickup {
            status: "ACCEPTED",
            collector: (some tx-sender)
        })))
    )
)

(define-private (update-dispute-reputation (collector principal))
    (let
        (
            (current-rep (default-to {
                total-score: u0,
                completed-pickups: u0,
                average-rating: u0,
                on-time-deliveries: u0,
                disputes-against: u0,
                reputation-level: "BRONZE"
            } (map-get? collector-reputation collector)))
        )
        (map-set collector-reputation collector (merge current-rep {
            disputes-against: (+ (get disputes-against current-rep) u1)
        }))
    )
)

(define-public (resolve-dispute-with-reputation (dispute-id uint) (resolution (string-ascii 100)) (refund-percentage uint) (penalize-collector bool))
    (let
        (
            (dispute (unwrap! (map-get? disputes dispute-id) ERR-NO-ACTIVE-DISPUTE))
            (pickup-id (get pickup-id dispute))
            (pickup (unwrap! (map-get? pickups pickup-id) ERR-INVALID-PICKUP))
            (refund-amount (/ (* (get reward pickup) refund-percentage) u100))
        )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) "OPEN") ERR-DISPUTE-RESOLVED)
        
        (if (> refund-amount u0)
            (try! (as-contract (stx-transfer? refund-amount tx-sender (get requester dispute))))
            true
        )
        
        (if penalize-collector
            (update-dispute-reputation (get collector dispute))
            true
        )
        
        (map-set disputes dispute-id (merge dispute {
            status: "RESOLVED",
            resolution: (some resolution)
        }))
        
        (map-set pickups pickup-id (merge pickup {
            status: "RESOLVED"
        }))
        
        (ok true)
    )
)

(define-read-only (get-collector-reputation (collector principal))
    (map-get? collector-reputation collector)
)

(define-read-only (get-pickup-rating (pickup-id uint))
    (map-get? pickup-ratings pickup-id)
)

(define-read-only (get-top-collectors (limit uint))
    (ok "Query top collectors by reputation - implement off-chain indexing")
)

(define-public (set-min-reputation-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-threshold u5) ERR-INVALID-RATING)
        (var-set min-reputation-threshold new-threshold)
        (ok true)
    )
)