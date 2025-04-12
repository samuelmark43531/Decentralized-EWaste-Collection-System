
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
