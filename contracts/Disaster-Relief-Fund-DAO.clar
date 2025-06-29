(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-NO-ACTIVE-DISASTER (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))

(define-data-var dao-owner principal tx-sender)
(define-data-var total-funds uint u0)
(define-data-var active-disaster-id (optional uint) none)
(define-data-var proposal-counter uint u0)
(define-data-var min-proposal-duration uint u144) ;; ~1 day in blocks
(define-data-var quorum-threshold uint u5)

(define-map disasters 
    uint 
    {
        name: (string-ascii 50),
        location: (string-ascii 50),
        required-funds: uint,
        status: (string-ascii 10),
        declaration-height: uint
    }
)

(define-map dao-members 
    principal 
    {
        joined-height: uint,
        contribution: uint,
        voting-power: uint
    }
)

(define-map proposals
    uint 
    {
        disaster-id: uint,
        amount: uint,
        recipient: principal,
        start-height: uint,
        end-height: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 10)
    }
)

(define-map votes 
    {proposal-id: uint, voter: principal} 
    bool
)

(define-public (initialize-dao (owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set dao-owner owner)
        (ok true)
    )
)

(define-public (join-dao)
    (let ((current-height (- burn-block-height u1)))
        (map-set dao-members tx-sender {
            joined-height: current-height,
            contribution: u0,
            voting-power: u1
        })
        (ok true)
    )
)

(define-public (contribute)
    (let (
        (amount (stx-get-balance tx-sender))
        (member-data (default-to 
            {joined-height: u0, contribution: u0, voting-power: u0}
            (map-get? dao-members tx-sender)
        ))
    )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-funds (+ (var-get total-funds) amount))
        (map-set dao-members tx-sender 
            (merge member-data {
                contribution: (+ (get contribution member-data) amount),
                voting-power: (+ (get voting-power member-data) u1)
            })
        )
        (ok true)
    )
)

(define-public (declare-disaster (name (string-ascii 50)) (location (string-ascii 50)) (required-funds uint))
    (let ((current-height (- burn-block-height u1)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (begin
            (var-set proposal-counter (+ (var-get proposal-counter) u1))
            (map-set disasters (var-get proposal-counter) {
                name: name,
                location: location,
                required-funds: required-funds,
                status: "active",
                declaration-height: current-height
            })
            (var-set active-disaster-id (some (var-get proposal-counter)))
            (ok (var-get proposal-counter))
        )
    )
)
(define-public (create-fund-proposal (disaster-id uint) (amount uint) (recipient principal))
    (let ((current-height (- burn-block-height u1)))
        (asserts! (is-some (map-get? disasters disaster-id)) ERR-NO-ACTIVE-DISASTER)
        (asserts! (<= amount (var-get total-funds)) ERR-INSUFFICIENT-BALANCE)
        (begin
            (var-set proposal-counter (+ (var-get proposal-counter) u1))
            (map-set proposals (var-get proposal-counter) {
                disaster-id: disaster-id,
                amount: amount,
                recipient: recipient,
                start-height: current-height,
                end-height: (+ current-height (var-get min-proposal-duration)),
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            })
            (ok (var-get proposal-counter))
        )
    )
)
(define-public (vote (proposal-id uint) (vote-bool bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-ACTIVE-DISASTER))
        (member (unwrap! (map-get? dao-members tx-sender) ERR-NOT-AUTHORIZED))
        (vote-key {proposal-id: proposal-id, voter: tx-sender})
    )
        (asserts! (< burn-block-height (get end-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
        
        (map-set votes vote-key true)
        (map-set proposals proposal-id 
            (merge proposal {
                yes-votes: (if vote-bool (+ (get yes-votes proposal) (get voting-power member)) (get yes-votes proposal)),
                no-votes: (if vote-bool (get no-votes proposal) (+ (get no-votes proposal) (get voting-power member)))
            })
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-ACTIVE-DISASTER))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
    )
        (asserts! (>= burn-block-height (get end-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (>= total-votes (var-get quorum-threshold)) ERR-INVALID-AMOUNT)
        
        (if (> (get yes-votes proposal) (get no-votes proposal))
            (begin
                (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
                (var-set total-funds (- (var-get total-funds) (get amount proposal)))
                (map-set proposals proposal-id (merge proposal {status: "executed"}))
                (ok true)
            )
            (begin
                (map-set proposals proposal-id (merge proposal {status: "rejected"}))
                (ok true)
            )
        )
    )
)

(define-read-only (get-disaster-info (disaster-id uint))
    (map-get? disasters disaster-id)
)

(define-read-only (get-proposal-info (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-member-info (member principal))
    (map-get? dao-members member)
)
