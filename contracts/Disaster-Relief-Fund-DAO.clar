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
        (log-audit-event ACTION-CONTRIBUTION none (some amount) none)
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
            (log-audit-event ACTION-DISASTER-DECLARED (some (var-get proposal-counter)) (some required-funds) (some location))
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
            (log-audit-event ACTION-PROPOSAL-CREATED (some (var-get proposal-counter)) (some amount) none)
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
        (log-audit-event ACTION-VOTE-CAST (some proposal-id) none (some (if vote-bool "yes" "no")))
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
                (log-audit-event ACTION-PROPOSAL-EXECUTED (some proposal-id) (some (get amount proposal)) none)
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

(define-constant ERR-NOT-EMERGENCY (err u106))
(define-constant ERR-EMERGENCY-THRESHOLD-NOT-MET (err u107))
(define-constant ERR-MILESTONE-NOT-FOUND (err u108))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u109))
(define-constant ERR-PREVIOUS-MILESTONE-INCOMPLETE (err u110))

(define-data-var emergency-vote-duration uint u24)
(define-data-var emergency-threshold uint u75)
(define-data-var daily-emergency-limit uint u1000000)

(define-map emergency-proposals
    uint 
    {
        disaster-id: uint,
        amount: uint,
        recipient: principal,
        start-height: uint,
        end-height: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 10),
        justification: (string-ascii 200)
    }
)

(define-map daily-emergency-usage
    uint
    uint
)

(define-map milestone-proposals
    uint
    {
        disaster-id: uint,
        total-amount: uint,
        recipient: principal,
        total-milestones: uint,
        current-milestone: uint,
        disbursed-amount: uint,
        status: (string-ascii 10),
        creation-height: uint
    }
)

(define-map milestones
    {proposal-id: uint, milestone-index: uint}
    {
        description: (string-ascii 100),
        amount: uint,
        completed: bool
    }
)

(define-data-var milestone-proposal-counter uint u0)

(define-public (create-emergency-proposal (disaster-id uint) (amount uint) (recipient principal) (justification (string-ascii 200)))
    (let (
        (current-height (- burn-block-height u1))
        (current-day (/ current-height u144))
        (daily-used (default-to u0 (map-get? daily-emergency-usage current-day)))
    )
        (asserts! (is-some (map-get? disasters disaster-id)) ERR-NO-ACTIVE-DISASTER)
        (asserts! (<= amount (var-get total-funds)) ERR-INSUFFICIENT-BALANCE)
        (asserts! (<= (+ daily-used amount) (var-get daily-emergency-limit)) ERR-INVALID-AMOUNT)
        (begin
            (var-set proposal-counter (+ (var-get proposal-counter) u1))
            (map-set emergency-proposals (var-get proposal-counter) {
                disaster-id: disaster-id,
                amount: amount,
                recipient: recipient,
                start-height: current-height,
                end-height: (+ current-height (var-get emergency-vote-duration)),
                yes-votes: u0,
                no-votes: u0,
                status: "active",
                justification: justification
            })
            (log-audit-event ACTION-EMERGENCY-PROPOSAL (some (var-get proposal-counter)) (some amount) (some justification))
            (ok (var-get proposal-counter))
        )
    )
)

(define-public (vote-emergency (proposal-id uint) (vote-bool bool))
    (let (
        (proposal (unwrap! (map-get? emergency-proposals proposal-id) ERR-NOT-EMERGENCY))
        (member (unwrap! (map-get? dao-members tx-sender) ERR-NOT-AUTHORIZED))
        (vote-key {proposal-id: proposal-id, voter: tx-sender})
    )
        (asserts! (< burn-block-height (get end-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
        
        (map-set votes vote-key true)
        (map-set emergency-proposals proposal-id 
            (merge proposal {
                yes-votes: (if vote-bool (+ (get yes-votes proposal) (get voting-power member)) (get yes-votes proposal)),
                no-votes: (if vote-bool (get no-votes proposal) (+ (get no-votes proposal) (get voting-power member)))
            })
        )
        (ok true)
    )
)

(define-public (execute-emergency-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? emergency-proposals proposal-id) ERR-NOT-EMERGENCY))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        (approval-rate (if (> total-votes u0) (/ (* (get yes-votes proposal) u100) total-votes) u0))
        (current-day (/ burn-block-height u144))
        (daily-used (default-to u0 (map-get? daily-emergency-usage current-day)))
    )
        (asserts! (>= burn-block-height (get end-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (>= total-votes (var-get quorum-threshold)) ERR-INVALID-AMOUNT)
        (asserts! (>= approval-rate (var-get emergency-threshold)) ERR-EMERGENCY-THRESHOLD-NOT-MET)
        
        (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
        (var-set total-funds (- (var-get total-funds) (get amount proposal)))
        (map-set daily-emergency-usage current-day (+ daily-used (get amount proposal)))
        (map-set emergency-proposals proposal-id (merge proposal {status: "executed"}))
        (ok true)
    )
)

(define-read-only (get-emergency-proposal-info (proposal-id uint))
    (map-get? emergency-proposals proposal-id)
)

(define-read-only (get-daily-emergency-usage (day uint))
    (default-to u0 (map-get? daily-emergency-usage day))
)

(define-constant ERR-INVALID-DELEGATE (err u111))
(define-constant ERR-SELF-DELEGATION (err u112))

(define-data-var reputation-decay-rate uint u95)
(define-data-var participation-bonus uint u10)

(define-map member-reputation
    principal
    {
        score: uint,
        last-updated: uint,
        votes-cast: uint,
        proposals-created: uint
    }
)

(define-map delegations
    principal
    principal
)

(define-map delegate-power
    principal
    uint
)

(define-public (delegate-voting-power (delegate principal))
    (let (
        (member (unwrap! (map-get? dao-members tx-sender) ERR-NOT-AUTHORIZED))
        (delegate-member (unwrap! (map-get? dao-members delegate) ERR-INVALID-DELEGATE))
        (current-power (default-to u0 (map-get? delegate-power delegate)))
    )
        (asserts! (not (is-eq tx-sender delegate)) ERR-SELF-DELEGATION)
        
        (match (map-get? delegations tx-sender)
            old-delegate (map-set delegate-power old-delegate 
                (- (default-to u0 (map-get? delegate-power old-delegate)) (get voting-power member)))
            true
        )
        
        (map-set delegations tx-sender delegate)
        (map-set delegate-power delegate (+ current-power (get voting-power member)))
        (ok true)
    )
)

(define-public (revoke-delegation)
    (let (
        (member (unwrap! (map-get? dao-members tx-sender) ERR-NOT-AUTHORIZED))
        (current-delegate (unwrap! (map-get? delegations tx-sender) ERR-INVALID-DELEGATE))
    )
        (map-delete delegations tx-sender)
        (map-set delegate-power current-delegate 
            (- (default-to u0 (map-get? delegate-power current-delegate)) (get voting-power member)))
        (ok true)
    )
)

(define-public (vote-with-delegation (proposal-id uint) (vote-bool bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-ACTIVE-DISASTER))
        (member (unwrap! (map-get? dao-members tx-sender) ERR-NOT-AUTHORIZED))
        (delegated-power (default-to u0 (map-get? delegate-power tx-sender)))
        (total-power (+ (get voting-power member) delegated-power))
        (vote-key {proposal-id: proposal-id, voter: tx-sender})
        (reputation (default-to {score: u100, last-updated: u0, votes-cast: u0, proposals-created: u0} 
            (map-get? member-reputation tx-sender)))
    )
        (asserts! (< burn-block-height (get end-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes vote-key)) ERR-ALREADY-VOTED)
        
        (map-set votes vote-key true)
        (map-set proposals proposal-id 
            (merge proposal {
                yes-votes: (if vote-bool (+ (get yes-votes proposal) total-power) (get yes-votes proposal)),
                no-votes: (if vote-bool (get no-votes proposal) (+ (get no-votes proposal) total-power))
            })
        )
        
        (map-set member-reputation tx-sender 
            (merge reputation {
                score: (+ (get score reputation) (var-get participation-bonus)),
                last-updated: burn-block-height,
                votes-cast: (+ (get votes-cast reputation) u1)
            })
        )
        (ok true)
    )
)

(define-public (update-reputation (member principal))
    (let (
        (reputation (default-to {score: u100, last-updated: u0, votes-cast: u0, proposals-created: u0} 
            (map-get? member-reputation member)))
        (blocks-passed (- burn-block-height (get last-updated reputation)))
        (decay-periods (/ blocks-passed u1008))
        (new-score (if (> decay-periods u0) 
            (/ (* (get score reputation) (pow (var-get reputation-decay-rate) decay-periods)) (pow u100 decay-periods))
            (get score reputation)))
    )
        (map-set member-reputation member 
            (merge reputation {
                score: new-score,
                last-updated: burn-block-height
            })
        )
        (ok new-score)
    )
)

(define-read-only (get-member-reputation (member principal))
    (map-get? member-reputation member)
)

(define-read-only (get-delegation-info (member principal))
    (map-get? delegations member)
)

(define-read-only (get-delegate-power (delegate principal))
    (default-to u0 (map-get? delegate-power delegate))
)

(define-read-only (get-effective-voting-power (member principal))
    (let (
        (member-data (map-get? dao-members member))
        (delegated-power (default-to u0 (map-get? delegate-power member)))
    )
        (match member-data
            data (+ (get voting-power data) delegated-power)
            u0
        )
    )
)

(define-public (create-milestone-proposal (disaster-id uint) (total-amount uint) (recipient principal) (milestone-count uint))
    (let ((current-height (- burn-block-height u1)))
        (asserts! (is-some (map-get? disasters disaster-id)) ERR-NO-ACTIVE-DISASTER)
        (asserts! (<= total-amount (var-get total-funds)) ERR-INSUFFICIENT-BALANCE)
        (asserts! (and (> milestone-count u0) (<= milestone-count u5)) ERR-INVALID-AMOUNT)
        (begin
            (var-set milestone-proposal-counter (+ (var-get milestone-proposal-counter) u1))
            (map-set milestone-proposals (var-get milestone-proposal-counter) {
                disaster-id: disaster-id,
                total-amount: total-amount,
                recipient: recipient,
                total-milestones: milestone-count,
                current-milestone: u0,
                disbursed-amount: u0,
                status: "active",
                creation-height: current-height
            })
            (ok (var-get milestone-proposal-counter))
        )
    )
)

(define-public (add-milestone (proposal-id uint) (milestone-index uint) (description (string-ascii 100)) (amount uint))
    (let ((proposal (unwrap! (map-get? milestone-proposals proposal-id) ERR-MILESTONE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (< milestone-index (get total-milestones proposal)) ERR-MILESTONE-NOT-FOUND)
        (asserts! (is-none (map-get? milestones {proposal-id: proposal-id, milestone-index: milestone-index})) ERR-MILESTONE-ALREADY-COMPLETED)
        (map-set milestones {proposal-id: proposal-id, milestone-index: milestone-index} {
            description: description,
            amount: amount,
            completed: false
        })
        (ok true)
    )
)

(define-public (complete-milestone (proposal-id uint) (milestone-index uint))
    (let (
        (proposal (unwrap! (map-get? milestone-proposals proposal-id) ERR-MILESTONE-NOT-FOUND))
        (milestone (unwrap! (map-get? milestones {proposal-id: proposal-id, milestone-index: milestone-index}) ERR-MILESTONE-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq milestone-index (get current-milestone proposal)) ERR-PREVIOUS-MILESTONE-INCOMPLETE)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
        
        (let (
            (milestone-amount (get amount milestone))
            (new-disbursed (+ (get disbursed-amount proposal) milestone-amount))
            (next-milestone (+ milestone-index u1))
            (total-milestones (get total-milestones proposal))
        )
            (try! (as-contract (stx-transfer? milestone-amount tx-sender (get recipient proposal))))
            (var-set total-funds (- (var-get total-funds) milestone-amount))
            (map-set milestones {proposal-id: proposal-id, milestone-index: milestone-index} 
                (merge milestone {completed: true}))
            (map-set milestone-proposals proposal-id 
                (merge proposal {
                    current-milestone: next-milestone,
                    disbursed-amount: new-disbursed,
                    status: (if (is-eq next-milestone total-milestones) "completed" "active")
                })
            )
            (log-audit-event ACTION-MILESTONE-COMPLETED (some proposal-id) (some milestone-amount) none)
            (ok true)
        )
    )
)

(define-public (cancel-milestone-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? milestone-proposals proposal-id) ERR-MILESTONE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status proposal) "active") ERR-INVALID-AMOUNT)
        (map-set milestone-proposals proposal-id (merge proposal {status: "cancelled"}))
        (ok true)
    )
)

(define-read-only (get-milestone-proposal-info (proposal-id uint))
    (map-get? milestone-proposals proposal-id)
)

(define-read-only (get-milestone-progress (proposal-id uint))
    (match (map-get? milestone-proposals proposal-id)
        proposal (ok {
            total-milestones: (get total-milestones proposal),
            completed-milestones: (get current-milestone proposal),
            disbursed-amount: (get disbursed-amount proposal),
            total-amount: (get total-amount proposal),
            completion-percentage: (if (> (get total-milestones proposal) u0)
                (/ (* (get current-milestone proposal) u100) (get total-milestones proposal))
                u0)
        })
        ERR-MILESTONE-NOT-FOUND
    )
)

(define-read-only (get-milestone-info (proposal-id uint) (milestone-index uint))
    (map-get? milestones {proposal-id: proposal-id, milestone-index: milestone-index})
)

(define-constant ACTION-CONTRIBUTION "contribute")
(define-constant ACTION-DISASTER-DECLARED "disaster-declared")
(define-constant ACTION-PROPOSAL-CREATED "proposal-created")
(define-constant ACTION-VOTE-CAST "vote-cast")
(define-constant ACTION-PROPOSAL-EXECUTED "proposal-executed")
(define-constant ACTION-EMERGENCY-PROPOSAL "emergency-proposal")
(define-constant ACTION-MILESTONE-COMPLETED "milestone-completed")

(define-data-var audit-counter uint u0)

(define-map audit-trail
    uint
    {
        action: (string-ascii 20),
        actor: principal,
        target-id: (optional uint),
        amount: (optional uint),
        block-height: uint,
        additional-info: (optional (string-ascii 200))
    }
)

(define-private (log-audit-event (action (string-ascii 20)) (target-id (optional uint)) (amount (optional uint)) (info (optional (string-ascii 200))))
    (let ((audit-id (+ (var-get audit-counter) u1)))
        (var-set audit-counter audit-id)
        (map-set audit-trail audit-id {
            action: action,
            actor: tx-sender,
            target-id: target-id,
            amount: amount,
            block-height: burn-block-height,
            additional-info: info
        })
        audit-id
    )
)

(define-read-only (get-audit-event (audit-id uint))
    (map-get? audit-trail audit-id)
)

(define-read-only (get-total-audit-events)
    (var-get audit-counter)
)