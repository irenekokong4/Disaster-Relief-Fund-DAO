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

;; =================================================================================
;; COMMUNITY WELLNESS MONITORING SYSTEM
;; Independent feature for tracking post-disaster community wellness metrics
;; =================================================================================

;; Wellness error constants
(define-constant ERR-WELLNESS-NOT-FOUND (err u200))
(define-constant ERR-INVALID-METRIC-VALUE (err u201))
(define-constant ERR-WELLNESS-UNAUTHORIZED (err u202))
(define-constant ERR-THRESHOLD-ALREADY-SET (err u203))
(define-constant ERR-INVALID-THRESHOLD (err u204))

;; Wellness data variables
(define-data-var wellness-report-counter uint u0)
(define-data-var wellness-threshold-health uint u30)      ;; Critical health threshold (0-100)
(define-data-var wellness-threshold-infrastructure uint u25)  ;; Critical infrastructure threshold
(define-data-var wellness-threshold-economic uint u20)   ;; Critical economic threshold
(define-data-var wellness-threshold-social uint u35)     ;; Critical social cohesion threshold
(define-data-var wellness-monitoring-enabled bool true)

;; Community wellness metrics per disaster
(define-map community-wellness
    {disaster-id: uint, reporter: principal}
    {
        health-score: uint,           ;; 0-100 scale
        infrastructure-score: uint,   ;; 0-100 scale  
        economic-score: uint,         ;; 0-100 scale
        social-cohesion-score: uint,  ;; 0-100 scale
        report-timestamp: uint,
        verified: bool
    }
)

;; Aggregated wellness data per disaster
(define-map disaster-wellness-summary
    uint
    {
        total-reports: uint,
        avg-health: uint,
        avg-infrastructure: uint,
        avg-economic: uint,
        avg-social: uint,
        last-updated: uint,
        critical-alerts: uint
    }
)

;; Wellness alert tracking
(define-map wellness-alerts
    uint
    {
        disaster-id: uint,
        alert-type: (string-ascii 20),
        severity-level: uint,         ;; 1=low, 2=medium, 3=high, 4=critical
        metric-value: uint,
        threshold-breached: uint,
        alert-timestamp: uint,
        resolved: bool
    }
)

;; Wellness trend tracking (simplified)
(define-map wellness-trends
    {disaster-id: uint, metric-type: (string-ascii 15)}
    {
        current-value: uint,
        previous-value: uint,
        last-updated: uint,
        trend-direction: (string-ascii 10)  ;; "improving", "declining", "stable"
    }
)

;; Public function: Submit wellness report
(define-public (submit-wellness-report (disaster-id uint) (health uint) (infrastructure uint) (economic uint) (social uint))
    (let (
        (current-height burn-block-height)
        (report-key {disaster-id: disaster-id, reporter: tx-sender})
        (existing-summary (default-to 
            {total-reports: u0, avg-health: u0, avg-infrastructure: u0, avg-economic: u0, avg-social: u0, last-updated: u0, critical-alerts: u0}
            (map-get? disaster-wellness-summary disaster-id)
        ))
    )
        ;; Validate inputs
        (asserts! (var-get wellness-monitoring-enabled) ERR-WELLNESS-UNAUTHORIZED)
        (asserts! (is-some (map-get? disasters disaster-id)) ERR-NO-ACTIVE-DISASTER)
        (asserts! (is-some (map-get? dao-members tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (and (<= health u100) (<= infrastructure u100) (<= economic u100) (<= social u100)) ERR-INVALID-METRIC-VALUE)
        
        ;; Store individual wellness report
        (map-set community-wellness report-key {
            health-score: health,
            infrastructure-score: infrastructure,
            economic-score: economic,
            social-cohesion-score: social,
            report-timestamp: current-height,
            verified: false
        })
        
        ;; Update aggregated summary
        (let (
            (new-total (+ (get total-reports existing-summary) u1))
            (new-avg-health (/ (+ (* (get avg-health existing-summary) (get total-reports existing-summary)) health) new-total))
            (new-avg-infra (/ (+ (* (get avg-infrastructure existing-summary) (get total-reports existing-summary)) infrastructure) new-total))
            (new-avg-econ (/ (+ (* (get avg-economic existing-summary) (get total-reports existing-summary)) economic) new-total))
            (new-avg-social (/ (+ (* (get avg-social existing-summary) (get total-reports existing-summary)) social) new-total))
        )
            (map-set disaster-wellness-summary disaster-id {
                total-reports: new-total,
                avg-health: new-avg-health,
                avg-infrastructure: new-avg-infra,
                avg-economic: new-avg-econ,
                avg-social: new-avg-social,
                last-updated: current-height,
                critical-alerts: (get critical-alerts existing-summary)
            })
            
            ;; Check for critical thresholds and generate alerts
            (unwrap-panic (check-wellness-thresholds disaster-id new-avg-health new-avg-infra new-avg-econ new-avg-social))
            
            ;; Update wellness trends
            (unwrap-panic (update-wellness-trends disaster-id "health" health current-height))
            (unwrap-panic (update-wellness-trends disaster-id "infrastructure" infrastructure current-height))
            (unwrap-panic (update-wellness-trends disaster-id "economic" economic current-height))
            (unwrap-panic (update-wellness-trends disaster-id "social" social current-height))
            
            ;; Log audit event
            (log-audit-event "wellness-report" (some disaster-id) none (some "Community wellness metrics updated"))
            (ok true)
        )
    )
)

;; Public function: Verify wellness report (DAO owner only)
(define-public (verify-wellness-report (disaster-id uint) (reporter principal))
    (let ((report-key {disaster-id: disaster-id, reporter: reporter}))
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (match (map-get? community-wellness report-key)
            report (begin
                (map-set community-wellness report-key (merge report {verified: true}))
                (log-audit-event "wellness-verified" (some disaster-id) none (some "Wellness report verified"))
                (ok true)
            )
            ERR-WELLNESS-NOT-FOUND
        )
    )
)

;; Public function: Set wellness thresholds (DAO owner only)
(define-public (set-wellness-thresholds (health-threshold uint) (infra-threshold uint) (econ-threshold uint) (social-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (and (<= health-threshold u100) (<= infra-threshold u100) 
                      (<= econ-threshold u100) (<= social-threshold u100)) ERR-INVALID-THRESHOLD)
        (var-set wellness-threshold-health health-threshold)
        (var-set wellness-threshold-infrastructure infra-threshold)
        (var-set wellness-threshold-economic econ-threshold)
        (var-set wellness-threshold-social social-threshold)
        (log-audit-event "thresholds-updated" none none (some "Wellness thresholds updated"))
        (ok true)
    )
)

;; Public function: Toggle wellness monitoring
(define-public (toggle-wellness-monitoring (enabled bool))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-NOT-AUTHORIZED)
        (var-set wellness-monitoring-enabled enabled)
        (log-audit-event "monitoring-toggled" none none (some (if enabled "Monitoring enabled" "Monitoring disabled")))
        (ok enabled)
    )
)

;; Private function: Check wellness thresholds and create alerts
(define-private (check-wellness-thresholds (disaster-id uint) (health uint) (infrastructure uint) (economic uint) (social uint))
    (begin
        ;; Check health threshold
        (if (< health (var-get wellness-threshold-health))
            (unwrap-panic (create-wellness-alert disaster-id "health" u4 health (var-get wellness-threshold-health)))
            u0
        )
        ;; Check infrastructure threshold  
        (if (< infrastructure (var-get wellness-threshold-infrastructure))
            (unwrap-panic (create-wellness-alert disaster-id "infrastructure" u3 infrastructure (var-get wellness-threshold-infrastructure)))
            u0
        )
        ;; Check economic threshold
        (if (< economic (var-get wellness-threshold-economic))
            (unwrap-panic (create-wellness-alert disaster-id "economic" u3 economic (var-get wellness-threshold-economic)))
            u0
        )
        ;; Check social threshold
        (if (< social (var-get wellness-threshold-social))
            (unwrap-panic (create-wellness-alert disaster-id "social" u2 social (var-get wellness-threshold-social)))
            u0
        )
        (ok true)
    )
)

;; Private function: Create wellness alert
(define-private (create-wellness-alert (disaster-id uint) (alert-type (string-ascii 20)) (severity uint) (metric-value uint) (threshold uint))
    (begin
        (var-set wellness-report-counter (+ (var-get wellness-report-counter) u1))
        (map-set wellness-alerts (var-get wellness-report-counter) {
            disaster-id: disaster-id,
            alert-type: alert-type,
            severity-level: severity,
            metric-value: metric-value,
            threshold-breached: threshold,
            alert-timestamp: burn-block-height,
            resolved: false
        })
        
        ;; Update critical alerts counter in summary
        (match (map-get? disaster-wellness-summary disaster-id)
            summary (map-set disaster-wellness-summary disaster-id 
                (merge summary {critical-alerts: (+ (get critical-alerts summary) u1)}))
            true
        )
        (ok (var-get wellness-report-counter))
    )
)

;; Private function: Update wellness trends
(define-private (update-wellness-trends (disaster-id uint) (metric-type (string-ascii 15)) (new-value uint) (timestamp uint))
    (let (
        (trend-key {disaster-id: disaster-id, metric-type: metric-type})
        (existing-trend (default-to
            {current-value: u0, previous-value: u0, last-updated: u0, trend-direction: "stable"}
            (map-get? wellness-trends trend-key)
        ))
        (current-val (get current-value existing-trend))
        (trend-dir (if (> new-value current-val) "improving"
                   (if (< new-value current-val) "declining" "stable")))
    )
        (map-set wellness-trends trend-key {
            current-value: new-value,
            previous-value: current-val,
            last-updated: timestamp,
            trend-direction: trend-dir
        })
        (ok true)
    )
)


;; Read-only function: Get individual wellness report
(define-read-only (get-wellness-report (disaster-id uint) (reporter principal))
    (map-get? community-wellness {disaster-id: disaster-id, reporter: reporter})
)

;; Read-only function: Get disaster wellness summary
(define-read-only (get-disaster-wellness-summary (disaster-id uint))
    (map-get? disaster-wellness-summary disaster-id)
)

;; Read-only function: Get wellness alert
(define-read-only (get-wellness-alert (alert-id uint))
    (map-get? wellness-alerts alert-id)
)

;; Read-only function: Get wellness trends
(define-read-only (get-wellness-trends (disaster-id uint) (metric-type (string-ascii 15)))
    (map-get? wellness-trends {disaster-id: disaster-id, metric-type: metric-type})
)

;; Read-only function: Get current wellness thresholds
(define-read-only (get-wellness-thresholds)
    {
        health: (var-get wellness-threshold-health),
        infrastructure: (var-get wellness-threshold-infrastructure),
        economic: (var-get wellness-threshold-economic),
        social: (var-get wellness-threshold-social),
        monitoring-enabled: (var-get wellness-monitoring-enabled)
    }
)

;; Read-only function: Calculate wellness risk level for disaster
(define-read-only (calculate-wellness-risk-level (disaster-id uint))
    (match (map-get? disaster-wellness-summary disaster-id)
        summary (let (
            (health-risk (if (< (get avg-health summary) (var-get wellness-threshold-health)) u4 u0))
            (infra-risk (if (< (get avg-infrastructure summary) (var-get wellness-threshold-infrastructure)) u3 u0))
            (econ-risk (if (< (get avg-economic summary) (var-get wellness-threshold-economic)) u3 u0))
            (social-risk (if (< (get avg-social summary) (var-get wellness-threshold-social)) u2 u0))
            (total-risk (+ health-risk infra-risk econ-risk social-risk))
        )
            (ok {
                risk-level: (if (>= total-risk u10) "critical"
                          (if (>= total-risk u7) "high"
                          (if (>= total-risk u4) "medium" "low"))),
                risk-score: total-risk,
                contributing-factors: {
                    health-critical: (< (get avg-health summary) (var-get wellness-threshold-health)),
                    infrastructure-critical: (< (get avg-infrastructure summary) (var-get wellness-threshold-infrastructure)),
                    economic-critical: (< (get avg-economic summary) (var-get wellness-threshold-economic)),
                    social-critical: (< (get avg-social summary) (var-get wellness-threshold-social))
                }
            })
        )
        ERR-WELLNESS-NOT-FOUND
    )
)

;; Read-only function: Get wellness monitoring status
(define-read-only (get-wellness-monitoring-status)
    {
        enabled: (var-get wellness-monitoring-enabled),
        total-reports: (var-get wellness-report-counter),
        thresholds: {
            health: (var-get wellness-threshold-health),
            infrastructure: (var-get wellness-threshold-infrastructure),
            economic: (var-get wellness-threshold-economic),
            social: (var-get wellness-threshold-social)
        }
    }
)

(define-constant ERR-ACCOUNTABILITY-NOT-FOUND (err u300))
(define-constant ERR-INVALID-ROI-VALUE (err u301))
(define-constant ERR-ACCOUNTABILITY-UNAUTHORIZED (err u302))
(define-constant ERR-SPENDING-EXCEEDS-ALLOCATION (err u303))
(define-constant ERR-INVALID-STATUS-CHANGE (err u304))
(define-constant ERR-AUDIT-REQUEST-DUPLICATE (err u305))

(define-data-var accountability-counter uint u0)
(define-data-var audit-request-counter uint u0)
(define-data-var accountability-enabled bool true)

(define-map resource-allocations
    uint
    {
        disaster-id: uint,
        allocated-amount: uint,
        allocated-by: principal,
        allocation-timestamp: uint,
        purpose: (string-ascii 100),
        status: (string-ascii 15)
    }
)

(define-map spending-records
    uint
    {
        allocation-id: uint,
        spent-amount: uint,
        recipient: principal,
        spending-timestamp: uint,
        description: (string-ascii 100),
        verified-by: (optional principal)
    }
)

(define-map roi-metrics
    uint
    {
        allocation-id: uint,
        expected-impact: uint,
        actual-impact: uint,
        impact-unit: (string-ascii 30),
        roi-percentage: uint,
        measurement-timestamp: uint,
        validator: principal
    }
)

(define-map allocation-summary
    uint
    {
        total-allocated: uint,
        total-spent: uint,
        total-returned: uint,
        spending-records-count: uint,
        roi-records-count: uint,
        last-updated: uint
    }
)

(define-map audit-requests
    uint
    {
        allocation-id: uint,
        requestor: principal,
        request-timestamp: uint,
        reason: (string-ascii 100),
        status: (string-ascii 15),
        audit-result: (optional (string-ascii 100))
    }
)

(define-public (create-resource-allocation (disaster-id uint) (amount uint) (purpose (string-ascii 100)))
    (let (
        (current-height burn-block-height)
        (allocation-id (+ (var-get accountability-counter) u1))
    )
        (asserts! (var-get accountability-enabled) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (is-some (map-get? disasters disaster-id)) ERR-NO-ACTIVE-DISASTER)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= amount (var-get total-funds)) ERR-INSUFFICIENT-BALANCE)
        
        (var-set accountability-counter allocation-id)
        (map-set resource-allocations allocation-id {
            disaster-id: disaster-id,
            allocated-amount: amount,
            allocated-by: tx-sender,
            allocation-timestamp: current-height,
            purpose: purpose,
            status: "active"
        })
        (map-set allocation-summary allocation-id {
            total-allocated: amount,
            total-spent: u0,
            total-returned: u0,
            spending-records-count: u0,
            roi-records-count: u0,
            last-updated: current-height
        })
        (log-audit-event "allocation-created" (some allocation-id) (some amount) (some purpose))
        (ok allocation-id)
    )
)

(define-public (record-spending (allocation-id uint) (spent-amount uint) (recipient principal) (description (string-ascii 100)))
    (let (
        (current-height burn-block-height)
        (allocation (unwrap! (map-get? resource-allocations allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
        (summary (unwrap! (map-get? allocation-summary allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
        (new-total-spent (+ (get total-spent summary) spent-amount))
        (spending-id (+ (var-get audit-request-counter) u1))
    )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (> spent-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= new-total-spent (get allocated-amount allocation)) ERR-SPENDING-EXCEEDS-ALLOCATION)
        (asserts! (is-eq (get status allocation) "active") ERR-INVALID-STATUS-CHANGE)
        
        (map-set spending-records spending-id {
            allocation-id: allocation-id,
            spent-amount: spent-amount,
            recipient: recipient,
            spending-timestamp: current-height,
            description: description,
            verified-by: none
        })
        
        (map-set allocation-summary allocation-id 
            (merge summary {
                total-spent: new-total-spent,
                spending-records-count: (+ (get spending-records-count summary) u1),
                last-updated: current-height
            })
        )
        (log-audit-event "spending-recorded" (some allocation-id) (some spent-amount) (some description))
        (ok spending-id)
    )
)

(define-public (verify-spending (spending-id uint))
    (let (
        (spending (unwrap! (map-get? spending-records spending-id) ERR-ACCOUNTABILITY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (is-none (get verified-by spending)) ERR-INVALID-STATUS-CHANGE)
        
        (map-set spending-records spending-id 
            (merge spending {verified-by: (some tx-sender)})
        )
        (log-audit-event "spending-verified" (some spending-id) none (some "Spending verified by DAO owner"))
        (ok true)
    )
)

(define-public (record-roi-metric (allocation-id uint) (expected-impact uint) (actual-impact uint) (impact-unit (string-ascii 30)))
    (let (
        (current-height burn-block-height)
        (allocation (unwrap! (map-get? resource-allocations allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
        (summary (unwrap! (map-get? allocation-summary allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
        (allocated-amount (get allocated-amount allocation))
        (spent-amount (get total-spent summary))
        (roi-calc (if (> spent-amount u0) (/ (* actual-impact u100) spent-amount) u0))
        (roi-id (var-get accountability-counter))
    )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (> actual-impact u0) ERR-INVALID-ROI-VALUE)
        
        (map-set roi-metrics roi-id {
            allocation-id: allocation-id,
            expected-impact: expected-impact,
            actual-impact: actual-impact,
            impact-unit: impact-unit,
            roi-percentage: roi-calc,
            measurement-timestamp: current-height,
            validator: tx-sender
        })
        
        (map-set allocation-summary allocation-id 
            (merge summary {
                roi-records-count: (+ (get roi-records-count summary) u1),
                last-updated: current-height
            })
        )
        (log-audit-event "roi-recorded" (some allocation-id) (some actual-impact) (some impact-unit))
        (ok roi-id)
    )
)

(define-public (request-audit (allocation-id uint) (reason (string-ascii 100)))
    (let (
        (current-height burn-block-height)
        (allocation (unwrap! (map-get? resource-allocations allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
        (request-id (+ (var-get audit-request-counter) u1))
    )
        (asserts! (var-get accountability-enabled) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (is-some (map-get? dao-members tx-sender)) ERR-NOT-AUTHORIZED)
        
        (var-set audit-request-counter request-id)
        (map-set audit-requests request-id {
            allocation-id: allocation-id,
            requestor: tx-sender,
            request-timestamp: current-height,
            reason: reason,
            status: "pending",
            audit-result: none
        })
        (log-audit-event "audit-requested" (some allocation-id) none (some reason))
        (ok request-id)
    )
)

(define-public (resolve-audit-request (request-id uint) (result (string-ascii 100)))
    (let (
        (request (unwrap! (map-get? audit-requests request-id) ERR-ACCOUNTABILITY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (is-eq (get status request) "pending") ERR-INVALID-STATUS-CHANGE)
        
        (map-set audit-requests request-id 
            (merge request {
                status: "resolved",
                audit-result: (some result)
            })
        )
        (log-audit-event "audit-resolved" (some request-id) none (some result))
        (ok true)
    )
)

(define-public (close-allocation (allocation-id uint))
    (let (
        (allocation (unwrap! (map-get? resource-allocations allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
        (summary (unwrap! (map-get? allocation-summary allocation-id) ERR-ACCOUNTABILITY-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (asserts! (is-eq (get status allocation) "active") ERR-INVALID-STATUS-CHANGE)
        
        (let (
            (unspent-amount (- (get allocated-amount allocation) (get total-spent summary)))
        )
            (map-set resource-allocations allocation-id 
                (merge allocation {status: "closed"})
            )
            (map-set allocation-summary allocation-id 
                (merge summary {total-returned: unspent-amount})
            )
            (var-set total-funds (+ (var-get total-funds) unspent-amount))
            (log-audit-event "allocation-closed" (some allocation-id) (some unspent-amount) (some "Allocation closed, funds returned"))
            (ok true)
        )
    )
)

(define-public (toggle-accountability-monitoring (enabled bool))
    (begin
        (asserts! (is-eq tx-sender (var-get dao-owner)) ERR-ACCOUNTABILITY-UNAUTHORIZED)
        (var-set accountability-enabled enabled)
        (log-audit-event "acctbl-toggled" none none (some (if enabled "Enabled" "Disabled")))
        (ok enabled)
    )
)

(define-read-only (get-resource-allocation (allocation-id uint))
    (map-get? resource-allocations allocation-id)
)

(define-read-only (get-spending-record (spending-id uint))
    (map-get? spending-records spending-id)
)

(define-read-only (get-roi-metric (roi-id uint))
    (map-get? roi-metrics roi-id)
)

(define-read-only (get-allocation-summary (allocation-id uint))
    (map-get? allocation-summary allocation-id)
)

(define-read-only (get-audit-request (request-id uint))
    (map-get? audit-requests request-id)
)

(define-read-only (get-accountability-status)
    {
        enabled: (var-get accountability-enabled),
        total-allocations: (var-get accountability-counter),
        total-audit-requests: (var-get audit-request-counter)
    }
)

(define-read-only (calculate-allocation-efficiency (allocation-id uint))
    (match (map-get? resource-allocations allocation-id)
        allocation (match (map-get? allocation-summary allocation-id)
            summary (let (
                (allocated (get allocated-amount allocation))
                (spent (get total-spent summary))
                (efficiency (if (> allocated u0) (/ (* spent u100) allocated) u0))
            )
                (ok {
                    allocated-amount: allocated,
                    spent-amount: spent,
                    efficiency-percentage: efficiency,
                    unspent-amount: (- allocated spent),
                    spending-count: (get spending-records-count summary),
                    roi-count: (get roi-records-count summary)
                })
            )
            ERR-ACCOUNTABILITY-NOT-FOUND
        )
        ERR-ACCOUNTABILITY-NOT-FOUND
    )
)
