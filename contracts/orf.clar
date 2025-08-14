(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-MIN-THRESHOLD (err u105))
(define-constant ERR-MILESTONE-NOT-FOUND (err u111))
(define-constant ERR-MILESTONE-ALREADY-APPROVED (err u112))
(define-constant ERR-MILESTONE-ALREADY-RELEASED (err u113))
(define-constant ERR-INVALID-MILESTONE-ORDER (err u114))
(define-constant ERR-PREVIOUS-MILESTONE-NOT-COMPLETE (err u115))
(define-constant ERR-MILESTONE-FUNDS-DEPLETED (err u116))
(define-constant ERR-INVALID-MILESTONE-COUNT (err u117))
(define-constant ERR-MILESTONE-VOTING-ACTIVE (err u118))
(define-constant ERR-INSUFFICIENT-MILESTONE_VOTES (err u119))
(define-constant ERR-MILESTONE-EXPIRED (err u120))

(define-constant MAX-MILESTONES u10)
(define-constant MILESTONE-VOTING-PERIOD u288)
(define-constant MILESTONE-APPROVAL-THRESHOLD u51)

(define-map milestone-proposals
    { proposal-id: uint, milestone-id: uint }
    {
        description: (string-ascii 100),
        amount: uint,
        due-date: uint,
        submitted: bool,
        approved: bool,
        funds-released: bool,
        submission-height: uint,
        voting-end-height: uint,
        approval-votes: uint,
        rejection-votes: uint
    }
)

(define-map milestone-votes
    { proposal-id: uint, milestone-id: uint, voter: principal }
    { amount: uint, support: bool }
)

(define-map milestone-submissions
    { proposal-id: uint, milestone-id: uint }
    {
        deliverable-hash: (string-ascii 64),
        submission-notes: (string-ascii 200),
        submitted-at: uint
    }
)

(define-map milestone-proposal-info
    uint
    {
        total-milestones: uint,
        completed-milestones: uint,
        total-milestone-funds: uint,
        released-funds: uint
    }
)

(define-constant PROPOSAL-DURATION u1440)
(define-constant MIN-PROPOSAL-AMOUNT u100000000)
(define-constant VOTING_TOKEN 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.token-xyz)

(define-data-var dao-treasury uint u0)

(define-map proposals 
    uint 
    {
        id: uint,
        creator: principal,
        title: (string-ascii 50),
        amount: uint,
        recipient: principal,
        votes-for: uint,
        votes-against: uint,
        end-stacks-block-height: uint,
        executed: bool
    }
)

(define-map votes 
    { proposal-id: uint, voter: principal } 
    { amount: uint, support: bool }
)

(define-data-var proposal-count uint u0)

(define-public (initialize-dao)
    (begin
        ;; (try! (contract-call? VOTING_TOKEN transfer u1000000000000 tx-sender (as-contract tx-sender) none))
        (ok true)
    )
)

(define-public (submit-proposal (title (string-ascii 50)) (amount uint) (recipient principal))
    (let
        (
            (new-id (+ (var-get proposal-count) u1))
            (end-height (+ stacks-block-height PROPOSAL-DURATION))
        )
        (asserts! (>= amount MIN-PROPOSAL-AMOUNT) ERR-MIN-THRESHOLD)
        (map-set proposals new-id
            {
                id: new-id,
                creator: tx-sender,
                title: title,
                amount: amount,
                recipient: recipient,
                votes-for: u0,
                votes-against: u0,
                end-stacks-block-height: end-height,
                executed: false
            }
        )
        (var-set proposal-count new-id)
        (ok new-id)
    )
)

(define-public (votee (proposal-id uint) (amount uint) (support bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (voter-key { proposal-id: proposal-id, voter: tx-sender })
        )
        (asserts! (< stacks-block-height (get end-stacks-block-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes voter-key)) ERR-ALREADY-VOTED)
        ;; (try! (contract-call? VOTING_TOKEN transfer amount tx-sender (as-contract tx-sender) none))
        (map-set votes voter-key { amount: amount, support: support })
        (map-set proposals proposal-id
            (merge proposal
                {
                    votes-for: (if support (+ (get votes-for proposal) amount) (get votes-for proposal)),
                    votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) amount))
                }
            )
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        )
        (asserts! (>= stacks-block-height (get end-stacks-block-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (not (get executed proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-NOT-AUTHORIZED)
        (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal))))
        (map-set proposals proposal-id
            (merge proposal { executed: true })
        )
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (ok (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (ok (map-get? votes { proposal-id: proposal-id, voter: voter }))
)



(define-constant ERR-NOT-CREATOR (err u106))
(define-constant ERR-VOTES-EXIST (err u107))

(define-public (withdraw-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        )
        (asserts! (< stacks-block-height (get end-stacks-block-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-CREATOR)
        (asserts! (and (is-eq (get votes-for proposal) u0) (is-eq (get votes-against proposal) u0)) ERR-VOTES-EXIST)
        (asserts! (not (get executed proposal)) ERR-NOT-AUTHORIZED)
        
        (map-delete proposals proposal-id)
        (ok true)
    )
)


(define-constant ERR-INSUFFICIENT-FUNDS (err u108))

(define-public (deposit-funds (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set dao-treasury (+ (var-get dao-treasury) amount))
        (ok true)
    )
)

(define-read-only (get-treasury-balance)
    (ok (var-get dao-treasury))
)

(define-read-only (get-dao-owner)
    (ok tx-sender)
)

(define-public (withdraw-excess-funds (amount uint))
    (let
        (
            (current-balance (var-get dao-treasury))
        )
        (asserts! (>= current-balance amount) ERR-INSUFFICIENT-FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender (unwrap! (get-dao-owner) ERR-NOT-AUTHORIZED))))
        (var-set dao-treasury (- current-balance amount))
        (ok true)
    )
)



(define-map delegations
    principal
    principal
)

(define-constant ERR-CANNOT-DELEGATE-TO-SELF (err u109))
(define-constant ERR-NO-DELEGATION (err u110))

(define-public (set-delegate (new-delegate principal))
    (begin
        (asserts! (not (is-eq tx-sender new-delegate)) ERR-CANNOT-DELEGATE-TO-SELF)
        (map-set delegations tx-sender new-delegate)
        (ok true)
    )
)

(define-public (undelegate)
    (begin
        (map-delete delegations tx-sender)
        (ok true)
    )
)

(define-read-only (get-delegate (address principal))
    (ok (map-get? delegations address))
)

(define-read-only (is-delegated (address principal))
    (ok (is-some (map-get? delegations address)))
)


(define-public (vote-new (proposal-id uint) (amount uint) (support bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (voter-key { proposal-id: proposal-id, voter: tx-sender })
            (delegate (map-get? delegations tx-sender))
        )
        (asserts! (< stacks-block-height (get end-stacks-block-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes voter-key)) ERR-ALREADY-VOTED)
        (asserts! (is-none delegate) ERR-NOT-AUTHORIZED)
        (map-set votes voter-key { amount: amount, support: support })
        (map-set proposals proposal-id
            (merge proposal
                {
                    votes-for: (if support (+ (get votes-for proposal) amount) (get votes-for proposal)),
                    votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) amount))
                }
            )
        )
        (ok true)
    )
)

(define-constant MAX-MULTIPLIER u4)
(define-constant BLOCKS-PER-MULTIPLIER u144)

(define-map token-locks
    principal
    {
        amount: uint,
        lock-height: uint
    }
)

(define-public (lock-tokens (amount uint))
    (let
        (
            (current-lock (default-to { amount: u0, lock-height: stacks-block-height } (map-get? token-locks tx-sender)))
        )
        (map-set token-locks tx-sender
            {
                amount: (+ amount (get amount current-lock)),
                lock-height: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-read-only (get-voting-power (user principal))
    (let
        (
            (lock-info (default-to { amount: u0, lock-height: stacks-block-height } (map-get? token-locks user)))
            (blocks-locked (- stacks-block-height (get lock-height lock-info)))
            (multiplier (min-uint MAX-MULTIPLIER (+ u1 (/ blocks-locked BLOCKS-PER-MULTIPLIER))))
        )
        (ok (* (get amount lock-info) multiplier))
    )
)

(define-read-only (min-uint (a uint) (b uint))
    (if (<= a b) a b)
)

(define-public (unlock-tokens)
    (let
        (
            (lock-info (unwrap! (map-get? token-locks tx-sender) ERR-NOT-AUTHORIZED))
        )
        (map-delete token-locks tx-sender)
        (ok true)
    )
)

(define-public (vote (proposal-id uint) (amount uint) (support bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (voter-key { proposal-id: proposal-id, voter: tx-sender })
            (voting-power (unwrap! (get-voting-power tx-sender) ERR-NOT-AUTHORIZED))
        )
        (asserts! (< stacks-block-height (get end-stacks-block-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes voter-key)) ERR-ALREADY-VOTED)
        (asserts! (<= amount voting-power) ERR-INVALID-AMOUNT)
        (map-set votes voter-key { amount: amount, support: support })
        (map-set proposals proposal-id
            (merge proposal
                {
                    votes-for: (if support (+ (get votes-for proposal) amount) (get votes-for proposal)),
                    votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) amount))
                }
            )
        )
        (ok true)
    )
)


(define-public (submit-milestone-proposal 
    (title (string-ascii 50)) 
    (recipient principal)
    (milestone-descriptions (list 10 (string-ascii 100)))
    (milestone-amounts (list 10 uint))
    (milestone-due-dates (list 10 uint))
)
    (let
        (
            (new-id (+ (var-get proposal-count) u1))
            (end-height (+ stacks-block-height PROPOSAL-DURATION))
            (total-amount (fold + milestone-amounts u0))
            (milestone-count (len milestone-descriptions))
        )
        (asserts! (>= total-amount MIN-PROPOSAL-AMOUNT) ERR-MIN-THRESHOLD)
        (asserts! (and (> milestone-count u0) (<= milestone-count MAX-MILESTONES)) ERR-INVALID-MILESTONE-COUNT)
        (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR-INVALID-MILESTONE-COUNT)
        (asserts! (is-eq (len milestone-amounts) (len milestone-due-dates)) ERR-INVALID-MILESTONE-COUNT)
        
        (map-set proposals new-id
            {
                id: new-id,
                creator: tx-sender,
                title: title,
                amount: total-amount,
                recipient: recipient,
                votes-for: u0,
                votes-against: u0,
                end-stacks-block-height: end-height,
                executed: false
            }
        )
        
        (map-set milestone-proposal-info new-id
            {
                total-milestones: milestone-count,
                completed-milestones: u0,
                total-milestone-funds: total-amount,
                released-funds: u0
            }
        )
        
        (unwrap-panic (process-milestone-creation new-id milestone-descriptions milestone-amounts milestone-due-dates))
        (var-set proposal-count new-id)
        (ok new-id)
    )
)

(define-private (process-milestone-creation
    (proposal-id uint)
    (descriptions (list 10 (string-ascii 100)))
    (amounts (list 10 uint))
    (due-dates (list 10 uint))
)
    (begin
        (if (> (len descriptions) u0)
            (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: u1 }
                {
                    description: (default-to "" (element-at descriptions u0)),
                    amount: (default-to u0 (element-at amounts u0)),
                    due-date: (default-to u0 (element-at due-dates u0)),
                    submitted: false,
                    approved: false,
                    funds-released: false,
                    submission-height: u0,
                    voting-end-height: u0,
                    approval-votes: u0,
                    rejection-votes: u0
                }
            )
            true
        )
        (if (> (len descriptions) u1)
            (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: u2 }
                {
                    description: (default-to "" (element-at descriptions u1)),
                    amount: (default-to u0 (element-at amounts u1)),
                    due-date: (default-to u0 (element-at due-dates u1)),
                    submitted: false,
                    approved: false,
                    funds-released: false,
                    submission-height: u0,
                    voting-end-height: u0,
                    approval-votes: u0,
                    rejection-votes: u0
                }
            )
            true
        )
        (if (> (len descriptions) u2)
            (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: u3 }
                {
                    description: (default-to "" (element-at descriptions u2)),
                    amount: (default-to u0 (element-at amounts u2)),
                    due-date: (default-to u0 (element-at due-dates u2)),
                    submitted: false,
                    approved: false,
                    funds-released: false,
                    submission-height: u0,
                    voting-end-height: u0,
                    approval-votes: u0,
                    rejection-votes: u0
                }
            )
            true
        )
        (ok true)
    )
)



(define-public (submit-milestone-deliverable 
    (proposal-id uint)
    (milestone-id uint)
    (deliverable-hash (string-ascii 64))
    (submission-notes (string-ascii 200))
)
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (milestone (unwrap! (map-get? milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
            (proposal-info (unwrap! (map-get? milestone-proposal-info proposal-id) ERR-PROPOSAL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get recipient proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (get executed proposal) ERR-NOT-AUTHORIZED)
        (asserts! (not (get submitted milestone)) ERR-MILESTONE-ALREADY-APPROVED)
        (asserts! (< stacks-block-height (get due-date milestone)) ERR-MILESTONE-EXPIRED)
        
        (if (> milestone-id u1)
            (let
                (
                    (prev-milestone (unwrap! (map-get? milestone-proposals { proposal-id: proposal-id, milestone-id: (- milestone-id u1) }) ERR-MILESTONE-NOT-FOUND))
                )
                (asserts! (get funds-released prev-milestone) ERR-PREVIOUS-MILESTONE-NOT-COMPLETE)
                true
            )
            true
        )
        
        (map-set milestone-submissions { proposal-id: proposal-id, milestone-id: milestone-id }
            {
                deliverable-hash: deliverable-hash,
                submission-notes: submission-notes,
                submitted-at: stacks-block-height
            }
        )
        
        (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }
            (merge milestone
                {
                    submitted: true,
                    submission-height: stacks-block-height,
                    voting-end-height: (+ stacks-block-height MILESTONE-VOTING-PERIOD)
                }
            )
        )
        (ok true)
    )
)

(define-public (vote-milestone 
    (proposal-id uint)
    (milestone-id uint)
    (amount uint)
    (support bool)
)
    (let
        (
            (milestone (unwrap! (map-get? milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
            (voter-key { proposal-id: proposal-id, milestone-id: milestone-id, voter: tx-sender })
            (voting-power (unwrap! (get-voting-power tx-sender) ERR-NOT-AUTHORIZED))
        )
        (asserts! (get submitted milestone) ERR-MILESTONE-NOT-FOUND)
        (asserts! (< stacks-block-height (get voting-end-height milestone)) ERR-MILESTONE-EXPIRED)
        (asserts! (is-none (map-get? milestone-votes voter-key)) ERR-ALREADY-VOTED)
        (asserts! (<= amount voting-power) ERR-INVALID-AMOUNT)
        
        (map-set milestone-votes voter-key { amount: amount, support: support })
        (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }
            (merge milestone
                {
                    approval-votes: (if support (+ (get approval-votes milestone) amount) (get approval-votes milestone)),
                    rejection-votes: (if support (get rejection-votes milestone) (+ (get rejection-votes milestone) amount))
                }
            )
        )
        (ok true)
    )
)

(define-public (finalize-milestone 
    (proposal-id uint)
    (milestone-id uint)
)
    (let
        (
            (milestone (unwrap! (map-get? milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }) ERR-MILESTONE-NOT-FOUND))
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (proposal-info (unwrap! (map-get? milestone-proposal-info proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (total-votes (+ (get approval-votes milestone) (get rejection-votes milestone)))
            (approval-percentage (if (> total-votes u0) (/ (* (get approval-votes milestone) u100) total-votes) u0))
        )
        (asserts! (get submitted milestone) ERR-MILESTONE-NOT-FOUND)
        (asserts! (>= stacks-block-height (get voting-end-height milestone)) ERR-MILESTONE-VOTING-ACTIVE)
        (asserts! (not (get approved milestone)) ERR-MILESTONE-ALREADY-APPROVED)
        (asserts! (not (get funds-released milestone)) ERR-MILESTONE-ALREADY-RELEASED)
        
        (if (>= approval-percentage MILESTONE-APPROVAL-THRESHOLD)
            (begin
                (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }
                    (merge milestone { approved: true, funds-released: true })
                )
                (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get recipient proposal))))
                (map-set milestone-proposal-info proposal-id
                    (merge proposal-info
                        {
                            completed-milestones: (+ (get completed-milestones proposal-info) u1),
                            released-funds: (+ (get released-funds proposal-info) (get amount milestone))
                        }
                    )
                )
                (ok true)
            )
            (begin
                (map-set milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }
                    (merge milestone { approved: false })
                )
                (ok false)
            )
        )
    )
)

(define-read-only (get-milestone 
    (proposal-id uint)
    (milestone-id uint)
)
    (ok (map-get? milestone-proposals { proposal-id: proposal-id, milestone-id: milestone-id }))
)

(define-read-only (get-milestone-submission 
    (proposal-id uint)
    (milestone-id uint)
)
    (ok (map-get? milestone-submissions { proposal-id: proposal-id, milestone-id: milestone-id }))
)

(define-read-only (get-milestone-proposal-info (proposal-id uint))
    (ok (map-get? milestone-proposal-info proposal-id))
)

(define-read-only (get-milestone-vote 
    (proposal-id uint)
    (milestone-id uint)
    (voter principal)
)
    (ok (map-get? milestone-votes { proposal-id: proposal-id, milestone-id: milestone-id, voter: voter }))
)

(define-read-only (calculate-milestone-progress (proposal-id uint))
    (let
        (
            (info (unwrap! (map-get? milestone-proposal-info proposal-id) ERR-PROPOSAL-NOT-FOUND))
        )
        (ok 
            {
                completion-rate: (if (> (get total-milestones info) u0) 
                    (/ (* (get completed-milestones info) u100) (get total-milestones info)) 
                    u0),
                funds-utilization: (if (> (get total-milestone-funds info) u0) 
                    (/ (* (get released-funds info) u100) (get total-milestone-funds info)) 
                    u0)
            }
        )
    )
)

;; Research Domain Expertise System
(define-constant ERR-DOMAIN-NOT-FOUND (err u121))
(define-constant ERR-EXPERTISE-ALREADY-CLAIMED (err u122))
(define-constant ERR-EXPERTISE-NOT-FOUND (err u123))
(define-constant ERR-INSUFFICIENT-VALIDATORS (err u124))
(define-constant ERR-ALREADY-VALIDATED (err u125))
(define-constant ERR-INVALID-DOMAIN (err u126))
(define-constant ERR-SELF-VALIDATION (err u127))

(define-constant MAX-DOMAINS u20)
(define-constant VALIDATION-THRESHOLD u3)
(define-constant EXPERTISE-VOTING-MULTIPLIER u2)

;; Domain registry
(define-map research-domains
    uint
    {
        name: (string-ascii 30),
        description: (string-ascii 100),
        active: bool,
        expert-count: uint
    }
)

;; Track next domain ID
(define-data-var domain-count uint u0)

;; Expertise claims by researchers
(define-map researcher-expertise
    { researcher: principal, domain-id: uint }
    {
        claimed-at: uint,
        validated: bool,
        validation-count: uint,
        evidence-hash: (string-ascii 64)
    }
)

;; Expertise validations
(define-map expertise-validations
    { researcher: principal, domain-id: uint, validator: principal }
    {
        validated-at: uint,
        support: bool,
        evidence-review: (string-ascii 200)
    }
)

;; Proposal domain classification
(define-map proposal-domains
    uint
    { primary-domain: uint, secondary-domain: (optional uint) }
)

;; Register a new research domain
(define-public (register-domain (name (string-ascii 30)) (description (string-ascii 100)))
    (let
        (
            (new-id (+ (var-get domain-count) u1))
        )
        (asserts! (<= new-id MAX-DOMAINS) ERR-INVALID-DOMAIN)
        (map-set research-domains new-id
            {
                name: name,
                description: description,
                active: true,
                expert-count: u0
            }
        )
        (var-set domain-count new-id)
        (ok new-id)
    )
)

;; Claim expertise in a research domain
(define-public (claim-expertise 
    (domain-id uint) 
    (evidence-hash (string-ascii 64))
)
    (let
        (
            (domain (unwrap! (map-get? research-domains domain-id) ERR-DOMAIN-NOT-FOUND))
            (expertise-key { researcher: tx-sender, domain-id: domain-id })
        )
        (asserts! (get active domain) ERR-DOMAIN-NOT-FOUND)
        (asserts! (is-none (map-get? researcher-expertise expertise-key)) ERR-EXPERTISE-ALREADY-CLAIMED)
        
        (map-set researcher-expertise expertise-key
            {
                claimed-at: stacks-block-height,
                validated: false,
                validation-count: u0,
                evidence-hash: evidence-hash
            }
        )
        (ok true)
    )
)

;; Validate another researcher's expertise claim
(define-public (validate-expertise 
    (researcher principal) 
    (domain-id uint) 
    (support bool)
    (evidence-review (string-ascii 200))
)
    (let
        (
            (expertise-key { researcher: researcher, domain-id: domain-id })
            (validation-key { researcher: researcher, domain-id: domain-id, validator: tx-sender })
            (expertise (unwrap! (map-get? researcher-expertise expertise-key) ERR-EXPERTISE-NOT-FOUND))
            (validator-expertise (map-get? researcher-expertise { researcher: tx-sender, domain-id: domain-id }))
        )
        (asserts! (not (is-eq tx-sender researcher)) ERR-SELF-VALIDATION)
        (asserts! (is-none (map-get? expertise-validations validation-key)) ERR-ALREADY-VALIDATED)
        
        ;; Validator should have some expertise or be an established member
        (asserts! (or 
            (is-some validator-expertise)
            (> (unwrap! (get-voting-power tx-sender) ERR-NOT-AUTHORIZED) u1000)
        ) ERR-NOT-AUTHORIZED)
        
        (map-set expertise-validations validation-key
            {
                validated-at: stacks-block-height,
                support: support,
                evidence-review: evidence-review
            }
        )
        
        (let
            (
                (new-count (if support (+ (get validation-count expertise) u1) (get validation-count expertise)))
            )
            (map-set researcher-expertise expertise-key
                (merge expertise
                    {
                        validation-count: new-count,
                        validated: (>= new-count VALIDATION-THRESHOLD)
                    }
                )
            )
            
            ;; Update domain expert count if newly validated
            (if (and support (>= new-count VALIDATION-THRESHOLD) (not (get validated expertise)))
                (let
                    (
                        (domain (unwrap! (map-get? research-domains domain-id) ERR-DOMAIN-NOT-FOUND))
                    )
                    (map-set research-domains domain-id
                        (merge domain { expert-count: (+ (get expert-count domain) u1) })
                    )
                    (ok true)
                )
                (ok true)
            )
        )
    )
)

;; Classify proposal by research domain
(define-public (classify-proposal 
    (proposal-id uint) 
    (primary-domain uint) 
    (secondary-domain (optional uint))
)
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? research-domains primary-domain)) ERR-DOMAIN-NOT-FOUND)
        
        (if (is-some secondary-domain)
            (asserts! (is-some (map-get? research-domains (unwrap-panic secondary-domain))) ERR-DOMAIN-NOT-FOUND)
            true
        )
        
        (map-set proposal-domains proposal-id
            {
                primary-domain: primary-domain,
                secondary-domain: secondary-domain
            }
        )
        (ok true)
    )
)

;; Enhanced voting with domain expertise weighting
(define-public (vote-with-expertise (proposal-id uint) (amount uint) (support bool))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (voter-key { proposal-id: proposal-id, voter: tx-sender })
            (base-voting-power (unwrap! (get-voting-power tx-sender) ERR-NOT-AUTHORIZED))
            (proposal-classification (map-get? proposal-domains proposal-id))
            (weighted-power (calculate-domain-weighted-power tx-sender proposal-classification base-voting-power))
        )
        (asserts! (< stacks-block-height (get end-stacks-block-height proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-none (map-get? votes voter-key)) ERR-ALREADY-VOTED)
        (asserts! (<= amount weighted-power) ERR-INVALID-AMOUNT)
        
        (map-set votes voter-key { amount: amount, support: support })
        (map-set proposals proposal-id
            (merge proposal
                {
                    votes-for: (if support (+ (get votes-for proposal) amount) (get votes-for proposal)),
                    votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) amount))
                }
            )
        )
        (ok true)
    )
)

;; Calculate domain-weighted voting power
(define-private (calculate-domain-weighted-power 
    (voter principal) 
    (classification (optional { primary-domain: uint, secondary-domain: (optional uint) }))
    (base-power uint)
)
    (if (is-none classification)
        base-power
        (let
            (
                (domains (unwrap-panic classification))
                (primary-expertise (map-get? researcher-expertise { researcher: voter, domain-id: (get primary-domain domains) }))
                (secondary-expertise (if (is-some (get secondary-domain domains))
                    (map-get? researcher-expertise { researcher: voter, domain-id: (unwrap-panic (get secondary-domain domains)) })
                    none
                ))
            )
            (let
                (
                    (primary-multiplier (if (and (is-some primary-expertise) (get validated (unwrap-panic primary-expertise))) EXPERTISE-VOTING-MULTIPLIER u1))
                    (secondary-multiplier (if (and (is-some secondary-expertise) (get validated (unwrap-panic secondary-expertise))) u1 u1))
                )
                (* base-power primary-multiplier secondary-multiplier)
            )
        )
    )
)

;; Read-only functions for expertise system
(define-read-only (get-domain (domain-id uint))
    (ok (map-get? research-domains domain-id))
)

(define-read-only (get-researcher-expertise (researcher principal) (domain-id uint))
    (ok (map-get? researcher-expertise { researcher: researcher, domain-id: domain-id }))
)

(define-read-only (get-expertise-validation (researcher principal) (domain-id uint) (validator principal))
    (ok (map-get? expertise-validations { researcher: researcher, domain-id: domain-id, validator: validator }))
)

(define-read-only (get-proposal-classification (proposal-id uint))
    (ok (map-get? proposal-domains proposal-id))
)

(define-read-only (is-domain-expert (researcher principal) (domain-id uint))
    (let
        (
            (expertise (map-get? researcher-expertise { researcher: researcher, domain-id: domain-id }))
        )
        (ok (and (is-some expertise) (get validated (unwrap-panic expertise))))
    )
)

(define-read-only (get-domain-experts (domain-id uint))
    (let
        (
            (domain (map-get? research-domains domain-id))
        )
        (if (is-some domain)
            (ok (get expert-count (unwrap-panic domain)))
            (ok u0)
        )
    )
)

