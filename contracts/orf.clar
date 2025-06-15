(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-MIN-THRESHOLD (err u105))

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