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

(define-public (vote (proposal-id uint) (amount uint) (support bool))
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