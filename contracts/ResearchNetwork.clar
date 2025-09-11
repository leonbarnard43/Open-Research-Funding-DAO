;; Research Collaboration Network & Impact Tracking Contract
;; Enables researcher networking, collaboration proposals, and impact measurement

(define-constant ERR_UNAUTHORIZED (err u300))
(define-constant ERR_PROFILE_NOT_FOUND (err u301))
(define-constant ERR_PROFILE_EXISTS (err u302))
(define-constant ERR_COLLABORATION_NOT_FOUND (err u303))
(define-constant ERR_COLLABORATION_EXISTS (err u304))
(define-constant ERR_INVALID_PARAMETERS (err u305))
(define-constant ERR_PROJECT_NOT_FOUND (err u306))
(define-constant ERR_CITATION_EXISTS (err u307))
(define-constant ERR_SELF_CITATION (err u308))

;; Constants
(define-constant MAX_INTERESTS u10)
(define-constant MAX_COLLABORATORS u20)
(define-constant COLLABORATION_DURATION u4320) ;; ~30 days
(define-constant MIN_IMPACT_SCORE u1)

;; Data variables
(define-data-var next-collaboration-id uint u1)
(define-data-var total-researchers uint u0)
(define-data-var total-collaborations uint u0)

;; Researcher profiles with research interests and background
(define-map researcher-profiles
  { researcher: principal }
  {
    name: (string-ascii 50),
    institution: (string-ascii 100),
    research-interests: (list 10 (string-ascii 30)),
    collaboration-count: uint,
    impact-score: uint,
    profile-created-at: uint,
    last-active: uint,
    total-citations-received: uint,
    total-projects-funded: uint
  }
)

;; Research project impact metrics
(define-map project-impact
  { project-id: uint }
  {
    research-outputs: uint,
    citations-received: uint,
    downloads: uint,
    replication-count: uint,
    collaboration-spawned: uint,
    impact-score: uint,
    last-updated: uint
  }
)

;; Collaboration proposals between researchers
(define-map collaboration-proposals
  { collaboration-id: uint }
  {
    proposer: principal,
    target-researcher: principal,
    project-focus: (string-ascii 100),
    proposed-duration: uint,
    skills-needed: (list 5 (string-ascii 30)),
    compensation-offered: uint,
    status: (string-ascii 20), ;; "pending", "accepted", "rejected", "active", "completed"
    created-at: uint,
    expires-at: uint
  }
)

;; Active collaborations tracking
(define-map active-collaborations
  { collaboration-id: uint }
  {
    lead-researcher: principal,
    collaborators: (list 20 principal),
    project-title: (string-ascii 100),
    start-date: uint,
    target-completion: uint,
    shared-resources: uint,
    collaboration-score: uint,
    deliverables-completed: uint,
    total-deliverables: uint
  }
)

;; Research citations and references between projects
(define-map research-citations
  { citing-project: uint, cited-project: uint }
  {
    citation-type: (string-ascii 20), ;; "reference", "builds-on", "compares", "refutes"
    citation-context: (string-ascii 150),
    cited-at: uint,
    impact-weight: uint ;; 1-5 scale
  }
)

;; Research output registry
(define-map research-outputs
  { project-id: uint, output-id: uint }
  {
    output-type: (string-ascii 30), ;; "paper", "code", "dataset", "protocol"
    title: (string-ascii 80),
    description: (string-ascii 200),
    access-hash: (string-ascii 64),
    published-at: uint,
    download-count: uint,
    citation-count: uint
  }
)

;; Collaboration scoring factors
(define-map collaboration-scores
  { researcher: principal }
  {
    successful-collaborations: uint,
    collaboration-rating: uint, ;; 1-100 scale
    response-time: uint, ;; Average blocks to respond to collaboration requests
    reliability-score: uint, ;; 1-100 scale based on completion rate
    last-calculated: uint
  }
)

;; Create or update researcher profile
(define-public (create-researcher-profile 
    (name (string-ascii 50)) 
    (institution (string-ascii 100))
    (research-interests (list 10 (string-ascii 30)))
)
    (let
        (
            (researcher-key { researcher: tx-sender })
            (current-block stacks-block-height)
            (existing-profile (map-get? researcher-profiles researcher-key))
        )
        (asserts! (<= (len research-interests) MAX_INTERESTS) ERR_INVALID_PARAMETERS)
        
        (match existing-profile
            profile (map-set researcher-profiles
                researcher-key
                (merge profile {
                    name: name,
                    institution: institution,
                    research-interests: research-interests,
                    last-active: current-block
                })
            )
            (begin
                (map-set researcher-profiles
                    researcher-key
                    {
                        name: name,
                        institution: institution,
                        research-interests: research-interests,
                        collaboration-count: u0,
                        impact-score: u0,
                        profile-created-at: current-block,
                        last-active: current-block,
                        total-citations-received: u0,
                        total-projects-funded: u0
                    }
                )
                (var-set total-researchers (+ (var-get total-researchers) u1))
            )
        )
        (ok true)
    )
)

;; Propose collaboration with another researcher
(define-public (propose-collaboration
    (target-researcher principal)
    (project-focus (string-ascii 100))
    (duration uint)
    (skills-needed (list 5 (string-ascii 30)))
    (compensation uint)
)
    (let
        (
            (collaboration-id (var-get next-collaboration-id))
            (current-block stacks-block-height)
            (proposer-profile (map-get? researcher-profiles { researcher: tx-sender }))
            (target-profile (map-get? researcher-profiles { researcher: target-researcher }))
        )
        (asserts! (not (is-eq tx-sender target-researcher)) ERR_UNAUTHORIZED)
        (asserts! (is-some proposer-profile) ERR_PROFILE_NOT_FOUND)
        (asserts! (is-some target-profile) ERR_PROFILE_NOT_FOUND)
        (asserts! (and (> duration u0) (<= duration COLLABORATION_DURATION)) ERR_INVALID_PARAMETERS)
        
        (map-set collaboration-proposals
            { collaboration-id: collaboration-id }
            {
                proposer: tx-sender,
                target-researcher: target-researcher,
                project-focus: project-focus,
                proposed-duration: duration,
                skills-needed: skills-needed,
                compensation-offered: compensation,
                status: "pending",
                created-at: current-block,
                expires-at: (+ current-block u1440) ;; 10 days
            }
        )
        
        (var-set next-collaboration-id (+ collaboration-id u1))
        (ok collaboration-id)
    )
)

;; Accept or reject collaboration proposal
(define-public (respond-to-collaboration (collaboration-id uint) (accept bool))
    (let
        (
            (proposal (unwrap! (map-get? collaboration-proposals { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
            (current-block stacks-block-height)
        )
        (asserts! (is-eq tx-sender (get target-researcher proposal)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status proposal) "pending") ERR_INVALID_PARAMETERS)
        (asserts! (< current-block (get expires-at proposal)) ERR_INVALID_PARAMETERS)
        
        (map-set collaboration-proposals
            { collaboration-id: collaboration-id }
            (merge proposal { status: (if accept "accepted" "rejected") })
        )
        
        ;; If accepted, create active collaboration
        (if accept
            (begin
                (map-set active-collaborations
                    { collaboration-id: collaboration-id }
                    {
                        lead-researcher: (get proposer proposal),
                        collaborators: (list tx-sender tx-sender tx-sender tx-sender tx-sender 
                                           tx-sender tx-sender tx-sender tx-sender tx-sender
                                           tx-sender tx-sender tx-sender tx-sender tx-sender
                                           tx-sender tx-sender tx-sender tx-sender tx-sender),
                        project-title: (get project-focus proposal),
                        start-date: current-block,
                        target-completion: (+ current-block (get proposed-duration proposal)),
                        shared-resources: (get compensation-offered proposal),
                        collaboration-score: u0,
                        deliverables-completed: u0,
                        total-deliverables: u5 ;; Default deliverable count
                    }
                )
                (var-set total-collaborations (+ (var-get total-collaborations) u1))
                (ok true)
            )
            (ok false)
        )
    )
)

;; Record research citation between projects
(define-public (add-research-citation
    (citing-project-id uint)
    (cited-project-id uint)
    (citation-type (string-ascii 20))
    (context (string-ascii 150))
    (impact-weight uint)
)
    (let
        (
            (citation-key { citing-project: citing-project-id, cited-project: cited-project-id })
            (current-block stacks-block-height)
            (existing-citation (map-get? research-citations citation-key))
        )
        (asserts! (not (is-eq citing-project-id cited-project-id)) ERR_SELF_CITATION)
        (asserts! (is-none existing-citation) ERR_CITATION_EXISTS)
        (asserts! (and (>= impact-weight u1) (<= impact-weight u5)) ERR_INVALID_PARAMETERS)
        
        (map-set research-citations
            citation-key
            {
                citation-type: citation-type,
                citation-context: context,
                cited-at: current-block,
                impact-weight: impact-weight
            }
        )
        
        ;; Update project impact metrics
        (let
            (
                (cited-impact (default-to 
                    { research-outputs: u0, citations-received: u0, downloads: u0, 
                      replication-count: u0, collaboration-spawned: u0, impact-score: u0, last-updated: u0 }
                    (map-get? project-impact { project-id: cited-project-id })))
            )
            (map-set project-impact
                { project-id: cited-project-id }
                (merge cited-impact {
                    citations-received: (+ (get citations-received cited-impact) u1),
                    impact-score: (+ (get impact-score cited-impact) impact-weight),
                    last-updated: current-block
                })
            )
        )
        
        (ok true)
    )
)

;; Register research output for a project
(define-public (register-research-output
    (project-id uint)
    (output-type (string-ascii 30))
    (title (string-ascii 80))
    (description (string-ascii 200))
    (access-hash (string-ascii 64))
)
    (let
        (
            (current-block stacks-block-height)
            (impact-data (default-to 
                { research-outputs: u0, citations-received: u0, downloads: u0, 
                  replication-count: u0, collaboration-spawned: u0, impact-score: u0, last-updated: u0 }
                (map-get? project-impact { project-id: project-id })))
            (next-output-id (+ (get research-outputs impact-data) u1))
        )
        
        (map-set research-outputs
            { project-id: project-id, output-id: next-output-id }
            {
                output-type: output-type,
                title: title,
                description: description,
                access-hash: access-hash,
                published-at: current-block,
                download-count: u0,
                citation-count: u0
            }
        )
        
        ;; Update project impact
        (map-set project-impact
            { project-id: project-id }
            (merge impact-data {
                research-outputs: next-output-id,
                last-updated: current-block
            })
        )
        
        (ok next-output-id)
    )
)

;; Track research output access/download
(define-public (track-output-access (project-id uint) (output-id uint))
    (let
        (
            (output-key { project-id: project-id, output-id: output-id })
            (output-data (unwrap! (map-get? research-outputs output-key) ERR_PROJECT_NOT_FOUND))
        )
        (map-set research-outputs
            output-key
            (merge output-data { download-count: (+ (get download-count output-data) u1) })
        )
        (ok true)
    )
)

;; Update collaboration score based on completed deliverables
(define-public (update-collaboration-progress (collaboration-id uint) (completed-deliverables uint))
    (let
        (
            (collaboration (unwrap! (map-get? active-collaborations { collaboration-id: collaboration-id }) ERR_COLLABORATION_NOT_FOUND))
            (current-block stacks-block-height)
        )
        ;; Only lead researcher or collaborators can update
        (asserts! (or 
            (is-eq tx-sender (get lead-researcher collaboration))
            (is-some (index-of (get collaborators collaboration) tx-sender))
        ) ERR_UNAUTHORIZED)
        
        (let
            (
                (completion-rate (if (> (get total-deliverables collaboration) u0)
                    (/ (* completed-deliverables u100) (get total-deliverables collaboration))
                    u0))
                (new-score (/ completion-rate u10)) ;; Score from 0-10 based on completion percentage
            )
            (map-set active-collaborations
                { collaboration-id: collaboration-id }
                (merge collaboration {
                    deliverables-completed: completed-deliverables,
                    collaboration-score: new-score
                })
            )
        )
        (ok true)
    )
)

;; Calculate and update researcher impact score
(define-public (calculate-researcher-impact (researcher principal))
    (let
        (
            (profile (unwrap! (map-get? researcher-profiles { researcher: researcher }) ERR_PROFILE_NOT_FOUND))
            (collaboration-data (default-to 
                { successful-collaborations: u0, collaboration-rating: u50, response-time: u100, reliability-score: u50, last-calculated: u0 }
                (map-get? collaboration-scores { researcher: researcher })))
        )
        
        (let
            (
                ;; Calculate impact based on various factors
                (citation-impact (* (get total-citations-received profile) u10))
                (collaboration-impact (* (get collaboration-count profile) u5))
                (reliability-impact (get reliability-score collaboration-data))
                (total-impact (+ citation-impact collaboration-impact reliability-impact))
            )
            (map-set researcher-profiles
                { researcher: researcher }
                (merge profile { impact-score: total-impact })
            )
            (ok total-impact)
        )
    )
)

;; Find potential collaborators based on research interests
(define-read-only (find-potential-collaborators (researcher principal) (interest (string-ascii 30)))
    (let
        (
            (profile (map-get? researcher-profiles { researcher: researcher }))
        )
        ;; In a real implementation, this would iterate through all researchers
        ;; For simplicity, returning a structure indicating the matching system exists
        (match profile
            data {
                search-interest: interest,
                matcher-count: u5, ;; Placeholder for actual matching algorithm
                avg-collaboration-score: u75,
                recommended-researchers: (list researcher) ;; Would contain actual matches
            }
            {
                search-interest: interest,
                matcher-count: u0,
                avg-collaboration-score: u0,
                recommended-researchers: (list)
            }
        )
    )
)

;; Get research network statistics
(define-read-only (get-network-stats)
    {
        total-researchers: (var-get total-researchers),
        total-collaborations: (var-get total-collaborations),
        active-collaborations: (var-get next-collaboration-id),
        avg-collaboration-duration: COLLABORATION_DURATION
    }
)

;; Read-only functions
(define-read-only (get-researcher-profile (researcher principal))
    (map-get? researcher-profiles { researcher: researcher })
)

(define-read-only (get-project-impact (project-id uint))
    (map-get? project-impact { project-id: project-id })
)

(define-read-only (get-collaboration-proposal (collaboration-id uint))
    (map-get? collaboration-proposals { collaboration-id: collaboration-id })
)

(define-read-only (get-active-collaboration (collaboration-id uint))
    (map-get? active-collaborations { collaboration-id: collaboration-id })
)

(define-read-only (get-research-citation (citing-project uint) (cited-project uint))
    (map-get? research-citations { citing-project: citing-project, cited-project: cited-project })
)

(define-read-only (get-research-output (project-id uint) (output-id uint))
    (map-get? research-outputs { project-id: project-id, output-id: output-id })
)

(define-read-only (get-collaboration-score (researcher principal))
    (map-get? collaboration-scores { researcher: researcher })
)

;; Get researcher's collaboration history and metrics
(define-read-only (get-researcher-collaboration-metrics (researcher principal))
    (let
        (
            (profile (map-get? researcher-profiles { researcher: researcher }))
            (collab-score (map-get? collaboration-scores { researcher: researcher }))
        )
        (match profile
            data {
                total-collaborations: (get collaboration-count data),
                impact-score: (get impact-score data),
                citations-received: (get total-citations-received data),
                collaboration-rating: (match collab-score score (get collaboration-rating score) u50),
                reliability: (match collab-score score (get reliability-score score) u50)
            }
            {
                total-collaborations: u0,
                impact-score: u0,
                citations-received: u0,
                collaboration-rating: u0,
                reliability: u0
            }
        )
    )
)

;; Get project's research impact summary
(define-read-only (get-project-impact-summary (project-id uint))
    (let
        (
            (impact-data (map-get? project-impact { project-id: project-id }))
        )
        (match impact-data
            data {
                outputs-published: (get research-outputs data),
                total-citations: (get citations-received data),
                total-downloads: (get downloads data),
                replications: (get replication-count data),
                collaborations-spawned: (get collaboration-spawned data),
                overall-impact-score: (get impact-score data),
                impact-rank: (calculate-impact-rank (get impact-score data))
            }
            {
                outputs-published: u0,
                total-citations: u0,
                total-downloads: u0,
                replications: u0,
                collaborations-spawned: u0,
                overall-impact-score: u0,
                impact-rank: "unranked"
            }
        )
    )
)

;; Calculate impact ranking tier
(define-private (calculate-impact-rank (score uint))
    (if (>= score u100)
        "high-impact"
        (if (>= score u50)
            "moderate-impact"
            "emerging"
        )
    )
)
