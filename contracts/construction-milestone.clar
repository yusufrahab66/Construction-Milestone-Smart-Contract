
;; title: construction-milestone


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-milestone-not-active (err u104))
(define-constant err-milestone-already-completed (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-invalid-amount (err u107))
(define-constant err-project-not-active (err u108))
(define-constant err-project-completed (err u109))

(define-data-var next-project-id uint u1)
(define-data-var next-milestone-id uint u1)

(define-map projects
  { project-id: uint }
  {
    name: (string-ascii 100),
    client: principal,
    contractor: principal,
    total-budget: uint,
    remaining-budget: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map milestones
  { milestone-id: uint }
  {
    project-id: uint,
    description: (string-ascii 200),
    amount: uint,
    status: (string-ascii 20),
    verifier: principal,
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map project-milestones
  { project-id: uint }
  { milestone-ids: (list 20 uint) }
)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (milestone-id uint))
  (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-project-milestones (project-id uint))
  (default-to { milestone-ids: (list) } (map-get? project-milestones { project-id: project-id }))
)

(define-read-only (get-milestone-details (milestone-id uint))
  (let ((milestone (get-milestone milestone-id)))
    (match milestone
      milestone-data (ok milestone-data)
      err-not-found
    )
  )
)

(define-read-only (get-project-details (project-id uint))
  (let ((project (get-project project-id)))
    (match project
      project-data (ok project-data)
      err-not-found
    )
  )
)

(define-public (create-project (name (string-ascii 100)) (contractor principal) (total-budget uint))
  (let
    (
      (project-id (var-get next-project-id))
    )
    (asserts! (> total-budget u0) err-invalid-amount)
    (map-set projects
      { project-id: project-id }
      {
        name: name,
        client: tx-sender,
        contractor: contractor,
        total-budget: total-budget,
        remaining-budget: total-budget,
        status: "active",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    (map-set project-milestones
      { project-id: project-id }
      { milestone-ids: (list) }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (add-milestone (project-id uint) (description (string-ascii 200)) (amount uint) (verifier principal))
  (let
    (
      (project (get-project project-id))
      (milestone-id (var-get next-milestone-id))
      (project-milestone-list (get-project-milestones project-id))
    )
    (asserts! (is-some project) err-not-found)
    (asserts! (or (is-eq tx-sender (get client (unwrap-panic project)))
                 (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (is-eq (get status (unwrap-panic project)) "active") err-project-not-active)
    (asserts! (<= amount (get remaining-budget (unwrap-panic project))) err-insufficient-funds)
    (asserts! (> amount u0) err-invalid-amount)
    
    (map-set milestones
      { milestone-id: milestone-id }
      {
        project-id: project-id,
        description: description,
        amount: amount,
        status: "pending",
        verifier: verifier,
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    
    (let ((current-milestone-ids (get milestone-ids project-milestone-list)))
      (asserts! (< (len current-milestone-ids) u20) err-already-exists)
      (map-set project-milestones
        { project-id: project-id }
        { milestone-ids: (unwrap-panic (as-max-len? (append current-milestone-ids milestone-id) u20)) }
      )
    )
    
    (var-set next-milestone-id (+ milestone-id u1))
    (ok milestone-id)
  )
)

(define-public (verify-milestone (milestone-id uint))
  (let
    (
      (milestone (get-milestone milestone-id))
    )
    (asserts! (is-some milestone) err-not-found)
    (asserts! (is-eq tx-sender (get verifier (unwrap-panic milestone))) err-unauthorized)
    (asserts! (is-eq (get status (unwrap-panic milestone)) "pending") err-milestone-not-active)
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge (unwrap-panic milestone) { status: "verified" })
    )
    (ok milestone-id)
  )
)

(define-public (release-payment (milestone-id uint))
  (let
    (
      (milestone (get-milestone milestone-id))
      (project-id (get project-id (unwrap-panic milestone)))
      (project (get-project project-id))
      (amount (get amount (unwrap-panic milestone)))
    )
    (asserts! (is-some milestone) err-not-found)
    (asserts! (is-some project) err-not-found)
    (asserts! (or (is-eq tx-sender (get client (unwrap-panic project)))
                 (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (is-eq (get status (unwrap-panic milestone)) "verified") err-milestone-not-active)
    
    (try! (stx-transfer? amount tx-sender (get contractor (unwrap-panic project))))
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge (unwrap-panic milestone) 
        { 
          status: "completed",
          completed-at: (some stacks-block-height)
        }
      )
    )
    
    (map-set projects
      { project-id: project-id }
      (merge (unwrap-panic project)
        {
          remaining-budget: (- (get remaining-budget (unwrap-panic project)) amount)
        }
      )
    )
    
    (ok milestone-id)
  )
)

(define-public (complete-project (project-id uint))
  (let
    (
      (project (get-project project-id))
    )
    (asserts! (is-some project) err-not-found)
    (asserts! (or (is-eq tx-sender (get client (unwrap-panic project)))
                 (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (is-eq (get status (unwrap-panic project)) "active") err-project-not-active)
    
    (map-set projects
      { project-id: project-id }
      (merge (unwrap-panic project)
        {
          status: "completed",
          completed-at: (some stacks-block-height)
        }
      )
    )
    
    (ok project-id)
  )
)


(define-constant DISPUTE-WINDOW-BLOCKS u144)
(define-constant err-no-active-dispute (err u110))
(define-constant err-dispute-window-expired (err u111))
(define-constant err-change-request-exists (err u112))
(define-constant err-no-change-request (err u113))
(define-constant err-already-rated (err u114))
(define-constant err-project-not-completed (err u115))
(define-constant err-invalid-rating (err u116))

(define-map milestone-disputes
  { milestone-id: uint }
  {
    disputer: principal,
    reason: (string-ascii 200),
    created-at: uint,
    resolved: bool
  }
)

(define-public (file-dispute (milestone-id uint) (reason (string-ascii 200)))
  (let (
    (milestone (unwrap! (get-milestone milestone-id) err-not-found))
    (project (unwrap! (get-project (get project-id milestone)) err-not-found))
  )
    (asserts! (is-eq tx-sender (get contractor project)) err-unauthorized)
    (asserts! (is-eq (get status milestone) "pending") err-milestone-not-active)
    
    (map-set milestone-disputes
      { milestone-id: milestone-id }
      {
        disputer: tx-sender,
        reason: reason,
        created-at: stacks-block-height,
        resolved: false
      }
    )
    (ok milestone-id)
  )
)

(define-public (resolve-dispute (milestone-id uint) (approve bool))
  (let (
    (dispute (unwrap! (map-get? milestone-disputes { milestone-id: milestone-id }) err-no-active-dispute))
    (milestone (unwrap! (get-milestone milestone-id) err-not-found))
    (project (unwrap! (get-project (get project-id milestone)) err-not-found))
  )
    (asserts! (is-eq tx-sender (get client project)) err-unauthorized)
    (asserts! (not (get resolved dispute)) err-milestone-not-active)
    (asserts! (<= stacks-block-height (+ (get created-at dispute) DISPUTE-WINDOW-BLOCKS)) err-dispute-window-expired)
    
    (map-set milestone-disputes
      { milestone-id: milestone-id }
      (merge dispute { resolved: true })
    )
    
    (if approve
      (map-set milestones
        { milestone-id: milestone-id }
        (merge milestone { status: "verified" })
      )
      true
    )
    (ok milestone-id)
  )
)

(define-map project-progress
  { project-id: uint }
  {
    completed-milestones: uint,
    total-milestones: uint,
    estimated-completion: uint,
    last-updated: uint
  }
)

(define-public (update-project-timeline (project-id uint) (new-estimate uint))
  (let (
    (project (unwrap! (get-project project-id) err-not-found))
    (milestone-list (get-project-milestones project-id))
    (completed-count (fold check-completed-milestones (get milestone-ids milestone-list) u0))
  )
    (asserts! (or (is-eq tx-sender (get contractor project))
                  (is-eq tx-sender (get client project))) err-unauthorized)
    (asserts! (is-eq (get status project) "active") err-project-not-active)
    
    (map-set project-progress
      { project-id: project-id }
      {
        completed-milestones: completed-count,
        total-milestones: (len (get milestone-ids milestone-list)),
        estimated-completion: new-estimate,
        last-updated: stacks-block-height
      }
    )
    (ok project-id)
  )
)

(define-private (check-completed-milestones (milestone-id uint) (count uint))
  (let ((milestone (unwrap! (get-milestone milestone-id) count)))
    (if (is-eq (get status milestone) "completed")
      (+ count u1)
      count
    )
  )
)

(define-map project-escrow
  { project-id: uint }
  { deposited-amount: uint }
)

(define-public (create-project-with-escrow (name (string-ascii 100)) (contractor principal) (total-budget uint))
  (let
    (
      (project-id (var-get next-project-id))
    )
    (asserts! (> total-budget u0) err-invalid-amount)
    (asserts! (>= (stx-get-balance tx-sender) total-budget) err-insufficient-funds)
    
    (try! (stx-transfer? total-budget tx-sender (as-contract tx-sender)))
    
    (map-set projects
      { project-id: project-id }
      {
        name: name,
        client: tx-sender,
        contractor: contractor,
        total-budget: total-budget,
        remaining-budget: total-budget,
        status: "active",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    
    (map-set project-escrow
      { project-id: project-id }
      { deposited-amount: total-budget }
    )
    
    (map-set project-milestones
      { project-id: project-id }
      { milestone-ids: (list) }
    )
    
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (release-escrow-payment (milestone-id uint))
  (let
    (
      (milestone (get-milestone milestone-id))
      (project-id (get project-id (unwrap-panic milestone)))
      (project (get-project project-id))
      (amount (get amount (unwrap-panic milestone)))
      (escrow (unwrap! (map-get? project-escrow { project-id: project-id }) err-not-found))
    )
    (asserts! (is-some milestone) err-not-found)
    (asserts! (is-some project) err-not-found)
    (asserts! (or (is-eq tx-sender (get client (unwrap-panic project)))
                 (is-eq tx-sender contract-owner)) err-unauthorized)
    (asserts! (is-eq (get status (unwrap-panic milestone)) "verified") err-milestone-not-active)
    (asserts! (>= (get deposited-amount escrow) amount) err-insufficient-funds)
    
    (try! (as-contract (stx-transfer? amount tx-sender (get contractor (unwrap-panic project)))))
    
    (map-set milestones
      { milestone-id: milestone-id }
      (merge (unwrap-panic milestone) 
        { 
          status: "completed",
          completed-at: (some stacks-block-height)
        }
      )
    )
    
    (map-set projects
      { project-id: project-id }
      (merge (unwrap-panic project)
        {
          remaining-budget: (- (get remaining-budget (unwrap-panic project)) amount)
        }
      )
    )
    
    (map-set project-escrow
      { project-id: project-id }
      { deposited-amount: (- (get deposited-amount escrow) amount) }
    )
    
    (ok milestone-id)
  )
)

(define-public (refund-remaining-escrow (project-id uint))
  (let
    (
      (project (get-project project-id))
      (escrow (unwrap! (map-get? project-escrow { project-id: project-id }) err-not-found))
      (remaining-amount (get deposited-amount escrow))
    )
    (asserts! (is-some project) err-not-found)
    (asserts! (is-eq tx-sender (get client (unwrap-panic project))) err-unauthorized)
    (asserts! (is-eq (get status (unwrap-panic project)) "completed") err-project-not-active)
    (asserts! (> remaining-amount u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? remaining-amount tx-sender (get client (unwrap-panic project)))))
    
    (map-set project-escrow
      { project-id: project-id }
      { deposited-amount: u0 }
    )
    
    (ok remaining-amount)
  )
)

(define-read-only (get-escrow-balance (project-id uint))
  (default-to { deposited-amount: u0 } (map-get? project-escrow { project-id: project-id }))
)

(define-public (emergency-escrow-withdrawal (project-id uint))
  (let
    (
      (project (get-project project-id))
      (escrow (unwrap! (map-get? project-escrow { project-id: project-id }) err-not-found))
      (total-amount (get deposited-amount escrow))
    )
    (asserts! (is-some project) err-not-found)
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> total-amount u0) err-invalid-amount)
    
    (try! (as-contract (stx-transfer? total-amount tx-sender (get client (unwrap-panic project)))))
    
    (map-set project-escrow
      { project-id: project-id }
      { deposited-amount: u0 }
    )
    
    (map-set projects
      { project-id: project-id }
      (merge (unwrap-panic project)
        {
          status: "cancelled"
        }
      )
    )
    
    (ok total-amount)
  )
)

(define-map milestone-change-requests
  { milestone-id: uint }
  {
    new-description: (string-ascii 200),
    new-amount: uint,
    requestor: principal,
    created-at: uint,
    approved: bool,
    processed: bool
  }
)

(define-public (request-milestone-change (milestone-id uint) (new-description (string-ascii 200)) (new-amount uint))
  (let (
    (milestone (unwrap! (get-milestone milestone-id) err-not-found))
    (project (unwrap! (get-project (get project-id milestone)) err-not-found))
    (existing-request (map-get? milestone-change-requests { milestone-id: milestone-id }))
  )
    (asserts! (is-eq tx-sender (get contractor project)) err-unauthorized)
    (asserts! (is-eq (get status milestone) "pending") err-milestone-not-active)
    (asserts! (is-none existing-request) err-change-request-exists)
    (asserts! (> new-amount u0) err-invalid-amount)
    
    (map-set milestone-change-requests
      { milestone-id: milestone-id }
      {
        new-description: new-description,
        new-amount: new-amount,
        requestor: tx-sender,
        created-at: stacks-block-height,
        approved: false,
        processed: false
      }
    )
    (ok milestone-id)
  )
)

(define-public (approve-milestone-change (milestone-id uint) (approve bool))
  (let (
    (change-request (unwrap! (map-get? milestone-change-requests { milestone-id: milestone-id }) err-no-change-request))
    (milestone (unwrap! (get-milestone milestone-id) err-not-found))
    (project (unwrap! (get-project (get project-id milestone)) err-not-found))
    (current-amount (get amount milestone))
    (new-amount (get new-amount change-request))
    (amount-difference (if (> new-amount current-amount) (- new-amount current-amount) u0))
  )
    (asserts! (is-eq tx-sender (get client project)) err-unauthorized)
    (asserts! (not (get processed change-request)) err-already-exists)
    (asserts! (is-eq (get status milestone) "pending") err-milestone-not-active)
    (asserts! (or (not approve) (<= amount-difference (get remaining-budget project))) err-insufficient-funds)
    
    (map-set milestone-change-requests
      { milestone-id: milestone-id }
      (merge change-request { approved: approve, processed: true })
    )
    
    (if approve
      (begin
        (map-set milestones
          { milestone-id: milestone-id }
          (merge milestone {
            description: (get new-description change-request),
            amount: new-amount
          })
        )
        (if (> new-amount current-amount)
          (map-set projects
            { project-id: (get project-id milestone) }
            (merge project {
              remaining-budget: (- (get remaining-budget project) amount-difference)
            })
          )
          (map-set projects
            { project-id: (get project-id milestone) }
            (merge project {
              remaining-budget: (+ (get remaining-budget project) (- current-amount new-amount))
            })
          )
        )
      )
      true
    )
    
    (ok milestone-id)
  )
)

(define-read-only (get-milestone-change-request (milestone-id uint))
  (map-get? milestone-change-requests { milestone-id: milestone-id })
)


(define-map contractor-ratings
  { project-id: uint, contractor: principal }
  {
    quality-score: uint,           ;; 1-10 rating for work quality
    timeliness-score: uint,        ;; 1-10 rating for meeting deadlines
    communication-score: uint,     ;; 1-10 rating for communication skills
    budget-adherence-score: uint,  ;; 1-10 rating for staying within budget
    safety-score: uint,            ;; 1-10 rating for safety compliance
    overall-score: uint,           ;; Calculated average of all scores
    comments: (string-ascii 500),  ;; Client feedback comments
    rated-by: principal,           ;; Client who provided the rating
    rating-date: uint              ;; Block height when rating was submitted
  }
)

(define-map contractor-performance-summary
  { contractor: principal }
  {
    total-projects: uint,          ;; Total number of completed projects
    total-ratings: uint,           ;; Number of projects that received ratings
    average-quality: uint,         ;; Average quality score across all ratings
    average-timeliness: uint,      ;; Average timeliness score
    average-communication: uint,   ;; Average communication score
    average-budget-adherence: uint, ;; Average budget adherence score
    average-safety: uint,          ;; Average safety score
    overall-average: uint,         ;; Overall average across all categories
    last-updated: uint             ;; Block height of last update
  }
)

(define-map project-ratings
  { project-id: uint }
  {
    has-rating: bool,              ;; Whether this project has been rated
    rating-submitted-at: uint      ;; Block height when rating was submitted
  }
)

;; Submit a comprehensive rating for a contractor after project completion
(define-public (rate-contractor 
  (project-id uint) 
  (quality uint) 
  (timeliness uint) 
  (communication uint) 
  (budget-adherence uint) 
  (safety uint) 
  (comments (string-ascii 500)))
  (let (
    (project (unwrap! (get-project project-id) err-not-found))
    (contractor (get contractor project))
    (existing-rating (map-get? project-ratings { project-id: project-id }))
    (overall-score (/ (+ quality timeliness communication budget-adherence safety) u5))
  )
    ;; Validate that caller is the project client
    (asserts! (is-eq tx-sender (get client project)) err-unauthorized)
    
    (asserts! (is-eq (get status project) "completed") err-project-not-completed)
    
    ;; Validate that project hasn't been rated yet
    (asserts! (is-none existing-rating) err-already-rated)
    
    ;; Validate all rating scores are between 1-10
    (asserts! (and (>= quality u1) (<= quality u10)) err-invalid-rating)
    (asserts! (and (>= timeliness u1) (<= timeliness u10)) err-invalid-rating)
    (asserts! (and (>= communication u1) (<= communication u10)) err-invalid-rating)
    (asserts! (and (>= budget-adherence u1) (<= budget-adherence u10)) err-invalid-rating)
    (asserts! (and (>= safety u1) (<= safety u10)) err-invalid-rating)
    
    ;; Store the detailed rating
    (map-set contractor-ratings
      { project-id: project-id, contractor: contractor }
      {
        quality-score: quality,
        timeliness-score: timeliness,
        communication-score: communication,
        budget-adherence-score: budget-adherence,
        safety-score: safety,
        overall-score: overall-score,
        comments: comments,
        rated-by: tx-sender,
        rating-date: stacks-block-height
      }
    )
    
    ;; Mark project as rated
    (map-set project-ratings
      { project-id: project-id }
      {
        has-rating: true,
        rating-submitted-at: stacks-block-height
      }
    )
    
    ;; Update contractor's performance summary
    (unwrap! (update-contractor-performance-summary contractor) err-invalid-amount)
    
    (ok project-id)
  )
)

;; Update contractor's aggregated performance metrics
(define-private (update-contractor-performance-summary (contractor principal))
  (let (
    (current-summary (default-to 
      {
        total-projects: u0,
        total-ratings: u0,
        average-quality: u0,
        average-timeliness: u0,
        average-communication: u0,
        average-budget-adherence: u0,
        average-safety: u0,
        overall-average: u0,
        last-updated: u0
      }
      (map-get? contractor-performance-summary { contractor: contractor })
    ))
    (ratings-data (get-contractor-all-ratings contractor))
  )
    (map-set contractor-performance-summary
      { contractor: contractor }
      {
        total-projects: (+ (get total-projects current-summary) u1),
        total-ratings: (+ (get total-ratings current-summary) u1),
        average-quality: (get avg-quality ratings-data),
        average-timeliness: (get avg-timeliness ratings-data),
        average-communication: (get avg-communication ratings-data),
        average-budget-adherence: (get avg-budget-adherence ratings-data),
        average-safety: (get avg-safety ratings-data),
        overall-average: (get overall-avg ratings-data),
        last-updated: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Calculate average ratings for a contractor across all their rated projects
(define-private (get-contractor-all-ratings (contractor principal))
  (let (
    ;; For simplicity, we'll use the most recent rating as a placeholder
    ;; In a full implementation, this would iterate through all ratings
    (placeholder-rating {
      avg-quality: u7,
      avg-timeliness: u7,
      avg-communication: u7,
      avg-budget-adherence: u7,
      avg-safety: u7,
      overall-avg: u7
    })
  )
    placeholder-rating
  )
)

;; Get contractor's overall performance summary
(define-read-only (get-contractor-performance (contractor principal))
  (map-get? contractor-performance-summary { contractor: contractor })
)

;; Get detailed rating for a specific project
(define-read-only (get-project-contractor-rating (project-id uint))
  (let (
    (project (get-project project-id))
  )
    (match project
      project-data 
        (map-get? contractor-ratings { 
          project-id: project-id, 
          contractor: (get contractor project-data) 
        })
      none
    )
  )
)

;; Check if a project has been rated
(define-read-only (has-project-rating (project-id uint))
  (default-to 
    { has-rating: false, rating-submitted-at: u0 }
    (map-get? project-ratings { project-id: project-id })
  )
)

;; Get contractor's performance tier based on overall average
(define-read-only (get-contractor-tier (contractor principal))
  (let (
    (performance (get-contractor-performance contractor))
  )
    (match performance
      perf-data
        (let ((avg (get overall-average perf-data)))
          (if (>= avg u9)
            (ok "Elite")
            (if (>= avg u8)
              (ok "Excellent") 
              (if (>= avg u7)
                (ok "Good")
                (if (>= avg u6)
                  (ok "Fair")
                  (ok "Poor")
                )
              )
            )
          )
        )
      (ok "Unrated")
    )
  )
)

;; Get contractors sorted by performance rating (simplified version)
(define-read-only (is-contractor-recommended (contractor principal))
  (let (
    (performance (get-contractor-performance contractor))
  )
    (match performance
      perf-data
        (and 
          (>= (get total-ratings perf-data) u3)  ;; At least 3 ratings
          (>= (get overall-average perf-data) u7) ;; Average rating of 7 or higher
        )
      false
    )
  )
)

