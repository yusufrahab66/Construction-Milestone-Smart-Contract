
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