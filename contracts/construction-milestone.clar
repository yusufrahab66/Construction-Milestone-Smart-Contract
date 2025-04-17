
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
