;; title: resource-tracker
;; summary: Resource management system for tracking materials, equipment, and labor costs

(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-invalid-amount (err u203))
(define-constant err-insufficient-budget (err u204))
(define-constant err-resource-exists (err u205))
(define-constant err-invalid-category (err u206))

(define-data-var next-resource-id uint u1)

;; Resource categories: materials, equipment, labor
(define-map milestone-resources
  { milestone-id: uint, resource-id: uint }
  {
    category: (string-ascii 20),
    name: (string-ascii 100),
    description: (string-ascii 200),
    estimated-cost: uint,
    actual-cost: uint,
    quantity: uint,
    unit: (string-ascii 20),
    supplier: (optional principal),
    status: (string-ascii 20),
    allocated-at: uint,
    updated-at: uint
  }
)

;; Resource budget tracking per milestone
(define-map milestone-budget
  { milestone-id: uint }
  {
    total-budget: uint,
    allocated-budget: uint,
    spent-budget: uint,
    materials-budget: uint,
    equipment-budget: uint,
    labor-budget: uint
  }
)

;; Cost summaries by category
(define-map resource-summary
  { milestone-id: uint, category: (string-ascii 20) }
  {
    estimated-total: uint,
    actual-total: uint,
    item-count: uint,
    last-updated: uint
  }
)

;; Resource allocation and cost tracking functions
(define-public (allocate-resource 
  (milestone-id uint)
  (category (string-ascii 20))
  (name (string-ascii 100))
  (description (string-ascii 200))
  (estimated-cost uint)
  (quantity uint)
  (unit (string-ascii 20))
  (supplier (optional principal)))
  (let
    (
      (resource-id (var-get next-resource-id))
      (milestone (contract-call? .construction-milestone get-milestone milestone-id))
      (budget-data (default-to
        { total-budget: u0, allocated-budget: u0, spent-budget: u0,
          materials-budget: u0, equipment-budget: u0, labor-budget: u0 }
        (map-get? milestone-budget { milestone-id: milestone-id })))
      (category-summary (default-to
        { estimated-total: u0, actual-total: u0, item-count: u0, last-updated: u0 }
        (map-get? resource-summary { milestone-id: milestone-id, category: category })))
    )
    
    ;; Validate milestone exists
    (asserts! (is-some milestone) err-not-found)
    
    ;; Validate category
    (asserts! (or (is-eq category "materials") (is-eq category "equipment") (is-eq category "labor")) 
              err-invalid-category)
    
    ;; Validate amounts
    (asserts! (> estimated-cost u0) err-invalid-amount)
    (asserts! (> quantity u0) err-invalid-amount)
    
    ;; Check budget availability
    (asserts! (<= (+ (get allocated-budget budget-data) estimated-cost) 
                  (get total-budget budget-data))
              err-insufficient-budget)
    
    ;; Store resource allocation
    (map-set milestone-resources
      { milestone-id: milestone-id, resource-id: resource-id }
      {
        category: category,
        name: name,
        description: description,
        estimated-cost: estimated-cost,
        actual-cost: u0,
        quantity: quantity,
        unit: unit,
        supplier: supplier,
        status: "allocated",
        allocated-at: stacks-block-height,
        updated-at: stacks-block-height
      })
    
    ;; Update budget tracking
    (map-set milestone-budget
      { milestone-id: milestone-id }
      (merge budget-data { allocated-budget: (+ (get allocated-budget budget-data) estimated-cost) }))
    
    ;; Update category summary
    (map-set resource-summary
      { milestone-id: milestone-id, category: category }
      {
        estimated-total: (+ (get estimated-total category-summary) estimated-cost),
        actual-total: (get actual-total category-summary),
        item-count: (+ (get item-count category-summary) u1),
        last-updated: stacks-block-height
      })
    
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (update-actual-cost (milestone-id uint) (resource-id uint) (actual-cost uint))
  (let
    (
      (resource (unwrap! (map-get? milestone-resources 
                                   { milestone-id: milestone-id, resource-id: resource-id }) 
                         err-not-found))
      (category (get category resource))
      (old-cost (get actual-cost resource))
      (budget-data (unwrap! (map-get? milestone-budget { milestone-id: milestone-id }) err-not-found))
      (category-summary (unwrap! (map-get? resource-summary 
                                          { milestone-id: milestone-id, category: category }) 
                                 err-not-found))
    )
    
    ;; Validate positive cost
    (asserts! (> actual-cost u0) err-invalid-amount)
    
    ;; Update resource with actual cost
    (map-set milestone-resources
      { milestone-id: milestone-id, resource-id: resource-id }
      (merge resource { 
        actual-cost: actual-cost,
        status: "completed",
        updated-at: stacks-block-height 
      }))
    
    ;; Update budget spent amount
    (map-set milestone-budget
      { milestone-id: milestone-id }
      (merge budget-data { 
        spent-budget: (+ (- (get spent-budget budget-data) old-cost) actual-cost)
      }))
    
    ;; Update category summary
    (map-set resource-summary
      { milestone-id: milestone-id, category: category }
      (merge category-summary {
        actual-total: (+ (- (get actual-total category-summary) old-cost) actual-cost),
        last-updated: stacks-block-height
      }))
    
    (ok actual-cost)
  )
)

(define-public (set-milestone-budget (milestone-id uint) (total-budget uint))
  (let
    (
      (milestone (contract-call? .construction-milestone get-milestone milestone-id))
    )
    
    ;; Validate milestone exists  
    (asserts! (is-some milestone) err-not-found)
    (asserts! (> total-budget u0) err-invalid-amount)
    
    (map-set milestone-budget
      { milestone-id: milestone-id }
      {
        total-budget: total-budget,
        allocated-budget: u0,
        spent-budget: u0,
        materials-budget: (/ (* total-budget u60) u100),  ;; 60% for materials
        equipment-budget: (/ (* total-budget u25) u100),  ;; 25% for equipment  
        labor-budget: (/ (* total-budget u15) u100)       ;; 15% for labor
      })
    
    (ok total-budget)
  )
)

;; Read-only functions for resource tracking
(define-read-only (get-resource-details (milestone-id uint) (resource-id uint))
  (map-get? milestone-resources { milestone-id: milestone-id, resource-id: resource-id })
)

(define-read-only (get-milestone-budget (milestone-id uint))
  (map-get? milestone-budget { milestone-id: milestone-id })
)

(define-read-only (get-category-summary (milestone-id uint) (category (string-ascii 20)))
  (map-get? resource-summary { milestone-id: milestone-id, category: category })
)

(define-read-only (calculate-cost-variance (milestone-id uint) (resource-id uint))
  (let
    (
      (resource (map-get? milestone-resources { milestone-id: milestone-id, resource-id: resource-id }))
    )
    (match resource
      resource-data
        (let
          (
            (estimated (get estimated-cost resource-data))
            (actual (get actual-cost resource-data))
            (variance (if (> actual estimated) (- actual estimated) (- estimated actual)))
            (variance-pct (if (> estimated u0) 
                            (/ (* variance u100) estimated)
                            u0))
          )
          (ok { variance: variance, variance-percentage: variance-pct, over-budget: (> actual estimated) })
        )
      err-not-found
    )
  )
)
