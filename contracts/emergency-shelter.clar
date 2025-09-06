;; Emergency Shelter Coordination Contract
;; Manages emergency shelter capacity, resource allocation, and evacuee tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-shelter-not-found (err u102))
(define-constant err-insufficient-capacity (err u103))
(define-constant err-evacuee-not-found (err u104))
(define-constant err-invalid-status (err u105))
(define-constant err-already-assigned (err u106))

;; Data Variables
(define-data-var shelter-id-nonce uint u0)
(define-data-var evacuee-id-nonce uint u0)
(define-data-var volunteer-id-nonce uint u0)

;; Shelter Management
(define-map shelters
  { shelter-id: uint }
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    total-capacity: uint,
    occupied-capacity: uint,
    manager: principal,
    status: (string-ascii 20), ;; "active", "inactive", "full"
    resources: {
      food-units: uint,
      water-units: uint,
      medical-supplies: uint,
      blankets: uint
    }
  }
)

;; Evacuee Tracking
(define-map evacuees
  { evacuee-id: uint }
  {
    name: (string-ascii 50),
    emergency-contact: (string-ascii 100),
    assigned-shelter: (optional uint),
    status: (string-ascii 20), ;; "registered", "sheltered", "evacuated"
    special-needs: (string-ascii 200),
    check-in-time: (optional uint)
  }
)

;; Volunteer Coordination
(define-map volunteers
  { volunteer-id: uint }
  {
    name: (string-ascii 50),
    contact: (string-ascii 100),
    skills: (string-ascii 200),
    assigned-shelter: (optional uint),
    availability: bool
  }
)

;; Shelter Assignments
(define-map shelter-assignments
  { shelter-id: uint, evacuee-id: uint }
  {
    assignment-time: uint,
    status: (string-ascii 20) ;; "active", "completed"
  }
)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (get-next-shelter-id)
  (let ((current-id (var-get shelter-id-nonce)))
    (var-set shelter-id-nonce (+ current-id u1))
    current-id
  )
)

(define-private (get-next-evacuee-id)
  (let ((current-id (var-get evacuee-id-nonce)))
    (var-set evacuee-id-nonce (+ current-id u1))
    current-id
  )
)

(define-private (get-next-volunteer-id)
  (let ((current-id (var-get volunteer-id-nonce)))
    (var-set volunteer-id-nonce (+ current-id u1))
    current-id
  )
)

;; Public Functions

;; Register a new emergency shelter
(define-public (register-shelter
    (name (string-ascii 50))
    (location (string-ascii 100))
    (total-capacity uint)
    (manager principal)
  )
  (let ((shelter-id (get-next-shelter-id)))
    (if (is-contract-owner)
      (begin
        (map-set shelters
          { shelter-id: shelter-id }
          {
            name: name,
            location: location,
            total-capacity: total-capacity,
            occupied-capacity: u0,
            manager: manager,
            status: "active",
            resources: {
              food-units: u0,
              water-units: u0,
              medical-supplies: u0,
              blankets: u0
            }
          }
        )
        (ok shelter-id)
      )
      err-owner-only
    )
  )
)

;; Register evacuee
(define-public (register-evacuee
    (name (string-ascii 50))
    (emergency-contact (string-ascii 100))
    (special-needs (string-ascii 200))
  )
  (let ((evacuee-id (get-next-evacuee-id)))
    (begin
      (map-set evacuees
        { evacuee-id: evacuee-id }
        {
          name: name,
          emergency-contact: emergency-contact,
          assigned-shelter: none,
          status: "registered",
          special-needs: special-needs,
          check-in-time: none
        }
      )
      (ok evacuee-id)
    )
  )
)

;; Assign evacuee to shelter
(define-public (assign-evacuee-to-shelter (evacuee-id uint) (shelter-id uint))
  (let (
    (evacuee-data (map-get? evacuees { evacuee-id: evacuee-id }))
    (shelter-data (map-get? shelters { shelter-id: shelter-id }))
  )
    (match evacuee-data
      evacuee
      (match shelter-data
        shelter
        (if (and
              (< (get occupied-capacity shelter) (get total-capacity shelter))
              (is-eq (get status shelter) "active")
              (is-none (get assigned-shelter evacuee))
            )
          (begin
            ;; Update evacuee record
            (map-set evacuees
              { evacuee-id: evacuee-id }
              (merge evacuee {
                assigned-shelter: (some shelter-id),
                status: "sheltered",
                check-in-time: (some burn-block-height)
              })
            )
            ;; Update shelter capacity
            (map-set shelters
              { shelter-id: shelter-id }
              (merge shelter {
                occupied-capacity: (+ (get occupied-capacity shelter) u1),
                status: (if (is-eq (+ (get occupied-capacity shelter) u1) (get total-capacity shelter)) "full" "active")
              })
            )
            ;; Create assignment record
            (map-set shelter-assignments
              { shelter-id: shelter-id, evacuee-id: evacuee-id }
              {
                assignment-time: burn-block-height,
                status: "active"
              }
            )
            (ok true)
          )
          (if (>= (get occupied-capacity shelter) (get total-capacity shelter))
            err-insufficient-capacity
            err-already-assigned
          )
        )
        err-shelter-not-found
      )
      err-evacuee-not-found
    )
  )
)

;; Register volunteer
(define-public (register-volunteer
    (name (string-ascii 50))
    (contact (string-ascii 100))
    (skills (string-ascii 200))
  )
  (let ((volunteer-id (get-next-volunteer-id)))
    (begin
      (map-set volunteers
        { volunteer-id: volunteer-id }
        {
          name: name,
          contact: contact,
          skills: skills,
          assigned-shelter: none,
          availability: true
        }
      )
      (ok volunteer-id)
    )
  )
)

;; Update shelter resources
(define-public (update-shelter-resources
    (shelter-id uint)
    (food-units uint)
    (water-units uint)
    (medical-supplies uint)
    (blankets uint)
  )
  (let ((shelter-data (map-get? shelters { shelter-id: shelter-id })))
    (match shelter-data
      shelter
      (if (or (is-contract-owner) (is-eq tx-sender (get manager shelter)))
        (begin
          (map-set shelters
            { shelter-id: shelter-id }
            (merge shelter {
              resources: {
                food-units: food-units,
                water-units: water-units,
                medical-supplies: medical-supplies,
                blankets: blankets
              }
            })
          )
          (ok true)
        )
        err-unauthorized
      )
      err-shelter-not-found
    )
  )
)

;; Read-only Functions

;; Get shelter information
(define-read-only (get-shelter (shelter-id uint))
  (map-get? shelters { shelter-id: shelter-id })
)

;; Get evacuee information
(define-read-only (get-evacuee (evacuee-id uint))
  (map-get? evacuees { evacuee-id: evacuee-id })
)

;; Get volunteer information
(define-read-only (get-volunteer (volunteer-id uint))
  (map-get? volunteers { volunteer-id: volunteer-id })
)

;; Get available shelter capacity
(define-read-only (get-available-capacity (shelter-id uint))
  (let ((shelter-data (map-get? shelters { shelter-id: shelter-id })))
    (match shelter-data
      shelter
      (some (- (get total-capacity shelter) (get occupied-capacity shelter)))
      none
    )
  )
)

;; Get total registered evacuees
(define-read-only (get-total-evacuees)
  (var-get evacuee-id-nonce)
)

;; Get total registered shelters
(define-read-only (get-total-shelters)
  (var-get shelter-id-nonce)
)

;; Get total registered volunteers
(define-read-only (get-total-volunteers)
  (var-get volunteer-id-nonce)
)
