;; CoachConnect: Professional coaching services platform with specialized training programs
;; Connects certified coaches with clients for personalized development and goal achievement

(define-data-var coaching-supervisor principal tx-sender)
(define-map coach-profiles
  { coach-id: uint }
  {
    trainer: principal,
    program-fee: uint,
    coaching-specialty: (string-ascii 50),
    certification-details: (string-ascii 500),
    coaching-experience: uint,
    licensed: bool
  }
)

(define-map coaching-records
  { coach-id: uint, program-id: uint }
  {
    participant: principal,
    enrollment-time: uint,
    program-format: (string-ascii 20)
  }
)

(define-data-var next-coach-id uint u1)
(define-map program-tracker 
  { coach-id: uint }
  { programs: uint }
)

;; Register as a coach
(define-public (register-coach (specialty-input (string-ascii 50)) (certification-input (string-ascii 500)) (experience-input uint) (fee-input uint))
  (let
    (
      (coach-id (var-get next-coach-id))
      (program-id u0)
      (specialty specialty-input)
      (certification certification-input)
      (experience experience-input)
      (fee fee-input)
    )
    ;; Input validation
    (asserts! (> fee u0) (err u1))
    (asserts! (> (len specialty) u0) (err u5))
    (asserts! (> (len certification) u0) (err u6))
    (asserts! (> experience u0) (err u7))
    
    (map-set coach-profiles
      { coach-id: coach-id }
      {
        trainer: tx-sender,
        program-fee: fee,
        coaching-specialty: specialty,
        certification-details: certification,
        coaching-experience: experience,
        licensed: false
      }
    )
    (map-set coaching-records
      { coach-id: coach-id, program-id: program-id }
      {
        participant: tx-sender,
        enrollment-time: coach-id,
        program-format: "registered"
      }
    )
    (map-set program-tracker 
      { coach-id: coach-id }
      { programs: u1 }
    )
    (var-set next-coach-id (+ coach-id u1))
    (ok coach-id)
  )
)

;; Enroll in coaching program
(define-public (enroll-coaching-program (coach-id-input uint))
  (let
    (
      (coach-id coach-id-input)
      (coach-info (unwrap! (map-get? coach-profiles { coach-id: coach-id }) (err u2)))
      (fee (get program-fee coach-info))
      (trainer (get trainer coach-info))
      (program-data (default-to { programs: u0 } (map-get? program-tracker { coach-id: coach-id })))
      (program-id (get programs program-data))
      (new-program-id (+ program-id u1))
    )
    ;; Input validation
    (asserts! (> coach-id u0) (err u8))
    (asserts! (not (is-eq tx-sender trainer)) (err u3))
    
    (try! (stx-transfer? fee tx-sender trainer))
    (map-set coaching-records
      { coach-id: coach-id, program-id: program-id }
      {
        participant: tx-sender,
        enrollment-time: (var-get next-coach-id),
        program-format: "enrolled"
      }
    )
    (map-set program-tracker 
      { coach-id: coach-id }
      { programs: new-program-id }
    )
    (ok true)
  )
)

;; License a coach (supervisor only)
(define-public (license-coach (coach-id-input uint))
  (let
    (
      (coach-id coach-id-input)
      (coach-info (unwrap! (map-get? coach-profiles { coach-id: coach-id }) (err u2)))
      (program-data (default-to { programs: u0 } (map-get? program-tracker { coach-id: coach-id })))
      (program-id (get programs program-data))
      (new-program-id (+ program-id u1))
    )
    ;; Input validation
    (asserts! (> coach-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get coaching-supervisor)) (err u4))
    
    (map-set coach-profiles
      { coach-id: coach-id }
      (merge coach-info { licensed: true })
    )
    (map-set coaching-records
      { coach-id: coach-id, program-id: program-id }
      {
        participant: (get trainer coach-info),
        enrollment-time: (var-get next-coach-id),
        program-format: "licensed"
      }
    )
    (map-set program-tracker 
      { coach-id: coach-id }
      { programs: new-program-id }
    )
    (ok true)
  )
)

;; Get coach profile
(define-read-only (get-coach (coach-id uint))
  (map-get? coach-profiles { coach-id: coach-id })
)

;; Get coaching program record
(define-read-only (get-program-record (coach-id uint) (program-id uint))
  (map-get? coaching-records { coach-id: coach-id, program-id: program-id })
)

;; Get total programs for a coach
(define-read-only (get-program-count (coach-id uint))
  (let
    (
      (program-data (default-to { programs: u0 } (map-get? program-tracker { coach-id: coach-id })))
    )
    (get programs program-data)
  )
)
