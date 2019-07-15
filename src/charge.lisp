(in-package #:stripe)

(define-object charge ()
  id
  amount
  amount-refunded
  balance-transaction
  billing-details
  captured
  created
  currency
  customer
  description
  dispute
  failure-code
  failure-message
  fraud-details
  invoice
  order
  outcome
  paid
  payment-intent
  payment-method
  payment-method-details
  receipt-email
  receipt-number
  receipt-url
  refunded
  refunds
  review
  shipping
  statement-descriptor
  status)

(define-object charge-outcome ()
  network-status
  reason
  risk-level
  risk-score
  seller-message
  (type :reader outcome-type))

(define-object charge-card-details ()
  brand
  checks
  country
  exp-month
  exp-year
  fingerprint
  funding
  last4
  three-d-secure
  wallet)

(defmethod initialize-instance :after ((instance charge-card-details) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key checks &allow-other-keys) data
    (reinitialize-instance
     instance
     :checks (make-instance 'charge-card-checks :data checks))))

(define-object charge-card-checks ()
  address-line1-check
  address-postal-code-check
  cvc-check)

(define-object charge-fraud-details ()
  stripe-report
  user-report)

(defmethod initialize-instance :after ((instance charge) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key billing-details created fraud-details outcome
                         payment-method-details refunds &allow-other-keys)
      data
    (destructuring-bind (&key card &allow-other-keys) payment-method-details
      (reinitialize-instance
       instance
       :billing-details (make-instance 'billing-details :data billing-details)
       :created (decode-timestamp created)
       :fraud-details (make-instance 'charge-fraud-details :data fraud-details)
       :outcome (make-instance 'charge-outcome :data outcome)
       :payment-method-details (make-instance 'charge-card-details
                                              :data card)
       :refunds (decode-list refunds)))))

(define-query create-charge (:type charge)
  (:post "charges")
  amount
  currency
  capture
  customer
  description
  shipping
  source
  statement-descriptor)

(define-query retrieve-charge (:type charge)
  (:get "charges/~a" charge))

(define-query update-charge (:type charge)
  (:post "charges/~a" charge)
  customer
  description
  fraud-details)

(define-query list-charges (:type list)
  (:get "charges")
  created
  customer
  ending-before
  limit
  payment-intent
  starting-after)
