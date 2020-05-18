(in-package #:net.mfiano.lisp.stripe)

(define-object customer-tax-id ()
  id
  country
  created
  customer
  (type :reader tax-id-type)
  value
  verification)

(define-object tax-id-verification ()
  status
  verified-address
  verified-name)

(defmethod initialize-instance :after ((instance customer-tax-id) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created verification &allow-other-keys) data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :verification (make-instance 'tax-id-verification :data verification))))

(define-query create-customer-tax-id (:type customer-tax-id)
  (:post "customers/~a/tax-ids" customer)
  type
  value)

(define-query retrieve-customer-tax-id (:type customer-tax-id)
  (:get "customers/~a/tax-ids/~a" customer tax-id))

(define-query delete-customer-tax-id ()
  (:delete "customers/~a/tax-ids/~a" customer tax-id))

(define-query list-customer-tax-ids (:type customer-tax-id)
  (:get "customers/~a/tax-ids" customer)
  ending-before
  limit
  starting-after)
