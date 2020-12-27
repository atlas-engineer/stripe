(in-package #:stripe)

(define-object customer-balance-transaction ()
  id
  amount
  created
  credit-note
  currency
  customer
  description
  ending-balance
  invoice
  (type :reader transaction-type))

(defmethod initialize-instance :after ((instance customer-balance-transaction)
                                       &key data &allow-other-keys)
  (destructuring-bind (&key created &allow-other-keys) data
    (reinitialize-instance
     instance
     :created (decode-timestamp created))))

(define-query create-customer-balance-transaction
    (:type customer-balance-transaction)
  (:post "customers/~a/balance-transactions" customer)
  amount
  currency
  description)

(define-query retrieve-customer-balance-transaction
    (:type customer-balance-transaction)
  (:get "customers/~a/balance-transactions/~a" customer transaction))

(define-query update-customer-balance-transaction
    (:type customer-balance-transaction)
  (:post "customers/~a/balance-transactions/~a" customer transaction)
  description)

(define-query list-customer-balance-transactions (:type list)
  (:get "customers/~a/balance-transactions" customer)
  ending-before
  limit
  starting-after)
