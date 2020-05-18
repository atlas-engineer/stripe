(in-package #:net.mfiano.lisp.stripe)

(define-object balance-transaction ()
  id
  amount
  available-on
  created
  currency
  description
  exchange-rate
  fee
  fee-details
  net
  source
  status
  (type :reader transaction-type))

(define-object fee ()
  amount
  application
  currency
  description
  (type :reader fee-type))

(defmethod initialize-instance :after ((instance balance-transaction) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key available-on created fee-details &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :available-on (decode-timestamp available-on)
     :created (decode-timestamp created)
     :fee-details (mapcar
                   (lambda (x)
                     (make-instance 'fee :data x))
                   fee-details))))

(define-query retrieve-balance-transaction (:type balance-transaction)
  (:get "balance/history/~a" balance-transaction))

(define-query list-balance-history (:type list)
  (:get "balance/history")
  available-on
  created
  currency
  ending-before
  limit
  payout
  source
  starting-after
  type)
