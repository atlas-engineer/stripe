(in-package #:net.mfiano.lisp.stripe)

(define-object refund ()
  id
  amount
  balance-transaction
  charge
  created
  currency
  failure-balance-transaction
  failure-reason
  reason
  receipt-number
  status)

(defmethod initialize-instance :after ((instance refund) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :created (decode-timestamp created))))

(define-query create-refund (:type refund)
  (:post "refunds")
  charge
  amount
  reason)

(define-query retrieve-refund (:type refund)
  (:get "refunds/~a" refund))

(define-query list-refunds (:type list)
  (:get "refunds")
  charge
  created
  ending-before
  limit
  starting-after)
