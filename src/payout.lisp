(in-package #:net.mfiano.lisp.stripe)

(define-object payout ()
  id
  amount
  arrival-date
  automatic
  balance-transaction
  created
  currency
  description
  destination
  failure-balance-transaction
  failure-code
  failure-message
  (method :reader payout-method)
  source-type
  statement-descriptor
  status
  (type :reader payout-type))

(defmethod initialize-instance :after ((instance payout) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key arrival-date created &allow-other-keys) data
    (reinitialize-instance
     instance
     :arrival-date (decode-timestamp arrival-date)
     :created (decode-timestamp created))))

(define-query create-payout (:type payout)
  (:post "payouts")
  amount
  currency
  description
  destination
  method
  source-type
  statement-descriptor)

(define-query retrieve-payout (:type payout)
  (:get "payouts" payout))

(define-query list-payouts (:type list)
  (:get "payouts")
  arrival-date
  created
  destination
  ending-before
  limit
  starting-after
  status)

(define-query cancel-payout (:type payout)
  (:post "payouts/~a/cancel" payout))
