(in-package #:stripe)

(define-object plan ()
  id
  active
  aggregate-usage
  amount
  billing-scheme
  created
  currency
  interval
  nickname
  product
  trial-period-days
  usage-type)

(defmethod initialize-instance :after ((instance plan) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created &allow-other-keys) data
    (reinitialize-instance
     instance
     :created (decode-timestamp created))))

(define-query create-plan (:type plan)
  (:post "plans")
  active
  aggregate-usage
  amount
  billing-scheme
  currency
  interval
  interval-count
  nickname
  product
  trial-period-days
  usage-type)

(define-query retrieve-plan (:type plan)
  (:get "plans/~a" plan))

(define-query update-plan (:type plan)
  (:post "plans/~a" plan)
  active
  nickname
  product
  trial-period-days)

(define-query delete-plan ()
  (:delete "plans/~a" plan))

(define-query list-plans (:type list)
  (:get "plans")
  active
  created
  ending-before
  limit
  product
  starting-after)
