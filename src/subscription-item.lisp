(in-package #:stripe)

(define-object subscription-item ()
  id
  created
  plan
  quantity
  subscription)

(defmethod initialize-instance :after ((instance subscription-item) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key plan &allow-other-keys) data
    (reinitialize-instance
     instance
     :plan (make-instance 'plan :data plan))))

(define-query create-subscription-item (:type subscription-item)
  (:post "subscription-items")
  plan
  subscription
  prorate
  proration-date
  quantity)

(define-query retrieve-subscription-item (:type subscription-item)
  (:get "subscription-items/~a" subscription-item))

(define-query update-subscription-item (:type subscription-item)
  (:post "subscription-items/~a" subscription-item)
  plan
  prorate
  proration-date
  quantity)

(define-query delete-subscription-item ()
  (:delete "subscription-items/~a" subscription-item)
  clear-usage
  prorate
  proration-date)

(define-query list-subscription-items (:type list)
  (:get "subscription-items")
  ending-before
  limit
  starting-after)
