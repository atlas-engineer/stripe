(in-package #:stripe)

(define-object subscription ()
  billing-cycle-anchor
  cancel-at
  cancel-at-period-end
  canceled-at
  collection-method
  created
  current-period-end
  current-period-start
  customer
  days-until-due
  default-payment-method
  discount
  ended-at
  items
  latest-invoice
  plan
  quantity
  schedule
  start
  start-date
  status
  trial-end
  trial-start)

(defmethod initialize-instance :after ((instance subscription) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key billing-cycle-anchor cancel-at
                         canceled-at created current-period-end
                         current-period-start discount ended-at plan start
                         start-date trial-end trial-start &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :billing-cycle-anchor (decode-timestamp billing-cycle-anchor)
     :cancel-at (decode-timestamp cancel-at)
     :canceled-at (decode-timestamp canceled-at)
     :created (decode-timestamp created)
     :current-period-end (decode-timestamp current-period-end)
     :current-period-start (decode-timestamp current-period-start)
     :discount (make-instance 'discount :data discount)
     :ended-at (decode-timestamp ended-at)
     :plan (make-instance 'plan :data plan)
     :start (decode-timestamp start)
     :start-date (decode-timestamp start-date)
     :trial-end (decode-timestamp trial-end)
     :trial-start (decode-timestamp trial-start))))

(define-query create-subscription (:type subscription)
  (:post "subscriptions")
  customer
  backdate-start-date
  billing-cycle-anchor
  cancel-at
  cancel-at-period-end
  collection-method
  coupon
  days-until-due
  default-payment-method
  default-source
  items
  prorate
  trial-end
  trial-from-plan
  trial-period-days)

(define-query retrieve-subscription (:type subscription)
  (:get "subscriptions/~a" subscription))

(define-query update-subscription (:type subscription)
  (:post "subscriptions/~a" subscription)
  billing-cycle-anchor
  cancel-at
  cancel-at-period-end
  collection-method
  coupon
  days-until-due
  default-payment-method
  default-source
  items
  prorate
  proration-date
  trial-end
  trial-from-plan)

(define-query cancel-subscription (:type subscription)
  (:delete "subscriptions/~a" subscription)
  invoice-now
  prorate)

(define-query list-subscriptions (:type list)
  (:get "subscriptions")
  collection-method
  created
  current-period-end
  current-period-start
  customer
  ending-before
  limit
  plan
  starting-after
  status)
