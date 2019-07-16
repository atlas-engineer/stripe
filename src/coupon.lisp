(in-package #:stripe)

(define-object coupon ()
  id
  amount-off
  created
  currency
  duration
  duration-in-months
  max-redemptions
  name
  percent-off
  redeem-by
  times-redeemed
  valid)

(defmethod initialize-instance :after ((instance coupon) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created redeem-by &allow-other-keys) data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :redeem-by (decode-timestamp redeem-by))))

(define-query create-coupon (:type coupon)
  (:post "coupons")
  id
  duration
  amount-off
  currency
  duration-in-months
  max-redemptions
  name
  percent-off
  redeem-by)

(define-query retrieve-coupon (:type coupon)
  (:get "coupons/~a" coupon))

(define-query update-coupon (:type coupon)
  (:post "coupons/~a" coupon)
  name)

(define-query delete-coupon ()
  (:delete "coupons/~a" coupon))

(define-query list-coupons (:type list)
  (:get "coupons")
  created
  ending-before
  limit
  starting-after)
