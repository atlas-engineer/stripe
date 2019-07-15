(in-package #:stripe)

(define-object discount ()
  coupon
  customer
  end
  start
  subscription)

(defmethod initialize-instance :after ((instance discount) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key end start &allow-other-keys) data
    (reinitialize-instance
     instance
     :end (decode-timestamp end)
     :start (decode-timestamp start))))

(define-query delete-customer-discount ()
  (:delete "customers/~a/discount" customer))

(define-query delete-subscription-discount ()
  (:delete "subscriptions/~a/discount" subscription))
