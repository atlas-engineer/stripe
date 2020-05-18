(in-package #:net.mfiano.lisp.stripe)

(define-object order ()
  id
  amount
  amount-returned
  charge
  created
  currency
  customer
  email
  external-coupon-code
  items
  returns
  selected-shipping-method
  shipping
  shipping-methods
  status
  status-transitions
  updated
  upstream-id)

(define-object order-shipping (shipping)
  carrier
  tracking-number)

(define-object order-shipping-methods ()
  id
  amount
  currency
  delivery-estimate
  description)

(defmethod initialize-instance :after ((instance order-shipping-methods)
                                       &key data &allow-other-keys)
  (destructuring-bind (&key delivery-estimate &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :delivery-estimate (make-instance 'order-deliver-estimate
                                       :data delivery-estimate))))

(define-object order-delivery-estimate ()
  date
  earliest
  latest
  (type :reader delivery-estimate-type))

(define-object order-item ()
  amount
  currency
  description
  parent
  quantity
  (type :reader order-item-type))

(defmethod initialize-instance :after ((instance order) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created items returns shipping
                         shipping-methods &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :items (mapcar
             (lambda (x)
               (make-instance 'order-item :data x))
             items)
     :returns (decode-list returns)
     :shipping (make-instance 'order-shipping :data shipping)
     :shipping-methods (mapcar
                        (lambda (x)
                          (make-instance 'order-shipping-methods
                                         :data x))
                        shipping-methods))))

(define-query create-order (:type order)
  (:post "orders")
  currency
  coupon
  customer
  email
  items
  shipping)

(define-query retrieve-order (:type order)
  (:get "orders/~a" order))

(define-query update-order (:type order)
  (:post "orders/~a" order)
  coupon
  selected-shipping-method
  shipping
  status)

(define-query pay-order (:type order)
  (:post "orders/~a/pay" order)
  customer
  source
  email)

(define-query list-orders (:type list)
  (:get "orders")
  created
  customer
  ending-before
  ids
  limit
  starting-after
  status
  status-transitions
  upstream-ids)

(define-query return-order (:type order-return)
  (:post "orders/~a/returns" order)
  items)
