(in-package #:net.mfiano.lisp.stripe)

(define-object customer ()
  id
  address
  balance
  created
  currency
  default-source
  delinquent
  description
  discount
  email
  name
  phone
  shipping
  sources
  subscriptions)

(defmethod initialize-instance :after ((instance customer) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created address shipping sources
                         subscriptions &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :address (make-instance 'address :data address)
     :shipping (make-instance 'shipping :data shipping)
     :sources (decode-list sources)
     :subscriptions (decode-list subscriptions))))

(define-query create-customer (:type customer)
  (:post "customers")
  balance
  description
  name
  email
  phone
  address
  shipping
  source)

(define-query retrieve-customer (:type customer)
  (:get "customers/~a" customer))

(define-query update-customer (:type customer)
  (:post "customers/~a" customer)
  address
  balance
  coupon
  default-source
  description
  email
  name
  phone
  shipping
  source)

(define-query delete-customer ()
  (:delete "customers/~a" customer))

(define-query list-customers (:type list)
  (:get "customers")
  created
  email
  ending-before
  limit
  starting-after)
