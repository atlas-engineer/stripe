(in-package #:net.mfiano.lisp.stripe)

(define-object product ()
  id
  active
  attributes
  caption
  created
  description
  images
  name
  package-dimensions
  shippable
  statement-descriptor
  (type :reader product-type)
  unit-label
  updated
  url)

(define-object package-dimensions ()
  height
  (length :reader package-length)
  weight
  width)

(defmethod initialize-instance :after ((instance product) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created package-dimensions updated
                       &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :package-dimensions (make-instance 'package-dimensions
                                        :data package-dimensions)
     :updated (decode-timestamp updated))))

(define-query create-product (:type product)
  (:post "products")
  name
  type
  active
  attributes
  caption
  description
  images
  package-dimensions
  shippable
  url)

(define-query retrieve-product (:type product)
  (:get "products/~a" product))

(define-query update-product (:type product)
  (:post "products/~a" product)
  active
  attributes
  caption
  description
  images
  name
  package-dimensions
  shippable
  statement-descriptor
  unit-label
  url)

(define-query list-products (:type list)
  (:get "products")
  active
  created
  ending-before
  ids
  limit
  shippable
  starting-after
  type
  url)

(define-query delete-product ()
  (:delete "products" product))
