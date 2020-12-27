(in-package #:stripe)

(define-object sku ()
  id
  active
  attributes
  created
  currency
  image
  inventory
  package-dimensions
  price
  product
  updated)

(define-object sku-inventory ()
  quantity
  (type :reader inventory-type)
  value)

(defmethod initialize-instance :after ((instance sku) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key attributes created inventory
                         package-dimensions updated &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :attributes (u:plist->hash attributes :test #'equal)
     :created (decode-timestamp created)
     :inventory (make-instance 'sku-inventory :data inventory)
     :package-dimensions (make-instance 'package-dimensions
                                        :data package-dimensions)
     :updated (decode-timestamp updated))))

(define-query create-sku (:type sku)
  (:post "skus")
  currency
  inventory
  price
  product
  active
  attributes
  image
  package-dimensions)

(define-query retrieve-sku (:type sku)
  (:get "skus/~a" sku))

(define-query update-sku (:type sku)
  (:post "skus/~a" sku)
  active
  attributes
  currency
  image
  inventory
  package-dimensions
  price
  product)

(define-query list-skus (:type list)
  (:get "skus")
  active
  attributes
  ending-before
  ids
  in-stock
  limit
  product
  starting-after)

(define-query delete-sku ()
  (:delete "skus/~a" sku))
