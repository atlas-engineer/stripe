(in-package #:net.mfiano.lisp.stripe)

(define-object order-return ()
  id
  amount
  created
  currency
  items
  order
  refund)

(defmethod initialize-instance :after ((instance order-return) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created items &allow-other-keys) data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :items (mapcar
             (lambda (x)
               (make-instance 'order-item :data x))
             items))))

(define-query retrieve-order-return (:type order-return)
  (:get "order-returns/~a" order-return))

(define-query list-order-returns (:type list)
  (:get "order-returns")
  created
  ending-before
  limit
  order
  starting-after)
