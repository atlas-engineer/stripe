(in-package #:net.mfiano.lisp.stripe)

(define-object invoice-item ()
  id
  amount
  currency
  customer
  date
  description
  discountable
  invoice
  period-end
  period-start
  plan
  proration
  quantity
  subscription
  subscription-item
  unified-proration
  unit-amount)

(defmethod initialize-instance :after ((instance invoice-item) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key date period plan &allow-other-keys) data
    (destructuring-bind (&key end start) period
      (reinitialize-instance
       instance
       :date (decode-timestamp date)
       :period-end end
       :period-start start
       :plan (make-instance 'plan :data plan)))))

(define-query create-invoice-item (:type invoice-item)
  (:post "invoiceitems")
  currency
  customer
  amount
  description
  discountable
  invoice
  period
  quantity
  subscription
  unit-amount)

(define-query retrieve-invoice-item (:type invoice-item)
  (:get "invoiceitems/~a" invoice-item))

(define-query update-invoice-item (:type invoice-item)
  (:post "invoiceitems/~a" invoice-item)
  amount
  description
  discountable
  period
  quantity
  unit-amount)

(define-query delete-invoice-item ()
  (:delete "invoiceitems/~a" invoice-item))

(define-query list-invoice-items (:type list)
  (:get "invoiceitems")
  created
  customer
  ending-before
  invoice
  limit
  pending
  starting-after)
