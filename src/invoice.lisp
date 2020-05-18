(in-package #:net.mfiano.lisp.stripe)

(define-object invoice ()
  id
  account-country
  account-name
  amount-due
  amount-remaining
  attempt-count
  attempted
  auto-advance
  billing-reason
  charge
  collection-method
  created
  currency
  customer
  customer-address
  customer-email
  customer-name
  customer-phone
  customer-shipping
  default-payment-method
  default-source
  description
  discount
  due-date
  ending-balance
  footer
  hosted-invoice-url
  invoice-pdf
  lines
  next-payment-attempt
  (number :reader invoice-number)
  paid
  payment-intent
  period-end
  period-start
  post-payment-credit-notes-amount
  pre-payment-credit-notes-amount
  receipt-number
  starting-balance
  status
  status-transitions
  subscription
  subscription-proration-date
  subtotal
  tax
  total
  webhooks-delivered-at)

(defmethod initialize-instance :after ((instance invoice) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key created customer-address customer-shipping
                         discount due-date next-payment-attempt period-end
                         period-start status-transitions
                       &allow-other-keys)
      data
    (reinitialize-instance
     instance
     :created (decode-timestamp created)
     :customer-address (make-instance 'address :data customer-address)
     :customer-shipping (make-instance 'shipping :data customer-shipping)
     :discount (make-instance 'discount :data discount)
     :due-date (decode-timestamp due-date)
     :next-payment-attempt (decode-timestamp next-payment-attempt)
     :period-end (decode-timestamp period-end)
     :period-start (decode-timestamp period-start)
     :status-transitions (make-instance 'invoice-status-transitions
                                        :data status-transitions))))

(define-object invoice-status-transition ()
  finalized-at
  marked-uncollectible-at
  paid-at
  voided-at)

(defmethod initialize-instance :after ((instance invoice-status-transition)
                                       &key data &allow-other-keys)
  (destructuring-bind (&key finalized-at marked-uncollectible-at paid-at
                         voided-at)
      data
    (reinitialize-instance
     instance
     :finalized-at (decode-timestamp finalized-at)
     :marked-uncollectible-at (decode-timestamp marked-uncollectible-at)
     :paid-at (decode-timestamp paid-at)
     :voided-at (decode-timestamp voided-at))))

(define-object invoice-line ()
  id
  amount
  currency
  description
  discountable
  invoice-item
  period
  proration
  quantity
  subscription
  subscription-item
  (type :reader source-type)
  unified-proration)

(define-query create-invoice (:type invoice)
  (:post "invoices")
  auto-advance
  collection-method
  customer
  days-until-due
  default-payment-method
  default-source
  description
  due-date
  footer
  statement-descriptor
  subscription)

(define-query retrieve-invoice (:type invoice)
  (:get "invoices/~a" invoice))

(define-query update-invoice (:type invoice)
  (:post "invoices/~a" invoice)
  auto-advance
  collection-method
  days-until-due
  default-payment-method
  default-source
  description
  due-date
  footer
  statement-descriptor)

(define-query delete-invoice (:type invoice)
  (:delete "invoices/~a" invoice))

(define-query finalize-invoice (:type invoice)
  (:post "invoices/~a/finalize" invoice)
  auto-advance)

(define-query pay-invoice (:type invoice)
  (:post "invoices/~a/pay" invoice)
  forgive
  paid-out-of-band
  payment-method
  source)

(define-query send-invoice (:type invoice)
  (:post "invoices/~a/send" invoice))

(define-query void-invoice (:type invoice)
  (:post "invoices/~a/void" invoice))

(define-query mark-invoice-uncollectible (:type invoie)
  (:post "invoices/~a/mark-uncollectible" invoice))

(define-query retrieve-invoice-lines (:type list)
  (:get "invoices/~a/lines" invoice)
  ending-before
  limit
  starting-after)

(define-query retrieve-upcoming-invoice (:type invoice)
  (:get "invoices/upcoming")
  coupon
  customer
  invoice-items
  schedule
  subscription
  subscription-billing-cycle-anchor
  subscription-cancel-at
  subscription-cancel-at-period-end
  subscription-cancel-now
  subscription-items
  subscription-prorate
  subscription-proration-date
  subscription-start-date
  subscription-trial-end
  subscription-trial-from-plan)

(define-query retrieve-upcoming-invoice-lines (:type list)
  (:get "invoices/upcoming/lines")
  coupon
  customer
  ending-before
  invoice-items
  limit
  schedule
  starting-after
  subscription
  subscription-billing-anchor
  subscription-cancel-at
  subscription-cancel-at-period-end
  subscription-cancel-now
  subscription-items
  subscription-prorate
  subscription-proration-date
  subscription-start-date
  subscription-trial-end
  subscription-trial-from-plan)

(define-query list-invoices (:type list)
  (:get "invoices")
  collection-method
  created
  customer
  due-date
  ending-before
  limit
  starting-after
  status
  subscription)
