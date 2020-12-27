(in-package #:stripe)

(define-object balance ()
  available
  pending)

(define-object balance-funds ()
  amount
  currency
  bank-account
  card)

(defmethod initialize-instance :after ((instance balance-funds) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key source-types &allow-other-keys) data
    (destructuring-bind (&key bank-account card) source-types
      (reinitialize-instance
       instance
       :bank-account (or bank-account 0)
       :card (or card 0)))))

(defmethod initialize-instance :after ((instance balance) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key available pending &allow-other-keys) data
    (reinitialize-instance
     instance
     :available (mapcar
                 (lambda (x)
                   (make-instance 'balance-funds :data x))
                 available)
     :pending (mapcar
               (lambda (x)
                 (make-instance 'balance-funds :data x))
               pending))))

(define-query retrieve-balance (:type balance)
  (:get "balance"))
