(in-package #:stripe)

(define-object session ()
  id
  cancel-url
  client-reference-id
  currency
  customer
  customer-email
  line-items
  metadata
  mode
  payment-intent
  payment-method-types
  payment-status
  success-url)

(defmethod initialize-instance :after ((instance session) &key data
                                       &allow-other-keys)
  (reinitialize-instance
   instance))

(define-query create-session (:type session)
  (:post "checkout/sessions")
  cancel-url
  line-items
  mode
  payment-method-types
  success-url)
