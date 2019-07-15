(asdf:defsystem #:stripe
  :description "A client for the Stripe payment API."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/stripe"
  :source-control (:git "https://github.com/mfiano/stripe.git")
  :bug-tracker "https://github.com/mfiano/stripe/issues"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:dexador
               #:golden-utils
               #:local-time
               #:yason)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "error")
   (:file "object")
   (:file "query")
   (:file "balance")
   (:file "balance-transaction")
   (:file "card")
   (:file "charge")
   (:file "coupon")
   (:file "credit-note")
   (:file "customer")
   (:file "customer-balance-transaction")
   (:file "customer-tax-id")
   (:file "discount")
   (:file "invoice")
   (:file "invoice-item")
   (:file "order")
   (:file "payout")
   (:file "plan")
   (:file "product")
   (:file "refund")
   (:file "return")
   (:file "sku")
   (:file "subscription")
   (:file "subscription-item")
   (:file "token")))
