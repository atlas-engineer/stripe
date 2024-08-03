(in-package #:stripe/tests)

(def-suite charge-tests)

(in-suite charge-tests)

(defparameter *charges-json*
  "{
    \"object\": \"list\",
    \"url\": \"/v1/charges\",
    \"has_more\": false,
    \"data\": [
      {
        \"id\": \"ch_3MmlLrLkdIwHu7ix0snN0B15\",
        \"object\": \"charge\",
        \"amount\": 1099,
        \"amount_captured\": 1099,
        \"amount_refunded\": 0,
        \"application\": null,
        \"application_fee\": null,
        \"application_fee_amount\": null,
        \"balance_transaction\": \"txn_3MmlLrLkdIwHu7ix0uke3Ezy\",
        \"billing_details\": {
          \"address\": {
            \"city\": null,
            \"country\": null,
            \"line1\": null,
            \"line2\": null,
            \"postal_code\": null,
            \"state\": null
          },
          \"email\": null,
          \"name\": null,
          \"phone\": null
        },
        \"calculated_statement_descriptor\": \"Stripe\",
        \"captured\": true,
        \"created\": 1679090539,
        \"currency\": \"usd\",
        \"customer\": null,
        \"description\": null,
        \"disputed\": false,
        \"failure_balance_transaction\": null,
        \"failure_code\": null,
        \"failure_message\": null,
        \"fraud_details\": {},
        \"invoice\": null,
        \"livemode\": false,
        \"metadata\": {},
        \"on_behalf_of\": null,
        \"outcome\": {
          \"network_status\": \"approved_by_network\",
          \"reason\": null,
          \"risk_level\": \"normal\",
          \"risk_score\": 32,
          \"seller_message\": \"Payment complete.\",
          \"type\": \"authorized\"
        },
        \"paid\": true,
        \"payment_intent\": null,
        \"payment_method\": \"card_1MmlLrLkdIwHu7ixIJwEWSNR\",
        \"payment_method_details\": {
          \"card\": {
            \"brand\": \"visa\",
            \"checks\": {
              \"address_line1_check\": null,
              \"address_postal_code_check\": null,
              \"cvc_check\": null
            },
            \"country\": \"US\",
            \"exp_month\": 3,
            \"exp_year\": 2024,
            \"fingerprint\": \"mToisGZ01V71BCos\",
            \"funding\": \"credit\",
            \"installments\": null,
            \"last4\": \"4242\",
            \"mandate\": null,
            \"network\": \"visa\",
            \"three_d_secure\": null,
            \"wallet\": null
          },
          \"type\": \"card\"
        },
        \"receipt_email\": null,
        \"receipt_number\": null,
        \"receipt_url\": \"https://pay.stripe.com/receipts/payment/CAcaFwoVYWNjdF8xTTJKVGtMa2RJd0h1N2l4KOvG06AGMgZfBXyr1aw6LBa9vaaSRWU96d8qBwz9z2J_CObiV_H2-e8RezSK_sw0KISesp4czsOUlVKY\",
        \"refunded\": false,
        \"review\": null,
        \"shipping\": null,
        \"source_transfer\": null,
        \"statement_descriptor\": null,
        \"statement_descriptor_suffix\": null,
        \"status\": \"succeeded\",
        \"transfer_data\": null,
        \"transfer_group\": null
      },
      {
        \"id\": \"ch_3MmlLrLkdIwHu7ix0snN0B16\",
        \"object\": \"charge\",
        \"amount\": 2000,
        \"amount_captured\": 2000,
        \"amount_refunded\": 0,
        \"application\": null,
        \"application_fee\": null,
        \"application_fee_amount\": null,
        \"balance_transaction\": \"txn_3MmlLrLkdIwHu7ix0uke3Ezz\",
        \"billing_details\": {
          \"address\": {
            \"city\": null,
            \"country\": null,
            \"line1\": null,
            \"line2\": null,
            \"postal_code\": null,
            \"state\": null
          },
          \"email\": null,
          \"name\": null,
          \"phone\": null
        },
        \"calculated_statement_descriptor\": \"Stripe\",
        \"captured\": true,
        \"created\": 1679090540,
        \"currency\": \"usd\",
        \"customer\": null,
        \"description\": null,
        \"disputed\": false,
        \"failure_balance_transaction\": null,
        \"failure_code\": null,
        \"failure_message\": null,
        \"fraud_details\": {},
        \"invoice\": null,
        \"livemode\": false,
        \"metadata\": {},
        \"on_behalf_of\": null,
        \"outcome\": {
          \"network_status\": \"approved_by_network\",
          \"reason\": null,
          \"risk_level\": \"normal\",
          \"risk_score\": 40,
          \"seller_message\": \"Payment complete.\",
          \"type\": \"authorized\"
        },
        \"paid\": true,
        \"payment_intent\": null,
        \"payment_method\": \"card_1MmlLrLkdIwHu7ixIJwEWSNS\",
        \"payment_method_details\": {
          \"card\": {
            \"brand\": \"visa\",
            \"checks\": {
              \"address_line1_check\": null,
              \"address_postal_code_check\": null,
              \"cvc_check\": null
            },
            \"country\": \"US\",
            \"exp_month\": 4,
            \"exp_year\": 2025,
            \"fingerprint\": \"mToisGZ01V71BCos\",
            \"funding\": \"credit\",
            \"installments\": null,
            \"last4\": \"4243\",
            \"mandate\": null,
            \"network\": \"visa\",
            \"three_d_secure\": null,
            \"wallet\": null
          },
          \"type\": \"card\"
        },
        \"receipt_email\": null,
        \"receipt_number\": null,
        \"receipt_url\": \"https://pay.stripe.com/receipts/payment/CAcaFwoVYWNjdF8xTTJKVGtMa2RJd0h1N2l4KOvG06AGMgZfBXyr1aw6LBa9vaaSRWU96d8qBwz9z2J_CObiV_H2-e8RezSK_sw0KISesp4czsOUlVKZ\",
        \"refunded\": false,
        \"review\": null,
        \"shipping\": null,
        \"source_transfer\": null,
        \"statement_descriptor\": null,
        \"statement_descriptor_suffix\": null,
        \"status\": \"succeeded\",
        \"transfer_data\": null,
        \"transfer_group\": null
      }
    ]
  }"
  "https://docs.stripe.com/api/charges/list")

(define-mock-query mock-list-charges (:type vector)
  *charges-json*)

(test list-charges
  (let* ((charges (mock-list-charges))
         (first-charge (aref charges 0))
         (second-charge (aref charges 1)))
    (is (= 2 (length charges)))
    (is (vectorp charges))
    (is (typep first-charge 'stripe:charge))
    (is (typep second-charge 'stripe:charge))
    (is (equal "ch_3MmlLrLkdIwHu7ix0snN0B15" (stripe:id first-charge)))
    (is (equal "ch_3MmlLrLkdIwHu7ix0snN0B16" (stripe:id second-charge)))
    (is (= 1099 (stripe:amount first-charge)))
    (is (= 2000 (stripe:amount second-charge)))
    (is (equal "usd" (stripe:currency first-charge)))
    (is (equal "usd" (stripe:currency second-charge)))
    (is (equal "succeeded" (stripe:status first-charge)))
    (is (equal "succeeded" (stripe:status second-charge)))))
