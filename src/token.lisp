(in-package #:stripe)

(define-object card-token ()
  id
  card
  client-ip
  created
  used)

(defmethod initialize-instance :after ((instance card-token) &key data
                                       &allow-other-keys)
  (destructuring-bind (&key card &allow-other-keys) data
    (reinitialize-instance
     instance
     :card (make-instance 'card :data card))))

(define-query create-card-token (:type card-token)
  (:post "tokens")
  card)

(define-query retrieve-card-token (:type card-token)
  (:get "tokens/~a" card-token))
