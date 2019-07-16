(in-package #:stripe)

(defvar *base-url* "https://api.stripe.com/v1")

(defvar *api-version* "2019-05-16")

(defvar *api-key*)

(defun normalize-string (string)
  (substitute #\- #\_ (string-upcase string) :test #'char=))

(defun normalize-json-key (string)
  (a:make-keyword (normalize-string string)))

(defun decode-list (list)
  (destructuring-bind (&key data has-more &allow-other-keys) list
    (values
     (mapcar
      (lambda (x)
        (let ((type (find-symbol (normalize-string (getf x :object)) :stripe2)))
          (make-instance type :data x)))
      data)
     has-more)))

(defun decode-timestamp (unix-time)
  (when unix-time
    (local-time:unix-to-timestamp unix-time)))
