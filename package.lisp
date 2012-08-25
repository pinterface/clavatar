;;;; package.lisp

(defpackage #:clavatar
  (:use #:cl)
  (:export #:get-avatar-url
           #:gravatar
           #:libravatar
           #:unicornify

           #:canonical-identifier
           #:identifier-domain))
