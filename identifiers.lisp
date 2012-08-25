(in-package #:clavatar)

;; This is just quick and dirty, to avoid depending on one of the numerous SMTP
;; libraries.  If you actually have a dependence on an SMTP library, you should
;; define appropriate CANONICAL-IDENTIFIER and IDENTIFIER-DOMAIN methods for
;; that library's e-mail object instead.
(defstruct (%e-mail (:constructor make-%e-mail (local-part domain)))
  local-part domain)

(defun make-e-mail (string)
  (let ((pos (position #\@ string :from-end t :test #'char=)))
    (make-%e-mail (subseq string 0 pos) (subseq string (1+ pos)))))

(defun ensure-identifier (identifier)
  (typecase identifier
    (string
     (let ((identifier (string-trim #(#\Space #\Tab) identifier)))
       (cond
         ((string-equal "http" (subseq identifier 0 4))
          (let ((uri (puri:uri identifier)))
            (setf (puri:uri-host uri) (string-downcase (puri:uri-host uri)))
            uri))
         ((find #\@ identifier :from-end t :test #'char=)
          (make-e-mail (string-downcase identifier))))))
    (t identifier)))

(defun identifier-hash (identifier hash)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    hash
    (babel:string-to-octets (canonical-identifier identifier) :encoding :utf-8))))



(defgeneric canonical-identifier (identifier))

(defmethod canonical-identifier ((identifier string))
  (canonical-identifier (ensure-identifier identifier)))

(defmethod canonical-identifier ((uri puri:uri))
  (setf (puri:uri-host uri) (string-downcase (puri:uri-host uri)))
  (puri:render-uri uri nil))

(defmethod canonical-identifier ((e-mail %e-mail))
  (string-downcase
   (concatenate 'string (%e-mail-local-part e-mail) "@" (%e-mail-domain e-mail))))



(defgeneric identifier-domain (identifier))

(defmethod identifier-domain ((identifier string))
  (identifier-domain (ensure-identifier identifier)))

(defmethod identifier-domain ((identifier puri:uri))
  (puri:uri-host identifier))

(defmethod identifier-domain ((identifier %e-mail))
  (%e-mail-domain identifier))
