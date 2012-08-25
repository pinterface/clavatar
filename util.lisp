(in-package #:clavatar)

(defun hash-mail (string hash)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    hash
    (babel:string-to-octets
     (nstring-downcase (string-trim #(#\Space #\Tab) string))
     :encoding :utf-8))))

(defun mail-domain (e-mail)
  (subseq e-mail (1+ (position #\@ e-mail :from-end t :test #'char=))))

(defun build-url-query (args)
  (let ((args (loop :for (k v) :on args :by #'cddr
                    :when v
                      :nconc (list k (etypecase v
                                       (string (urlencode:urlencode v :queryp t))
                                       (puri:uri (urlencode:urlencode (puri:render-uri v nil) :queryp t))
                                       (number v))))))
    (and args (format nil "~{~(~a~)=~a~^&~}" args))))

(defun default-port-p (scheme port)
  (when port
    (or (and (eq :https scheme) (= 443 port))
        (and (eq :http  scheme) (=  80 port)))))
