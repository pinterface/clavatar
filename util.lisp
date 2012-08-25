(in-package #:clavatar)

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
