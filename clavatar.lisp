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
                      :nconc (list k (if (stringp v)
                                         (urlencode:urlencode v :queryp t)
                                         v)))))
    (and args (format nil "~{~(~a~)=~a~^&~}" args))))

(defun default-port-p (scheme port)
  (when port
    (or (and (eq :https scheme) (= 443 port))
        (and (eq :http  scheme) (=  80 port)))))

(defclass hosted-service () (base-uri))
(defclass libravatar-protocol () ())

(defclass gravatar (hosted-service)
  ((base-uri :initform (puri:uri "https://secure.gravatar.com/"))))

(defclass libravatar (hosted-service libravatar-protocol)
  ((base-uri :initform (puri:uri "https://seccdn.libravatar.org/"))))

(defclass unicornify (hosted-service)
  ((base-uri :initform (puri:uri "https://unicornify.appspot.com/"))))

(defclass federated-service (libravatar-protocol) ())

(defgeneric avatar-url (service identifier &key size default))

(defmethod avatar-url ((service (eql 'gravatar)) identifier &rest rest)
  (apply #'avatar-url (make-instance 'gravatar) identifier rest))
(defmethod avatar-url ((service (eql 'libravatar)) identifier &rest rest)
  (apply #'avatar-url (make-instance 'libravatar) identifier rest))
(defmethod avatar-url ((service (eql 'unicornify)) identifier &rest rest)
  (apply #'avatar-url (make-instance 'unicornify) identifier rest))
(defmethod avatar-url ((service (eql 'federated)) identifier &rest rest)
  (apply #'avatar-url (make-instance 'federated-service) identifier rest))

(defmethod avatar-url :around ((service hosted-service) (identifier string) &key size default &allow-other-keys)
  (let ((uri (puri:uri (slot-value service 'base-uri))))
    (setf (puri:uri-parsed-path uri) (list :absolute "avatars" (call-next-method))
          (puri:uri-query uri) (build-url-query (list :s size :d default)))
    uri))

(defmethod avatar-url ((service gravatar) (identifier string) &rest rest)
  (declare (ignore rest))
  (hash-mail identifier :md5))

;; FIXME: Add support for OpenID identifiers
(defmethod avatar-url ((service libravatar-protocol) (identifier string) &rest rest)
  (declare (ignore rest))
  (hash-mail identifier :sha256))

(defmethod avatar-url ((service unicornify) (identifier string) &rest rest)
  (declare (ignore rest))
  (hash-mail identifier :md5))

;; FIXME: Add support for OpenID identifiers
(defmethod avatar-url ((service federated-service) (identifier string) &key size default &allow-other-keys)
  (let* ((response (iolib.sockets::dns-query (format nil "_avatars-sec._tcp.~A." (mail-domain identifier)) :type :srv :decode t)))
    (when (listp response)
      (destructuring-bind (port . host) (cdddr response)
        (let ((uri (make-instance 'puri:uri :scheme :https
                                            :host host
                                            :port (unless (default-port-p :https port) port)
                                            :query (build-url-query (list :s size :d default)))))
          (setf (puri:uri-parsed-path uri) (list :absolute "avatars" (hash-mail identifier :sha256)))
          uri)))))

(or (avatar-url 'federated "foo@bar.com" :size 80)
    (avatar-url 'unicornify "foo@bar.com" :size 80))
