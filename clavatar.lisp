(in-package #:clavatar)

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
    (setf (puri:uri-parsed-path uri) (list :absolute "avatar" (call-next-method))
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
          (setf (puri:uri-parsed-path uri) (list :absolute "avatar" (hash-mail identifier :sha256)))
          uri)))))

(defun get-avatar-url (identifier &key size default (services '(libravatar gravatar)))
  "Returns an avatar URL for a given identifier (e-mail address).

First, checks if the domain of identifier provides some clue as to how to get
avatars.  If so, uses that.  Otherwise, tries SERVICES in order and uses the
first service which does not return a 404.  Will use DEFAULT on the last service
if no service knows about the identifier.

Note that because this involves DNS and HTTP queries, it is slow and you should
cache the results."
  (or (avatar-url 'federated identifier :size size :default default)
      (loop :for service :in (butlast services)
            :for url := (avatar-url service identifier :size size :default 404)
            :unless (= 404 (nth-value 1 (drakma:http-request url :redirect nil)))
              :do (return url))
      (avatar-url (car (last services)) identifier :default default)))
