(asdf:defsystem #:clavatar
  :description "Determines appropriate URLs for avatars using assorted avatar services."
  :author "Pixie <pix@kepibu.org>"
  :license "MIT"
  :depends-on ("iolib" "ironclad" "babel" "do-urlencode")
  :serial t
  :components ((:file "package")
               (:file "clavatar")))