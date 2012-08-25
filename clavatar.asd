(asdf:defsystem #:clavatar
  :description "Determines appropriate URLs for avatars using assorted avatar services."
  :author "Pixie <pix@kepibu.org>"
  :license "MIT"
  :depends-on ("iolib" "ironclad" "babel" "drakma")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "identifiers")
               (:file "clavatar")))
