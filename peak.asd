(in-package :asdf-user)

(defsystem "peak"
  :version "0.0.1"
  :author "ronnie"
  :license "MIT"
  :depends-on ("sqlite" "str")
  :components ((:module "src"
				;; :serial t
                :components
                (;; (:file "file-name")
				 (:file "main" :depends-on ("options"))
				 (:file "options"))))
  :description "a simple productivity tool"
  :build-operation "program-op" ;; leave as is
  :build-pathname "peak"
  ;; entry-point: main is an exported symbol. Otherwise, use "peak::main" instead.
  :entry-point "peak:main")

;;;; sbcl executable compression
;;#+sb-core-compression
;;(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
;;  (uiop:dump-image (asdf:output-file o c)
;;                   :executable t
;;                   :compression t))
