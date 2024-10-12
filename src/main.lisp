(in-package :cl-user)

(defpackage peak
  (:use :cl)
  (:import-from :peak.options
				:show-options)
  (:export :main))
(in-package :peak)

(defun main ()
  (show-options))
