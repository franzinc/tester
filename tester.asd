(in-package :cl-user)

(asdf:defsystem :tester
    :description "A test harness for Common Lisp."
    :author "Kevin Layer <layer@franz.com>"
    :license "LLGPL (http://opensource.franz.com/preamble.html)"
    :components ((:static-file "readme.txt")
		 (:file "tester")))

(defmethod asdf::source-file-type ((c asdf::cl-source-file)
				   (s (eql (asdf::find-system :tester))))
  "cl")

