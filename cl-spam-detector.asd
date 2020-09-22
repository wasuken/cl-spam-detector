(defsystem "cl-spam-detector"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-ppcre"
			   "trivial-hashtable-serialize")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-spam-detector/tests"))))

(defsystem "cl-spam-detector/tests"
  :author ""
  :license ""
  :depends-on ("cl-spam-detector"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-spam-detector"
  :perform (test-op (op c) (symbol-call :rove :run c)))
