(defsystem "jpmap"
  :version "0.0.1"
  :author "biofermin2"
  :license "MIT"
  :depends-on ("unio"
               "group-by"
	       "dexador")
  :serial t
  :components ((:file "jpmap"))
  :description "make a location infomation dataset"
  :in-order-to ((test-op (test-op "jpmap/tests"))))

(defsystem "jpmap/tests"
  :author ""
  :license ""
  :depends-on ("jpmap"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for jpmap"
  :perform (test-op (op c) (symbol-call :rove :run c)))
