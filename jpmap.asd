(defsystem :jpmap
  :version "0.0.1"
  :licence "MIT"
  :description ""
  :author "biofermin2 <twitter @biofermin2>"
  :components
  ((:module "src"
	    :depends-on ("unio" "group-by")
	    :serial t
	    :components ((:file "package")
			 (:file "csv2tree")
			 (:file "set-hts")
			 (:file "jpmap")))))
