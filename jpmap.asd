(defsystem :jpmap
  :version "0.0.1"
  :licence "MIT"
  :description "make a location infomation dataset"
  :author "biofermin2 <twitter @biofermin2>"
  :depends-on ("unio" "group-by")
  :components
  ((:module "src"
     :serial t
     :components ((:file "package")
		  (:file "csv2tree")
		  (:file "set-hts")
		  (:file "jpmap")))))
