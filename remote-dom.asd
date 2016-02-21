(defsystem remote-dom
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:remote-js
               :plump-dom)
  :components ((:module "src"
                :serial t
                :components
                ((:file "remote-dom"))))
  :description "Control a browser DOM from Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op remote-dom-test))))
