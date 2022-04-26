(defsystem "cl-site-gen"
  :version "0.1.0"
  :author "Gavin Jaeger-Freeborn"
  :license ""
  :depends-on ("arrows"
               "uiop"
               "alexandria"
               "fset"
               "flute"   ;; html generation
               "cl-css") ;; css generation
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Create Your Static Websites With Lisp"
  :in-order-to ((test-op (test-op "cl-site-gen/tests")))
  ;; Build a binary
  :build-operation "program-op"
  :build-pathname "csg"
  :entry-point "csg::main")

(defsystem "cl-site-gen/tests"
  :author "Gavin Jaeger-Freeborn"
  :license ""
  :depends-on ("cl-site-gen"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-site-gen"
  :perform (test-op (op c) (symbol-call :rove :run c)))
