(defsystem "etradejanitor"
  :version "0.9.0"
  :author ""
  :license ""
  :depends-on ("str" "cl-csv" "postmodern" "local-time")
  :components ((:module "src"
                :components
                  ((:file "janitor/common")
                   (:file "janitor/types")
                   (:file "janitor/main" :depends-on ("janitor/common"))
                   (:file "janitor/parser" :depends-on ("janitor/common")))))
  :description ""
  :in-order-to ((test-op (test-op "etradejanitor/tests"))))

(defsystem "etradejanitor/tests"
  :author ""
  :license ""
  :depends-on ("etradejanitor"
               "rove")
  :components ((:module "t"
                :components
                (
                  (:file "janitor-test")
                )))
  :description "Test system for etradejanitor"
  :perform (test-op (op c) (symbol-call :rove :run c)))
