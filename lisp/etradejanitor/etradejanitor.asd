(defsystem "etradejanitor"
  :version "0.9.0"
  :author ""
  :license ""
  :depends-on
    ("str"
    "cl-csv"
    "postmodern"
    "local-time"
    "parse-number"
    "trivia"
    "cl-redis")
  :components ((:module "src"
                :components
                  ((:file "janitor/common")
                   (:file "janitor/stockmarket/stockpurchase" 
                      :depends-on 
                      ("janitor/common" 
                       "janitor/stockmarket/util"))
                   (:file "janitor/stockmarket/stockprice" 
                      :depends-on 
                        ("janitor/common"))
                   (:file "janitor/stockmarket/util")
                   (:file "janitor/yahoo" :depends-on ("janitor/common"))
                   (:file "janitor/main"
                      :depends-on
                       ("janitor/common"
                        "janitor/db"
                        "janitor/redisutil"
                        "janitor/parser"))
                   (:file "janitor/testsetup"
                      :depends-on
                       ("janitor/stockmarket/stockprice"
                        "janitor/redisutil"))
                   (:file "janitor/db"
                      :depends-on
                       ("janitor/common"
                        "janitor/stockmarket/stockprice"
                        "janitor/stockmarket/stockpurchase"))
                   (:file "janitor/redisutil"
                      :depends-on
                       ("janitor/common"
                        "janitor/stockmarket/stockprice"
                        "janitor/parser"))
                   (:file "janitor/migrations"
                      :depends-on
                       ("janitor/common"
                        "janitor/db"))
                   (:file "janitor/parser"
                      :depends-on
                       ("janitor/stockmarket/stockprice"
                        "janitor/stockmarket/util"
                        "janitor/common")))))
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
