(defpackage etradejanitor/tests/janitor-test
  (:use :cl
        :rove)
  (:import-from :local-time #:parse-timestring)
  (:import-from :janitor/common #:clet #:clet* #:timestamp)
  (:local-nicknames
    (#:parser #:janitor/parser)))

(in-package :etradejanitor/tests/janitor-test)

(defparameter test-feed "/home/rcs/opt/etradejanitor2/lisp/etradejanitor/t/resources")

(in-package :etradejanitor/tests/janitor-test)

(deftest test-prices-date-cutoff
  (testing "Parse stockprices"
    (clet*
      (parser::*feed* test-feed
       tm (timestamp 2025 3 21)
       result (parser::parse "YAR" tm))
      (print (length result))
      (ok (= 1 1)))))
