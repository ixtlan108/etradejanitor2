(defpackage etradejanitor/tests/janitor-test
  (:use :cl
        :rove)
  (:import-from :local-time #:parse-timestring)
  (:import-from :janitor/common #:clet #:clet* #:date)
  (:import-from #:janitor/types #sp-dx))
  (:local-nicknames
    (#:parser #:janitor/parser)))

(in-package :etradejanitor/tests/janitor-test)
; company portal
(defparameter test-feed "/home/rcs/opt/etradejanitor2/lisp/etradejanitor/t/resources")

(in-package :etradejanitor/tests/janitor-test)

(defun sp-equal (sp)
  (clet (dx (sp-dx sp))
    nil))

(deftest test-prices-date-cutoff
  (testing "Parse stockprices"
    (clet*
      (parser::*feed* test-feed
       tm (date 2025 3 21)
       result (parser::parse "YAR" tm))
      (ok (= 6 (length result))))))
