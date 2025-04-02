(defpackage etradejanitor/tests/janitor-test
  (:use :cl
        :rove)
  (:import-from :local-time #:parse-timestring #:timestamp=)
  (:import-from :janitor/common #:clet #:clet* #:date)
  (:local-nicknames
    (#:ty #:janitor/types)
    (#:parser #:janitor/parser)))

(in-package :etradejanitor/tests/janitor-test)

; company portal
;
(defparameter test-feed "/home/rcs/opt/etradejanitor2/lisp/etradejanitor/t/resources")

(defvar expected
  (ty::make-stockprice
    :dx (date 2025 3 28)
    :opn 317.1
    :hi 319.3
    :lo 311.9
    :cls 314.2
    :vol 497342))

(deftest test-types
  (testing "Stockprice"
    (clet
      (actual
        (ty:mk-stockprice
          '("2025-03-28 00:00:00+00:00" "317.1000061035156" "319.29998779296875" "311.8999938964844" "314.20001220703125" "497342" "0.0" "0.0")))
      (ok (ty:stockprice-equals actual expected)))))

(defvar march-28 (janitor/common:date 2025 3 28))
(defvar march-21 (janitor/common:date 2025 3 21))

(deftest test-parser
  (testing "Parse stockprices"
    (clet*
      (parser::*feed* test-feed
       items (parser:parse "YAR"))
      (clet*
        (tm (date 2025 3 21)
         result (parser:cut-off items tm)
         lx (length result))
        (ok (= 6 lx))
        (ok (timestamp= march-28 (ty:s-dx (aref result 0))))
        (ok (timestamp= march-21 (ty:s-dx (aref result (1- lx))))))
      (clet*
        (tm (date 2025 3 28)
         result (parser:cut-off items tm)
         lx (length result))
        (ok (= 1 lx))
        (ok (timestamp= march-28 (ty:s-dx (aref result 0)))))
      (clet*
        (tm (date 2025 3 30)
        result (parser:cut-off items tm))
        (ok (= 0 (length result)))))))
