(defpackage etradejanitor/tests/janitor-test
  (:use :cl
        :rove)
  (:import-from :local-time #:parse-timestring)
  (:import-from :janitor/common #:clet #:clet* #:date)
  ;(:import-from #:janitor/types #sp-dx))
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

(deftest test-parser
  (testing "Parse stockprices"
    (clet*
      (parser::*feed* test-feed
       tm (date 2025 3 21)
       result (parser::parse "YAR" tm))
      (ok (= 6 (length result))))))
