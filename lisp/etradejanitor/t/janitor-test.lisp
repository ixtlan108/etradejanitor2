(defpackage etradejanitor/tests/janitor-test
  (:use :cl
        :rove)
  (:import-from :local-time #:parse-timestring #:timestamp=)
  (:import-from :janitor/common
    #:clet
    #:clet*
    #:date
    #:iso-8601-string
    #:between
    #:float-equals-p)
  (:local-nicknames
    (#:ya #:janitor/yahoo)
    (#:ty #:janitor/types)
    (#:parser #:janitor/parser)))

(in-package :etradejanitor/tests/janitor-test)

; company portal
;
;(defparameter test-feed "/home/rcs/opt/etradejanitor2/lisp/etradejanitor/t/resources")

(defvar test-feed
  (uiop:native-namestring "~/opt/etradejanitor2/lisp/etradejanitor/t/resources"))


(defvar expected
  (ty::make-stockprice
    :dx (date 2025 3 28)
    :opn 317.1
    :hi 319.3
    :lo 311.9
    :cls 314.2
    :vol 497342))

(defvar jan-1 (date 2025 1 1))
(defvar jan-2 (date 2025 1 2))
(defvar jan-5 (date 2025 1 5))
(defvar jan-6 (date 2025 1 6))
(defvar jan-7 (date 2025 1 7))
(defvar jan-29 (date 2025 1 29))
(defvar feb-1 (date 2025 2 1))
(defvar apr-1 (date 2025 4 1))
(defvar oct-1 (date 2025 10 1))
(defvar oct-30 (date 2025 10 30))
(defvar march-21 (date 2025 3 21))
(defvar march-28 (date 2025 3 28))
(defvar dec-31 (date 2025 12 31))
(defvar jan-2026-1 (date 2026 1 1))
(defvar march-2026-30 (date 2026 3 30))
(defvar aug-2027-2 (date 2027 8 2))

(deftest test-yahoo
  (testing "yahoo-period"
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-1) "na"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-2) "1d"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-5) "5d"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-6) "1mo"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-7) "1mo"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-29) "1mo"))
    (ok (equal (ya:yahoo-period jan-1 :end-date feb-1) "3mo"))
    (ok (equal (ya:yahoo-period jan-1 :end-date apr-1) "6mo"))
    (ok (equal (ya:yahoo-period jan-1 :end-date dec-31) "1y"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-2026-1) "2y"))
    (ok (equal (ya:yahoo-period jan-1 :end-date march-2026-30) "2y"))
    (ok (equal (ya:yahoo-period jan-1 :end-date aug-2027-2) "5y"))))

(deftest test-common
  (testing "iso-date-string"
    (ok (equal (iso-8601-string jan-1) "2025-01-01"))
    (ok (equal (iso-8601-string march-21) "2025-03-21"))
    (ok (equal (iso-8601-string oct-1) "2025-10-01"))
    (ok (equal (iso-8601-string oct-30) "2025-10-30")))
  (testing "float-equals-p"
    (ok (float-equals-p 0.1 0.1))
    (ng (float-equals-p 0.1 0.01)))
  (testing "between"
    (ok (equal (between 0 1.0 0) t))
    (ok (null (between 0 1.0 0 :open-end t)))
    (ok (null (between 0 1.0 1.0)))
    (ok (equal (between 0 1.0 1.0 :closed-end t) t))))

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
        (ok (= 0 (length result))))))
  (testing "Parse no stockprices"
    (clet*
      (parser::*feed* test-feed
       items (parser:parse "YARX"))
      (print items)
      (ok (null items)))))
