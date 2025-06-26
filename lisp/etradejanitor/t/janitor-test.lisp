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
    #:float-equals-p
    #:*home*)
  (:local-nicknames
    (#:ma #:janitor/main)
    (#:ya #:janitor/yahoo)
    (#:mig #:janitor/migrations)
    (#:sprice #:janitor/stockmarket/stockprice)
    (#:parser #:janitor/parser)))

(in-package :etradejanitor/tests/janitor-test)

;(defvar test-feed

(defparameter test-feed
  (format nil "~a/lisp/etradejanitor/t/resources" *home*))

(defparameter test-feed-mig
  (format nil "~a/lisp/etradejanitor/t/resources/migrations" *home*))

(defparameter test-feed-feedstatus
  (format nil "~a/lisp/etradejanitor/t/resources/feedstatus" *home*))

(defvar expected
  (sprice::make-stockprice
    :ticker 3
    :dx (date 2025 3 28)
    :opn 317.1
    :hi 319.3
    :lo 311.9
    :cls 314.2
    :vol 497342))

(defvar dec-2024-30 (date 2024 12 30))
(defvar jan-1 (date 2025 1 1))
(defvar jan-2 (date 2025 1 2))
(defvar jan-4 (date 2025 1 4))
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

(defun test-mig-path (unix-time comment suffix)
  (let ((p (pathname (format nil "~a/~a__~a.sql" test-feed-mig unix-time suffix))))
    (list :unix unix-time :comment comment :sql p)))
        

(deftest test-migrations
  (testing "migrations"
    (let* ((mig::*feed* test-feed-mig)
           (result (mig:get-migrations 1746602800)))
      (ok (= 2 (hash-table-count result)))
      (let ((p1 (gethash 1746702000 result)))
        (ok (equal (test-mig-path 1746702000 "The next one" "the_next_one") p1)))))
  (testing "migrations edge case"
    (let* ((mig::*feed* test-feed-mig)
           (result (mig:get-migrations 1746702000)))
      (ok (= 1 (hash-table-count result)))
      (let ((p2 (gethash 1746902800 result)))
        (ok (equal (test-mig-path 1746902800 "The last one" "the_last_one") p2)))))
  (testing "migrations sorted keys"
    (let* ((mig::*feed* test-feed-mig)
           (mig-ht (mig:get-migrations 1745000000))
           (keys (mig::get-migrations-keys mig-ht)))
      (ok (equal (list 1746502000 1746702000 1746902800) keys))))
  (testing "migrations no sql files left"
    (let* ((mig::*feed* test-feed-mig)
           (mig-ht (mig:get-migrations 1746903000))
           (keys (mig::get-migrations-keys mig-ht)))
      (ok (= 0 (hash-table-count mig-ht)))
      (ok (equal nil keys)))))

(deftest test-yahoo
  (testing "yahoo-period"
    (ok (equal (ya:yahoo-period jan-1 :end-date dec-2024-30) "na"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-1) "1d"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-2) "5d"))
    (ok (equal (ya:yahoo-period jan-1 :end-date jan-4) "5d"))
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
    (ok (null (between 0 1.0 0 :begin-open t)))
    (ok (null (between 0 1.0 1.0)))
    (ok (equal (between 0 1.0 1.0 :end-closed t) t))))

(deftest test-types
  (testing "Stockprice"
    (clet
      (actual
        (sprice:mk-stockprice 3
          '("2025-03-28 00:00:00+00:00" "317.1000061035156" "319.29998779296875" "311.8999938964844" "314.20001220703125" "497342" "0.0" "0.0")))
      (ok (sprice:stockprice-equals actual expected)))))

(deftest test-parser
  (testing "Parse stockprices"
    (clet*
      (parser::*feed* test-feed
       items (parser:parse "YAR"))
      (clet*
        (tm (date 2024 12 24)
          result (parser:cut-off items 3 tm)
          lx (length result))
        (ok (= 62 lx)))
      (clet*
        (tm (date 2025 3 21)
         result (parser:cut-off items 3 tm)
         lx (length result))
        (ok (= 6 lx))
        (ok (timestamp= march-28 (sprice:s-dx (aref result 0))))
        (ok (timestamp= march-21 (sprice:s-dx (aref result (1- lx))))))
      (clet*
        (tm (date 2025 3 28)
         result (parser:cut-off items 3 tm)
         lx (length result))
        (ok (= 1 lx))
        (ok (timestamp= march-28 (sprice:s-dx (aref result 0)))))
      (clet*
        (tm (date 2025 3 30)
        result (parser:cut-off items 3 tm))
        (ok (= 0 (length result))))))
  (testing "Parse no stockprices"
    (clet*
      (parser::*feed* test-feed
       items (parser:parse "YARX"))
      (ok (null items)))))

(defun create-tdx (ts)
  (clet
    (ht (make-hash-table :test 'equal)
    pts (parse-timestring ts))
    (setf (gethash "YAR" ht) pts)
    (setf (gethash "ORK" ht) pts)
    ht))

(defun create-tdx-nhy (ts)
  (clet
    (ht (make-hash-table :test 'equal)
    pts (parse-timestring ts))
    (setf (gethash "NHY" ht) pts)
    ht))

(deftest test-parser-skip-today
  (testing "Parse stockprices skipping today"
    (let* ((parser::*feed* test-feed)
           (tdx (create-tdx-nhy "2025-01-08"))
           (result (ma::parse-ticker "NHY" tdx :skip-today t)))
      (ok (equal (getf result :status) :ok))
      (ok (equal (length (getf result :result)) 2)))))

(deftest test-main
  (testing "process-ticker"
    (clet*
      (parser::*feed* test-feed)
      (clet*
        (tdx (create-tdx "2024-12-24")
          result-yarx (ma::parse-ticker "YARX" tdx)
          result-ork (ma::parse-ticker "ORK" tdx))
        (ok (equal (list :status :empty-dx) result-yarx))
        (ok (equal (list :status :missing-csv) result-ork)))
      (clet*
        (tdx-1 (create-tdx-nhy "2025-01-12")
          result-1 (ma::parse-ticker "NHY" tdx-1))
        (ok (equal (getf result-1 :status) :empty-cutoffs)))
      (clet*
        (tdx-2 (create-tdx-nhy "2025-01-08")
          result-2 (ma::parse-ticker "NHY" tdx-2))
        (ok (equal (getf result-2 :status) :ok))
        (ok (equal (length (getf result-2 :result)) 3)))
      (clet*
        (tdx-3 (create-tdx-nhy "2024-12-30")
          result-3 (ma::parse-ticker "NHY" tdx-3))
        (ok (equal (getf result-3 :status) :start-date-error))))))

(defun check-feed-status (ticker csv-result num-lines)
  (let ((result (first (ma::feed-status ticker))))
    (ok (equal (getf result :csv) csv-result))
    (ok (= (getf result :lines) num-lines))))

(deftest test-feed-status
  (let ((parser::*feed* test-feed-feedstatus))
    (testing "NHY"
      (check-feed-status "NHY" :ok 2))
    (testing "YAR"
      (check-feed-status "YAR" :ok 8))
    (testing "BAKKA"
      (check-feed-status "BAKKA" :incomplete 0))
    (testing "STB"
      (check-feed-status "STB" :incomplete 1))
    (testing "AKSO"
      (check-feed-status "AKSO" :missing 0))))
        
        

