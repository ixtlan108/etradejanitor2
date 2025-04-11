(defpackage janitor/parser
  (:use :cl)
  (:local-nicknames
    (#:com #:janitor/common)
    (#:ty #:janitor/types))
  (:import-from :local-time #:timestamp<)
  (:import-from :janitor/common
    #:clet
    #:clet* #:fn
    #:*home*)
  (:export
    #:parse
    #:cut-off
    #:parse-cut-off))

(in-package :janitor/parser)

(defparameter *feed*
  (format nil "~a/feed" *home*))

(defun feed-csv-name (ticker)
  (format nil "~a/~a.csv" *feed* ticker))

; Date,Open,High,Low,Close,Volume,Dividends,Stock Splits
; 2025-01-02 00:00:00+01:00,301.1000061035156,305.5,301.1000061035156,304.8999938964844,494966,0.0,0.0
; (2025-01-02 00:00:00+01:00,301.1000061035156,305.5,301.1000061035156,304.8999938964844,494966,0.0,0.0)
; ("2025-03-28 00:00:00+01:00" "317.1000061035156" "319.29998779296875" "311.8999938964844" "314.20001220703125" "497342" "0.0" "0.0")

(defun mk-stockprice-fn (ticker-oid cut-off-date)
  (clet (hit nil)
    (fn (row)
      (clet (price (ty:mk-stockprice ticker-oid row))
        (if hit
          nil
          (clet (cur-dx (ty:s-dx price))
            (if (timestamp< cur-dx cut-off-date)
              (progn
                (setq hit t)
                nil)
              price)))))))

(defun cut-off (items ticker-oid cut-off-date)
  (clet (fx (mk-stockprice-fn ticker-oid cut-off-date))
    (remove-if #'null (map 'vector fx items))))

(defun parse (ticker)
  (clet (csv-name (feed-csv-name ticker))
    (if (uiop:file-exists-p csv-name)
      (nreverse (com:read-csv csv-name))
      nil)))

(defun parse-cut-off (ticker cut-off-date)
  (cut-off (parse ticker) 1 cut-off-date))

; (defun parse (ticker cut-off-date)
;   (clet (items (nreverse (com:read-csv (feed-csv-name ticker))))
;     (cut-off-items items cut-off-date)))

; (defvar ixx
;   (let ((counter 0))
;     (lambda ()
;       (setq counter (+ counter 1))
;       counter)))
; (funcall ixx)

;(com:read-csv (feed-csv-name ticker) :keep-header t))

; 22.1.4. Standard Dispatching Macro Character Syntax
; The standard syntax includes forms introduced by the # character. These take the general form of a #, a second character that identifies the syntax, and following arguments in some form. If the second character is a letter, then case is not important; #O and #o are considered to be equivalent, for example.

; Certain # forms allow an unsigned decimal number to appear between the # and the second character; some other forms even require it. Those forms that do not explicitly permit such a number to appear forbid it.

; ----------------------------------------------------------------
; Table 22-4: Standard # Macro Character Syntax

; #!  undefined *                #<backspace>  signals error
; #"  undefined                  #<tab>        signals error
; ##  reference to #= label      #<newline>    signals error
; #$  undefined                  #<linefeed>   signals error
; #%  undefined                  #<page>       signals error
; #&  undefined                  #<return>     signals error
; #'  function abbreviation      #<space>      signals error
; #(  simple vector              #+      read-time conditional
; #)  signals error              #-      read-time conditional
; #*  bit-vector                 #.      read-time evaluation
; #,  load-time evaluation       #/      undefined
; #0  used for infix arguments   #A, #a  array
; #1  used for infix arguments   #B, #b  binary rational
; #2  used for infix arguments   #C, #c  complex number
; #3  used for infix arguments   #D, #d  undefined
; #4  used for infix arguments   #E, #e  undefined
; #5  used for infix arguments   #F, #f  undefined
; #6  used for infix arguments   #G, #g  undefined
; #7  used for infix arguments   #H, #h  undefined
; #8  used for infix arguments   #I, #i  undefined
; #9  used for infix arguments   #J, #j  undefined
; #:  uninterned symbol          #K, #k  undefined
; #;  undefined                  #L, #l  undefined
; #<  signals error              #M, #m  undefined
; #=  label following object     #N, #n  undefined
; #>  undefined                  #O, #o  octal rational
; #?  undefined *                #P, #p  pathname
; #@  undefined                  #Q, #q  undefined
; #[  undefined *                #R, #r  radix-n rational
; #\  character object           #S, #s  structure
; #]  undefined *                #T, #t  undefined
; #^  undefined                  #U, #u  undefined
; #_  undefined                  #V, #v  undefined
; #`  undefined                  #W, #w  undefined
; #{  undefined *                #X, #x  hexadecimal rational
; #|  balanced comment           #Y, #y  undefined
; #}  undefined *                #Z, #z  undefined
; #~  undefined                  #<rubout> undefined
