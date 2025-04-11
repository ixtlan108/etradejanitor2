(defpackage janitor/common
  (:use :cl)
  (:import-from :local-time
    #:timestamp-day
    #:timestamp-month
    #:format-timestring
    #:timestamp-to-unix)
  (:export
    #:clet
    #:clet*
    #:read-csv
    #:date
    #:iso-8601-string
    #:diff-days
    #:between
    #:float-equals-p
    #:print-hash
    #:fn
    #:>0
    #:vec-last
    #:partial
    #:gethx
    #:*home*))

;(:import-from :trivia #:match)

(in-package :janitor/common)

(defparameter *home*
  (uiop:native-namestring "~/opt/etradejanitor2"))
  ;(uiop:native-namestring "~/Projects/lisp/etradejanitor2"))

(defun print-hash-entry (key value)
  (format t "~S => ~S~%"
            key value))

(defun print-hash (ht)
  (maphash #'print-hash-entry ht))

(defun >0 (items)
  (if (null items)
    nil
    (> (length items) 0)))

(defun vec-last (v)
  (when (>0 v)
    (elt v (- (length v) 1))))

(defun gethx (ht key)
  (gethash key ht))

(defmacro fn (&rest forms)
  `(lambda ,@forms))

(defmacro clet (bindings &body body)
  `(let ,(loop for (a b) on bindings by #'cddr collect (list a b))
     ,@body))

(defmacro clet* (bindings &body body)
  `(let* ,(loop for (a b) on bindings by #'cddr collect (list a b))
     ,@body))


(defun date (year month day)
  (local-time:encode-timestamp 0 0 0 0 day month year :timezone local-time:+utc-zone+))

(defun iso-8601-string (dt)
  (clet* (d (timestamp-day dt)
         m (timestamp-month dt)
         my-format
          (cond
            ((and (< m 10) (< d 10)) '(:year "-0" :month "-0" :day))
            ((and (< m 10) (>= d 10)) '(:year "-0" :month "-" :day))
            ((and (>= m 10) (< d 10)) '(:year "-" :month "-0" :day))
            (t                        '(:year "-" :month "-" :day))))
    (format-timestring nil dt :format my-format)))

(defconstant +seconds-in-day+ 86400)

(defun diff-days (from-date to-date)
  (clet*
    (ttu-from (timestamp-to-unix from-date)
    ttu-to (timestamp-to-unix to-date)
    diff (- ttu-to ttu-from))
  (if (<= diff 0)
    0
    (/ diff +seconds-in-day+))))

(defun between (from-value to-value value &key (begin-open nil) (end-closed nil))
  (clet
    (opn-fn (if begin-open #'< #'<=)
    end-fn (if end-closed #'<= #'<))
    (if
      (and
        (funcall opn-fn from-value value)
        (funcall end-fn value to-value))
      t
      nil)))

(defun float-equals-p (f1 f2 &optional (epsilon 0.0005))
  (declare (type real f1 f2 epsilon))
  (let ((delta (* (abs f1) epsilon)))
    (<= (- f1 delta)
        f2
        (+ f1 delta))))

(defun read-csv (csv-file &key (keep-header nil))
  (clet* (pn (pathname csv-file)
          items (cl-csv:read-csv pn :separator ","))
    (if keep-header
      items
      (rest items))))

(defun partial (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

; (map 'list (partial '+ 2) (list 1 2 3)) ;=> (3 4 5)
;
; (defun mapc-hash (hash-table fun)
;   (loop for key being the hash-keys of hash-table
;         for value being the hash-values of hash-table
;         do (funcall fun key value)))


; (defconstant %pos-args% #(_1 _2 _3 _4 _5 _6 _7 _8 _9))

; (defconstant %rest-arg% '_rest)

; (defmacro partial (fun-expr &rest arg-spec)
;   (multiple-value-bind (pos-args max-pos rest-occurs)
;     (loop for arg in arg-spec
;           for pos = (position arg %pos-args%)
;           with rest-occurs = nil
;           maximizing (or pos 0) into max-pos
;           if pos
;             collect arg into args
;           if (eq arg %rest-arg%)
;             do (setf rest-occurs t)
;           finally (return (values args (or max-pos -1) rest-occurs)))
;     (let ((args (loop for i from 0 to max-pos
;                       for pa across %pos-args%
;                       if (member pa pos-args)
;                         collect pa
;                       else
;                         collect (gensym))))
;       (if (and (not rest-occurs) (null pos-args))
;         (let ((one-arg (gensym)))
;           `(lambda (,one-arg)
;              (,fun-expr ,@arg-spec ,one-arg)))
;         `(lambda (,@args ,@(if rest-occurs `(&rest ,%rest-arg%)))
;            (,fun-expr ,@arg-spec))))))
