(defpackage janitor/common
  (:use :cl)
  (:export
    #:clet
    #:clet*
    #:read-csv
    #:timestamp
    #:fn
    #:partial))

(in-package :janitor/common)

(defun timestamp (year month day)
  (local-time:encode-timestamp 0 0 0 0 day month year))

(defmacro fn (&rest forms)
  `(lambda ,@forms))

(defmacro clet (bindings &body body)
  `(let ,(loop for (a b) on bindings by #'cddr collect (list a b))
     ,@body))

(defmacro clet* (bindings &body body)
  `(let* ,(loop for (a b) on bindings by #'cddr collect (list a b))
     ,@body))

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
