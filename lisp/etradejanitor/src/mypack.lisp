(defpackage mypack
  (:use :cl)
  (:local-nicknames
    (#:com #:janitor/common)
    (#:p #:janitor/parser)
    (#:main #:janitor/main)))

; (ql:quickload :photo) (load "mypack.lisp") (in-package :mypack)

(in-package :mypack)



