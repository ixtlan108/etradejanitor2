(defpackage janitor/main
  (:use :cl)
  (:import-from :janitor/common #:clet #:clet*))

  ;(:local-nicknames
  ;  (#:lim #:photo-limbo)
  ;  (#:sql #:photo-sql)))

(in-package :janitor/main)

(defvar *gpsx-f* #P"underlag/gpsx.csv")

; (defun write-sql (items sql-file)
;   (with-open-file (output sql-file ;"sql/photos.sql"
;                     :direction         :output
;                     :if-does-not-exist :create
;                     :if-exists         :supersede)
;     (dolist (item items)
;       (format output "~a~%" item))))

; (defun sql ()
;   (let* ((photos (lim:bundle-hits))
;          (result (sql:make-sql photos *gpsx-f*))
;          (photos (getf result :photo))
;          (gpsx (getf result :gpsx)))
;     (write-sql photos "sql/photos.sql")
;     (write-sql gpsx "sql/gpsx.sql")))
