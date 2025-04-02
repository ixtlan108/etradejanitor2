(defpackage janitor/db
  (:use :cl))

(in-package :janitor/db)

(defparameter *db* nil)

(defun my-connect ()
  "Start the database connection."
  (unless *db*
    (print "Connecting to database...")
    (setq *db* t)
    (postmodern:connect-toplevel
      "trader" "trader" "ok" "172.20.1.6" :port 5432)))

(defun my-disconnect()
  (postmodern:disconnect-toplevel))
