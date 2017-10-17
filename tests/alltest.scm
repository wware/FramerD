;;; -*- Mode: fdscript; -*-

;;; Loader file for various FramerD tests
;;; $Id: alltest.scm,v 1.14 2003/08/27 10:53:30 haase Exp $

(define test-batch-mode #t)
(set-default-encoding! "us-ascii") ; Really test the procedures below.
(load-component "test-util.scm")
(when (file-exists? (get-component "alltest.running"))
  (remove-file (get-component "alltest.running"))
  (display (session-id) (open-output-file (get-component "alltest.running"))))
(load-test (get-component "r4rstest.scm"))
(load-test (get-component "poison.scm"))
(load-test (get-component "ambtest.scm"))
(load-test (get-component "dtypetest.scm"))
(load-test (get-component "glambda.scm"))
(load-test (get-component "sorted.scm"))
(when (or (bound? tx-matcher) (not (signals-error? (load-dll "fdtext"))))
  (load-test (get-component "text-test.scm")))
(load-test (get-component "stringio.scm"))
(load-test (get-component "qstrings.scm"))
(load-test (get-component "zstrings.scm"))
(load-test (get-component "unicode-test.scm") "iso-latin1")
(if (file-exists? "../etc/encodings/BIG5")
    (load-test (get-component "i18n-test.scm")))
(load-test (get-component "utf8-test.scm"))
(load-test (get-component "misctest.scm"))
(load-test (get-component "seqtest.scm"))
(load-test (get-component "hashtest.scm"))
(load-test (get-component "indextest.scm"))
(when (and (not (getenv "NOTHREADS")) (bound? spawn))
  (load-test (get-component "threadtest.scm")))
(load-test (get-component "dbtest.scm"))
(define (main . args)
  (report-problems)
  (notify " === Finished running alltest.scm"))
(remove-file (get-component "alltest.running"))
(notify " === Finished loading alltest.scm")

;;; $Log: alltest.scm,v $
;;; Revision 1.14  2003/08/27 10:53:30  haase
;;; Merged 2.4 patches into trunk, started 2.5
;;;
;;; Revision 1.13.2.2  2003/01/26 20:51:06  haase
;;; Various test fixes
;;;
;;; Revision 1.13.2.1  2002/08/12 18:53:41  haase
;;; Added qstring test
;;;
;;; Revision 1.13  2002/04/03 13:03:02  haase
;;; Made simple-test target report directly if its test failed
;;;

