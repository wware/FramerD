;;; -*- Mode: fdscript; -*-

;;; Test unicode strings and symbols

(set-encoding! "utf-8")

(define (colon? c) (eqv? c #\:))

(load-once (get-component "test-util.scm"))
(start-test "utf8-test.scm")

(define string-data
  (read-dtype-from-file
   (get-component "i18n/UTF-8-demo.dtype")))
(testing 'utf8-read
	 '(equal? (filestring (get-component "i18n/UTF-8-demo.txt") "utf-8")
		  string-data)
	 #t)
(display (filestring (get-component "i18n/UTF-8-demo.txt") "utf-8")
	 (fopen-encoded (get-component "utf8-tmp.text") "w" "utf8"))
(testing 'utf8-write
	 (equal? (filedata (get-component "utf8-tmp.text"))
		 (filedata (get-component "i18n/UTF-8-demo.txt")))
	 #t)

(clear-env-changes!)

(report-problems)
