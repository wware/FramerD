;;; -*- Mode: fdscript -*-

;;; Test unicode strings and symbols

(load-once (get-component "test-util.scm"))
(define test-filename "i18n-test.scm")

(load-encoding (get-component "../etc/encodings/GB2312"))
(load-encoding (get-component "../etc/encodings/BIG5"))
(load-encoding (get-component "../etc/encodings/EUC-JP"))
(load-encoding (get-component "../etc/encodings/EUC-KR"))
(load-encoding (get-component "../etc/encodings/EUC-TW"))
(load-encoding (get-component "../etc/encodings/SHIFT_JIS"))
(load-encoding (get-component "../etc/encodings/KOI8R"))

(define coding-pattern
  '(IC #("charset=" (label encoding (chunk (* {(isalnum+) "-" "_"}))))))

(define (read-test-file filename)
  (lineout "Reading " filename)
  (let* ((string (filestring filename "latin-1"))
	 (encoding (get (match->frame #f coding-pattern (tx-gather coding-pattern string))
			'encoding))
	 (realstring (filestring filename encoding)))
    (list (basename filename) encoding realstring (parse-html realstring))))

(define (write-test-data)
  (write-dtype-to-file (read-test-file (getfiles "i18n"))
		       "i18n.dtype"))

(define data-dir (get-component "i18n"))

(define (run-i18n-tests)
  (do-choices (entry (read-dtype-from-file (get-component "i18n.dtype")))
    (lineout "Reading " (car entry) " in " (cadr entry))
    (let* ((filename (string-append data-dir "/" (car entry)))
	   (string (filestring filename  (cadr entry)))
	   (markup (parse-html string)))
      (unless (and (equal? string (caddr entry))
		   (equal? markup (cadddr entry)))
	(report-problem 'i18n-read (car entry)))
      (write-data string (fopen-encoded (get-component "temp.html") "w" (cadr entry)))
      (unless (equal? (filedata (get-component "temp.html"))
		      (filedata filename))
	(report-problem 'i18n-write (car entry))))))

(define (run-i18n-test filename)
  (do-choices (entry (read-dtype-from-file (get-component "i18n.dtype")))
    (when (equal? filename (car entry))
      (lineout "Reading " (car entry) " using " (cadr entry))
      (let* ((string (filestring (car entry) (cadr entry)))
	     (markup (parse-html string)))
	(unless (and (equal? string (caddr entry))
		     (equal? markup (cadddr entry)))
	  (report-problem 'i18n-read (car entry)))
	(write-data string
		    (fopen-encoded (get-component "temp.html") "w"
				   (cadr entry)))
	(unless (equal? (filedata (get-component "temp.html"))
			(filedata (car entry)))
	  (report-problem 'i18n-write (car entry)))))))

(run-i18n-tests)
(clear-env-changes!)
(report-problems)
