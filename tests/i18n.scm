;;; Test unicode strings and symbols

(load-once "test-util.scm")
(start-test "i18n.scm")

(define coding-pattern
  '(IC #("charset=" (label encoding (chunk (* {(isalnum+) "-"}))))))

(define (read-test-file filename)
  (lineout "Reading " filename)
  (let* ((string (filestring filename "latin-1"))
	 (encoding (get (match->frame #f coding-pattern (tx-gather coding-pattern string))
			'encoding))
	 (realstring (filestring filename encoding)))
    (list filename encoding realstring (parse-html realstring))))

(clear-env-changes!)

(report-problems)




