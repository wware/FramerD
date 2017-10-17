;;; Testing non-determinism

(load-once (get-component "test-util.scm"))
(start-test "qstrings.scm")

(define-if-needed qstrings {})

(define a-code (char->integer #\A))

(define (random-string)
  (with-string-output
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))))

(define (init-qstrings-test)
  (dotimes (i 500)
    (set+! qstrings (qstring (random-string)))))

(define (do-qstrings-test)
  (let ((success #t))
    (do-choices (qs qstrings)
      (unless (eq? qs (qstring (stringout qs)))
	(lineout "Lost qstring " (readably qs))
	(set! success #f)))
    success))

(define (do-qstrings-cleanup-test)
  (let ((before (memusage)))
    (set! qstrings {})
    (zero? (- (memusage) before))))

(when (empty? qstrings)
  (init-qstrings-test))

(testing 'qstring-bug '(do-qstrings-test) #t)
;(unless first-run
;  (testing 'qstring-cleanup '(do-qstrings-cleanup-test) #t))

(report-problems)


