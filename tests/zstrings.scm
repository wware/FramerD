;;; Testing non-determinism

(load-once (get-component "test-util.scm"))
(start-test "zstrings.scm")

(define-if-needed zstrings {})
(define first-run #f)

(define a-code (char->integer #\A))

(define (random-string)
  (with-string-output
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))
   (write-char (integer->char (+ a-code (random 26))))))

(define (init-zstrings-test)
  (dotimes (i 500)
    (set+! zstrings (zstring (random-string)))))

(define (do-zstrings-test)
  (let ((success #t))
    (do-choices (zs zstrings)
      (unless (eq? zs (zstring (stringout zs)))
	(lineout "Lost zstring " (readably zs))
	(set! success #f)))
    success))

(define (do-zstrings-cleanup-test)
  (let ((before (memusage)))
    (set! zstrings {})
    (zero? (- (memusage) before))))

(when (empty? zstrings)
  (init-zstrings-test)
  (set! first-run #t))

(testing 'zstring-bug '(do-zstrings-test) #t)
;(unless first-run
;  (testing 'zstring-cleanup '(do-zstrings-cleanup-test) #t))

(report-problems)

