;;; Initialization for each test file.

(set-default-encoding! 'latin1)

(set-notify! #t)

(define test-filename "unknown")

(unless (bound? problems)
  (define problems '())
  (define initial-problems 0))

(define (start-test filename)
  (set! test-filename filename)
  (set! initial-problems (length problems)))

(define (report-problem test . args)
  (set! problems (cons (list* test-filename test args) problems))
  (printout-apply "\n!!! " (timestring) " === (" test-filename ": " test ") " args)
  (printout "\n"))

(define (report-problems)
  (if (= (length problems) initial-problems)
      (notify "=== No problems with tests in " test-filename)
      (notify "=== " (- (length problems) initial-problems)
	      " problems with tests in " test-filename))
  (dolist (problem problems)
    (printout-apply "\n" "(" (car problem) ": " (cadr problem) ") "
		    (cddr problem)))
  (lineout))

(define (testing test expr . l)
  (let ((got (eval expr)) (expected (elts l)))
    (if (and (= (set-size expected) (set-size got))
	     (if (= (set-size got) 1)
		 (equal? got expected)
	       (empty? (difference got expected))))
	(let ((as-string (stringout got)))
	  (if (> (length as-string) 20)
	      (begin (lineout "Passed test of " test ": "  expr)
		     (pprint got) (lineout))
	      (lineout "Passed test of " test ": "  expr " ==> " as-string)))
	(begin (report-problem test expr " returned " got
			       " but should have returned " expected)
	       (lineout "Failed test " test ": "  expr)
	       (printout "Expected:") (pprint expected) (lineout)
	       (printout "Got:     ") (pprint got) (lineout))))) 

(define (load-test filename (enc #f))
  (let ((before (memusage))
	(after (memusage))
	(beforec (consusage))
	(afterc (consusage)))
    (lineout "Loading " filename)
    (load filename enc)
    (set! before (memusage))
    (set! beforec (consusage))
    (lineout "Reloading " filename)
    (load filename enc)
    (set! after (memusage))
    (set! afterc (consusage))
    (notify "=== Reloading " filename " used up " (- after before) " extra bytes; m=" (* (or (ru-data-size) 0) 512) "/t=" (* (or (ru-runtime) 0) .000001))))








 




