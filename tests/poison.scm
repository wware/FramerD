;;;; -*- Scheme -*-

;; These functions should signal errors but have, in the past, dumped core. 
;; This just checks if they really signal errors as they should.  Loading
;; this file should dump core if these errors come back for some reason.

(load-once (get-component "test-util.scm"))
(start-test "poison.scm")

;;; These should signal errors, but we catch them.  Some bugs caused
;;; these to blow the stack by parsing an infinite list.
(testing 'read-from-incomplete-string-1
         '(signals-error? (read-from-string "(+ 2 3"))
	 (return-error
	  '("Unexpected end of file while reading DTYPE" "")))
(testing 'read-from-incomplete-string-2
         '(signals-error? (read-from-string "{2 3 foo"))
	 (return-error
	  '("Unexpected end of file while reading DTYPE" "")))
(testing 'read-from-incomplete-string-3
         '(signals-error? (read-from-string "#(2 3 foo"))
	 (return-error
	  '("Unexpected end of file while reading DTYPE" "")))
(testing 'read-from-incomplete-string-4
	 '(signals-error? (read-from-string "\"Foo"))
	 (return-error
	  '("Unexpected end of file while reading DTYPE" "")))
(testing 'read-unsharp-slotmap
	 '(signals-error? (read-from-string "([1 2])"))
	 #f)

;;; Here we test that tail calls actually get resolved before being
;;;  put into non-deterministic values.

(define (p1 x y) (+ x y))
(define (p2 x y) (p1 x y))

(testing 'nd-tail-call '(p2 3 {4 5}) 7 8)

;; Here we test the sproc procedure functions

(testing 'compound-procedure-arity '(procedure-arity p1) 2)
(testing 'compound-procedure-arguments '(procedure-arguments p1)
	 '(x y))

(sdefine (sp1 x y) (+ x y))
(testing 'synchronized-procedure-arity '(procedure-arity sp1) 2)
(testing 'synchronized-procedure-arguments '(procedure-arguments sp1)
	 '(x y))

(gdefine (gp1 x y) (+ x y))
(testing 'goal-procedure-arity '(procedure-arity gp1) 2)
(testing 'goal-procedure-arguments '(procedure-arguments gp1)
	 '(x y))


(define (lp1 x . y) (+ x (car y)))
(testing 'lexpr-procedure-arity '(procedure-arity lp1) #f)
(testing 'lexpr-procedure-arguments '(procedure-arguments lp1)
	 '(x . y))

(define xa (letrec ((a (lambda (n) (if (= n 0) (begin (display "ok") #t)
				       (begin (display n)
					      (a (- n 1)))))))
	     a))
(testing 'joerg-letrec-bug '(xa 5) #t)

(testing 'non-tail-recursive-stack-overflow
         '(signals-error?
	   (let loop ((n 10000)) (if (= n 0) 'ok (loop (1- n)))))
	 #f)

(testing 'vector-equals-bug
         '(equal? #("foo" "bar") 80)
	 #f)

(define test-vec #(3 4 5))

(testing 'vector-set!-bug
         '(not (signals-error? (vector-set! test-vec 5 8)))
	 #f)

(testing 'negative-string-ref
	 '(not (signals-error? (string-ref "foobar" -1)))
	 #f)
(testing 'negative-vector-ref
	 '(not (signals-error? (vector-ref test-vec -1)))
	 #f)
(testing 'negative-seq-ref-1
	 '(not (signals-error? (elt test-vec -1)))
	 #f)

(testing 'no-args-intersection '(intersection))

(define (bad-function x) (+ x 'y))

(if (and (not (getenv "NOTHREADS")) (bound? spawn))
    (testing 'errors-in-threads
	     '(begin (spawn (bad-function 3)) 3)
	     3))

(lineout "Expect some warnings, this is the UTF-8 stress test...")
(define bad-utf8 (filestring (get-component "i18n/UTF-8-test.txt") "utf-8"))

(clear-env-changes!)
(report-problems)
