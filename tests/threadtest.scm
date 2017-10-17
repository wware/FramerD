;;; Tests threading existence and support

(load-component "test-util.scm")
(start-test "threadtest.scm")
(define threading #f)

;; update-state sets
(define state 0)
(define (update-state n)
  ;; (lineout "STATE=" state " N=" n)
  (if (> state n)
      (set! threading #t)
      (set! state n))) 
(define (countup from to)
  (dotimes (i (- to from))
    (printout (+ from i) " ")
    (update-state (+ from i))
    (sleep (/ (random 5) 100)))
  (lineout))

(lineout "Counting up sequentially")
(begin (countup 0 10) (countup 10 20)) 
(set! state 0)
(lineout "Counting up in parallel")
(parallel (countup 0 10) (countup 10 20)) 
(if threading
    (lineout "Threading seems to work")
    (begin (report-problem 'threadtest "Threading doesn't seem to work")
	   (lineout "Threading doesn't seem to work"))) 


(define buggy-numbers '())
(define numbers '())
(define snumbers '())
(define numbers-lock (make-mutex))

(define spush-onto-numbers
  (slambda (n)
    (let ((cur snumbers))
      (sleep (/ (random 5) 100))
      (set! snumbers (cons n cur)))
    (sleep (/ (random 5) 100))))
(sdefine (spush-onto-numbers n)
  ;; (lineout "NUMBERS=" buggy-numbers)
  (let ((cur snumbers))
    (sleep (/ (random 5) 100))
    (set! snumbers (cons n cur)))
  (sleep (/ (random 5) 100)))
(define (push-onto-numbers n)
  ;; (lineout "NUMBERS=" buggy-numbers)
  (with-mutex-locked numbers-lock
    (let ((cur numbers))
      (sleep (/ (random 5) 100))
      (set! numbers (cons n cur))))
  (sleep (/ (random 5) 100)))
(define (buggy-push-onto-numbers n)
  ;; (lineout "BUGGY-NUMBERS=" buggy-numbers)
  (let ((cur buggy-numbers))
    (sleep (/ (random 5) 100))
    (set! buggy-numbers (cons n cur)))
  (sleep (/ (random 5) 100)))
(define (test-mutex-need)
  (parallel (dotimes (i 10) (buggy-push-onto-numbers (* i 2)))
	    (dotimes (i 10) (buggy-push-onto-numbers (+ 1 (* i 2)))))
  (if (= (length buggy-numbers) 20)
      (report-problem 'test-mutexes-test
		      "Threading isn't screwing up shared variables (problem)")
      (lineout "Shared variables get messed up without mutexes (expected)")))
(define (test-mutexes)
  (parallel (dotimes (i 10) (push-onto-numbers (* i 2)))
	    (dotimes (i 10) (push-onto-numbers (+ 1 (* i 2)))))
  (if (= (length numbers) 20)
      (lineout "Mutexes seem to work")
      (report-problem 'test-mutexes
		      "Mutexes didn't seem protect shared variable NUMBERS")))
(define (test-slambda)
  (parallel (dotimes (i 10) (spush-onto-numbers (* i 2)))
	    (dotimes (i 10) (spush-onto-numbers (+ 1 (* i 2)))))
  (if (= (length snumbers) 20)
      (lineout "Synchronized lambdas seem to work")
      (report-problem 'test-slambda
	 "Synchronized lambdas didn't protect shared variable SNUMBERS")))

(define (iterative-summer total from to)
  (if (= from to) (+ total from)
      (iterative-summer (+ total from) (+ from 1) to)))
(define (iterative-sum from to)
  (do ((i from (1+ i))
       (sum 0 (+ sum i)))
      ((>= i to) sum)))
(define (recursive-iterative-sum from to)
  (let summer ((total 0) (from from) (to to))
    (if (= from to) (+ total from)
	(summer (+ total from) (+ from 1) to))))
(define (clockit expr)
  (let ((start (timestamp)))
    (eval expr)
    (let ((end (timestamp)))
      (timestamp-diff end start))))

(define (estimate-smp-gain)
  (let ((time1 (clockit '(choice (iterative-sum 0 50000)
				 (iterative-sum 0 60000))))
	(time2 (clockit '(parallel (iterative-sum 0 50000)
				   (iterative-sum 0 60000)))))
    (warn "=== SMP gain appears to be " (/ time1 time2))))
(define (test-mpcall)
  (mpcall iterative-sum 0 (choice 5000 10000)))

(when threading
  (lineout "Checking if mutexes are needed")
  (test-mutex-need)
  (lineout "Checking if mutexes work")
  (test-mutexes)
  (lineout "Checking synchronized lambdas")
  (test-slambda)
  (lineout "Estimating SMP gain")
  (estimate-smp-gain)
  (lineout "Testing MPCALL")
  (if (identical? (test-mpcall) {12497500 49995000})
      (lineout "MPCALL seems to work")
      (report-problem 'mpcall-failed)))

(clear-env-changes!)
