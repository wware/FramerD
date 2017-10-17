;;; Testing for indices

(load-once (get-component "test-util.scm"))
(use-module! fdmaint)
(use-module! fdinternals)

(define first-time #t)

(if (file-exists? "test.index")
    (set! first-time #f)
    (make-file-index "test.index" 60000))
(define test-table (open-index "test.index"))

(if first-time
    (start-test "indextest.scm (1)")
    (start-test "indextest.scm (2)"))

(define n-symbols 0)

(define (build-table)
  (do-choices (sym (all-symbols)) 
    (set! n-symbols (+ n-symbols 1))
    (let* ((name (symbol->string sym))
	   (nchars (length name))
	   (class (remainder (length name) 3)))
      (index-add! test-table nchars sym)
      (index-add! test-table (/ 1 (* 2 nchars)) name)
      (cond ((= class 0) (index-set! test-table sym name))
	    ((= class 1) (index-set! test-table (cons name nchars) sym))
	    ((= class 2)
	     (index-set! test-table
			 (vector name nchars) (cons name
						    nchars))))))
  (commit-index test-table)
  (swap-out-index test-table))

(define (build-table2)
  (do-choices (sym (all-symbols))
    (when (zero? (random 2))
      (set! n-symbols (+ n-symbols 1))
      (let* ((name (symbol->string sym))
	     (nchars (length name))
	     (class (remainder (length name) 3)))
	(index-add! test-table nchars sym)
	(index-add! test-table (/ 1 (* 2 nchars)) name)
	(cond ((= class 0) (index-set! test-table sym name))
	      ((= class 1) (index-set! test-table (cons name nchars) sym))
	      ((= class 2)
	       (index-set! test-table
			   (vector name nchars) (cons name
						      nchars)))))))
  (commit-index test-table)
  (swap-out-index test-table)
  (do-choices (sym (all-symbols))
    (when (zero? (random 2))
      (set! n-symbols (+ n-symbols 1))
      (let* ((name (symbol->string sym))
	     (nchars (length name))
	     (class (remainder (length name) 3)))
	(index-add! test-table nchars sym)
	(index-add! test-table (/ 1 (* 2 nchars)) name)
	(cond ((= class 0) (index-set! test-table sym name))
	      ((= class 1) (index-set! test-table (cons name nchars) sym))
	      ((= class 2)
	       (index-set! test-table
			   (vector name nchars) (cons name
						      nchars)))))))
  (commit-index test-table)
  (swap-out-index test-table))

(define (check-numeric-key i)
  (let ((elts1 (index-get test-table i))
	(elts2 (index-get test-table (/ 1 (* i 2)))))
    (do-choices (elt elts1)
      (unless (and (symbol? elt) (= (length (symbol->string elt)) i))
	(lineout "Error for " elt)))
    (do-choices (elt elts2)
      (unless (and (string? elt) (= (length elt) i))
	(lineout "Error for " elt)))))

(define (check-numeric-keys)
  (dotimes (i 25)
    (when (> i 0) (check-numeric-key i))))

(define (check-symbolic-keys)
  (let ((elts (index-keys test-table)))
    (do-choices (sym elts)
      (when (symbol? sym)
	(let* ((name (symbol->string sym)) (nchars (length name)))
	  (unless (or (and (= 0 (remainder nchars 3))
			   (equal? (index-get test-table sym) name))
		    (and (= 1 (remainder nchars 3))
			 (eq? (index-get test-table (cons name nchars))
			      symbol))
		    (and (= 1 (remainder nchars 3))
			 (equal? (index-get test-table (vector name nchars))
				 (cons name nchars))))
	    (lineout "Problem with " sym)))))))


(if first-time
    (begin (build-table)
	   (check-numeric-keys) (check-symbolic-keys)
	   (swap-out-index test-table)
	   (close-index test-table))
    (begin (check-numeric-keys) (check-symbolic-keys)
	   (swap-out-index test-table)
	   (close-index test-table)
	   (remove-file "test.index")))

(clear-env-changes!)
(report-problems)
