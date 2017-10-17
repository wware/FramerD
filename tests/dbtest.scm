;; Testing data base functionality

(load-once (get-component "test-util.scm"))

(define dbroot (string-append (dirname (get-component "dbtest.scm")) "/"))

;; We define this in a variable to avoid an erroneous leak indication
;; caused by the fact that the string used as a pool label persists
;; between the build phase and the use phase.  Putting it in a
;; variable makes it count the same in each run.
(define-if-needed pool-name "dbtest.framerd.org")

(use-module! fdinternals)
(use-module! fdmaint)

(define (get-scheme-files)
  (filter-choices (file (getfiles dbroot))
    (> (length file) 4)
    (has-suffix ".scm" file)
    (not (or (search "/#" file)  (search "/.#" file)))))

(define (remove-if-exists file)
  (when (file-exists? file) (remove-file file)))

(define (cleanup-dbtest-files)
  (lineout "Cleaning up dbtest files")
  (if test-pool (close-pool test-pool))
  (if auto-index (close-index auto-index))
  (if test-index (close-index test-index))
  (remove-if-exists (string-append dbroot "dbtest.pool"))
  (remove-if-exists (string-append dbroot "dbtest.files"))
  (remove-if-exists (string-append dbroot "dbtest.symbols"))
  (remove-if-exists (string-append dbroot "dbtest.index"))
  (remove-if-exists (string-append dbroot "dbtest.autoindex"))
  (remove-if-exists (string-append dbroot "dbtest.running")))

(define test-pool #f)
(define test-index #f)
(define auto-index #f)
(set+! %stop-slots 'context)
(set+! %stop-slots 'obj-name)
(set! %default-expanders #t)

(define (dbtest-init)
  (if (file-exists? (string-append dbroot "dbtest.running"))
      (begin (lineout "Cleaning up from aborted test")
	     (cleanup-dbtest-files)))  
  (write-dtype-to-file (timestring) (string-append dbroot "dbtest.running"))
  (unless (file-exists? (string-append dbroot "dbtest.pool"))
    (make-file-pool (string-append dbroot "dbtest.pool") 4000 @17/0)
    (label-pool! (string-append dbroot "dbtest.pool") pool-name))
  (set! test-pool (use-pool (string-append dbroot "dbtest.pool")))
  (unless (file-exists? (string-append dbroot "dbtest.index"))
    (make-file-index (string-append dbroot "dbtest.index") 250000))
  (use-index (string-append dbroot "dbtest.index"))
  (set! test-index (open-index (string-append dbroot "dbtest.index")))
  (unless (file-exists? (string-append dbroot "dbtest.autoindex"))
    (make-file-index (string-append dbroot "dbtest.autoindex") 250000))
  (use-index (string-append dbroot "dbtest.autoindex"))
  (set! auto-index (open-index (string-append dbroot "dbtest.autoindex")))
  ;; Make sure the indices are cached
  (index-get test-index (string-append dbroot "foo"))
  (index-get auto-index (string-append dbroot "foo"))
  (use-autoindex! auto-index)) 

(define (dbtest-server-init server)
  (set! test-pool (use-pool server))
  (set! test-index (use-index server)))

(define (test-pools)
  (unless (= (pool-capacity test-pool) 4096)
    (report-problem 'pool-capacity "Pool has strange size")) 
  (unless (= (pool-base test-pool) @17/0)
    (report-problem 'pool-base "Pool has strange base")))

(define (get-contents-as-list file)
  (let ((elts '()))
    (call-with-input-file file
      (lambda (stream)
	(let ((input (read stream)))
	  (until (eof-object? input)
		 (set! elts (cons input elts))
		 (set! input (read stream))))
	(reverse elts)))))
(define (get-contents-as-vector file)
  (let ((elts '()))
    (call-with-input-file file
      (lambda (stream)
	(let ((input (read stream)))
	  (until (eof-object? input)
		 (set! elts (cons input elts))
		 (set! input (read stream))))
	(vector (reverse elts))))))
(define (get-contents-as-set file)
  (let ((elts {}))
    (call-with-input-file file
      (lambda (stream)
	(let ((input (read stream)))
	  (until (eof-object? input)
		 (set+! elts input)
		 (set! input (read stream))))
	elts))))

(define (get-expr-atoms x)
  (if (pair? x)
      (amb (get-expr-atoms (car x)) (get-expr-atoms (cdr x)))
      x))
(define (random-atom x)
  (if (pair? x)
      (if (null? (cdr x))
	  (random-atom (car x))
	(try (random-atom (pick-one (amb (car x) (cdr x))))
	     (random-atom (car x)) (random-atom (cdr x))))
    x))

(define (build-db)
  (unless test-pool (dbtest-init))
  (lineout "Building DB")
  (let* ((sym-oid (allocate-oid test-pool))
	 (file-oid (allocate-oid test-pool)))
    (do-choices (file (get-scheme-files))
      (lineout ";;; Analyzing file " file)
      (let ((file-frame
	     (frame-create test-pool
		 'filename file
		 'obj-name file
		 'frame-created
		 (amb (timestring) (get-hour) (get-season))
		 'contents-as-string (filestring file "iso-latin0")
		 'contents-as-list (get-contents-as-list file)
		 'contents-as-vector (get-contents-as-vector file)
		 'contents-as-set (get-contents-as-set file))))
	(lineout ";;; Analyzing contents of " file)
	(do-choices (expr (get-contents-as-set file))
	  (let ((expr-frame
		 (frame-create test-pool
		     'expr expr
		     'frame-created
		     (amb (timestring) (get-hour) (get-season))
		     'in-file file-frame
		     'context file-frame
		     'atoms (get-expr-atoms expr))))
	 
	    (frame-add! file-frame 'contents-as-frames expr-frame)
	    (if (and (pair? expr) (eq? (car expr) 'define))
		(let ((name (if (pair? (cadr expr)) (car (cadr expr))
				(cadr expr))))
		  (frame-add! expr-frame 'defines name)
		  (frame-add! expr-frame 'obj-name (list 'definition name)))
		(frame-add! expr-frame 'obj-name expr))
	    (when test-index
	      (index-frame test-index expr-frame
			   (amb 'frame-created 'in-file 'defines 'expr 'atoms)))
	    ))
	(when test-index
	  (index-frame test-index file-frame
		       (amb 'filename 'frame-created 'contents-as-set
			    'contents-as-frames)))
	))
    (let ((test-files (get-scheme-files))
	  (symbols (all-symbols)))
      (unless (in-pool? sym-oid test-pool)
	(report-problem 'in-pool? "Doesn't claim membership"))
      (set-oid-value! sym-oid symbols)
      (write-dtype-to-file symbols (string-append dbroot "dbtest.symbols"))
      (unless (equal? (get-pool file-oid) test-pool)
	(report-problem 'get-pool "Doesn't get EQ pool"))
      (set-oid-value! file-oid test-files)
      (write-dtype-to-file test-files (string-append dbroot "dbtest.files"))
      (unless (compare-oids file-oid sym-oid)
	(report-problem 'compare-oids "Funny OID order"))
      (unless (eq? (oid-plus sym-oid 1) file-oid)
	(report-problem 'oid-plus "Funny OID increment/compare")))))
  
(define (test-oids)
  (unless test-pool (dbtest-init))
  (unless (oid? (read-from-string "@/dbtest/0"))
    (report-problem 'oid-reading
		    "Pools don't seem to declare read prefixes"))
  (let* ((sym-oid (pool-base test-pool))
	 (file-oid (oid-plus sym-oid 1)))
    (unless (and (symbol? (oid-value sym-oid))
		 (= 1 (set-size (symbol? (oid-value sym-oid)))))
      (report-problem 'symbols-retrieved "Retrieved symbols seem strange"))
    (unless (and (string? (oid-value file-oid))
		 (= 1 (set-size (string? (oid-value file-oid)))))
      (lineout (oid-value file-oid))
      (report-problem 'filenames-retrieved "Retrieved filename seem strange"))
    (unless (or (oid-modified? sym-oid) (oid-modified? file-oid))
      (let ((before #f) (after #f))
	(set! before (memusage))
	(swap-out sym-oid) (swap-out file-oid)
	(set! after (memusage))
	(unless (> before after)
	  (report-problem 'swap-out "Didn't save any memory"))
	(unless (and (not (oid-loaded? sym-oid))
		     (not (oid-loaded? file-oid)))
	  (report-problem 'swap-out "OID is still loaded"))))
    (let ((new (difference (get-scheme-files) (oid-value file-oid)))
	  (missing (difference (oid-value file-oid) (get-scheme-files))))
      (unless (and (empty? new) (empty? missing))
	(report-problem 'file-change "New files: " new)
	(report-problem 'file-change "Missing files: " missing)))
    (let ((new (difference (all-symbols) (oid-value sym-oid)))
	  (missing (difference (oid-value sym-oid) (all-symbols))))
      (unless (and (empty? new) (empty? missing))
	(report-problem 'symbol-change (set-size new) " new symbols  "
			new)
	(report-problem 'symbol-change (set-size missing) " missing symbols")))
    (unless (and (symbol? (oid-value sym-oid))
		 (= 1 (set-size (symbol? (oid-value sym-oid)))))
      (report-problem 'swap-symbols-retrieved
		      "Retrieved symbols seem strange: " (oid-value sym-oid)))
    (unless (and (string? (oid-value file-oid))
		 (= 1 (set-size (string? (oid-value file-oid)))))
      (report-problem 'swap-filenames-retrieved
		      "Retrieved filename seem strange: "
		      (oid-value file-oid)))))

(define (random-frame pool)
  (let ((oid (random-oid pool)))
    (until (frame? oid) (set! oid (random-oid pool)))
    oid))

(define (filename-frame)
  (let ((frame (random-frame test-pool)))
    (while (empty? (fget frame 'filename))
	   (set! frame (random-frame test-pool)))
    frame))

(define (test-filename-frame fname-frame)
  (lineout)
  (lineout "Testing with " fname-frame)
  (unless (string? (fget fname-frame 'filename))
    (report-problem 'filename "String slot written funny"))
  (unless (and (string? (fget fname-frame 'contents-as-string))
	       (equal? (fget fname-frame 'contents-as-string)
		       (filestring (fget fname-frame 'filename)
				   "iso-latin0")))
    (lineout "Here's the problem")
    (write (fget fname-frame 'contents-as-string)) (newline)
    (write (filestring (fget fname-frame 'filename) "iso-latin0"))
    (newline)
    (report-problem 'contents-as-string "Contents may have changed"))
  (unless (list? (fget fname-frame 'contents-as-list))
    (report-problem 'contents-as-list "pair slot written funny"))
  (unless (vector? (fget fname-frame 'contents-as-vector))
    (report-problem 'contents-as-vector "vector slot written funny"))
  (do-choices (expr-frame (fget fname-frame 'contents))
    (unless (eq? (fget expr-frame 'in-file) fname-frame)
      (report-problem 'in-file "Inverse pointer incorrect"))
    (unless (ftest file-frame 'contents-as-set (fget expr-frame 'expr))
      (report-problem 'contents-as-set "Expr not handled right"))))

(define (test-frames)
  (test-filename-frame (filename-frame)))
      
(define (test-indexing index full-index)
  (test-indexing-on-frame (filename-frame) index full-index))
(define (test-indexing-on-frame fname-frame index full-index)
  (let ((start #f) (end #f)
	(identity
	 (find-frames index
	     'filename (fget fname-frame 'filename))))
    (lineout "Testing"
      (if full-index " auto " " ") "indexing coverage on " fname-frame)
    (unless (= 1 (set-size identity))
      (report-problem
       'multiplicity "Too many frames for one filename: " fname-frame))
    
    (do-choices (expr-frame (fget fname-frame 'contents-as-frames))
      (unless (contains? fname-frame
			 (find-frames index
			     'contents-as-set (fget expr-frame
						    'expr)))
	(report-problem
	 'contents-as-set "Can't find parent indexed by expr: "
	 expr-frame)
	(do-choices (each-expr (fget expr-frame 'expr))
	  (unless (contains? fname-frame
			     (find-frames index
			       'contents-as-set each-expr))
	    (report-problem
	     'contents-as-set "Detail: Can't find parent indexed by expr: "
	     each-expr))))
      (when full-index
	(unless (eq? fname-frame
		     (find-frames index 'contents expr-frame))
	  (report-problem 'contents "Can't find parent from child: "
			  expr-frame)))
      (unless (empty? (find-frames index 'context fname-frame))
	(report-problem 'stop-slots "Someone indexed context"))
      (when full-index
	(let ((atoms (random-atom (fget expr-frame 'expr))))
	  ;; (get-expr-atoms (fget expr-frame 'expr))
	  (do-choices (atom atoms)
	    (unless (contains? expr-frame
			       (find-frames index 'atoms atom))
	      (report-problem 'atoms "Atom indexing didn't work: "
			      expr-frame atom))))
	(let ((atoms (random-atom (fget expr-frame 'expr))))
	  ;; (get-expr-atoms (fget expr-frame 'expr))
	  (do-choices (atom atoms)
	    (unless (contains? fname-frame
			       (find-frames index 'contents-as-frames
				 (find-frames index 'atoms atom)))
	      (report-problem
	       'two-stage-atoms
	       "Two stage indexing (with atoms) didn't work"
	       "; atom=" atom "; expr=" expr-frame))))))))
(define (expr-frame-test expr-frame fname-frame index)
  (unless (contains? fname-frame
		     (find-frames index
			 'contents-as-set (fget expr-frame 'expr)))
    (report-problem
     'contents-as-set "Can't find parent indexed by expr: "
     expr-frame)))
(define (do-dbtest)
  (test-pools)
  (test-oids)
  (test-frames)
  (when test-index (test-indexing test-index #f)) 
  (when auto-index (test-indexing auto-index #t))
  (framerd-stats))

(define (cleanup-memory)
  (lineout "Swapping stuff out")
  (commit-all) (swap-out-all))

(define (full-dbtest)
  (if (zero? (pool-load test-pool)) (build-db))
  (do-dbtest)
  (cleanup-memory)
  (framerd-stats)
  (do-dbtest)
  (cleanup-memory)
  (framerd-stats)
  ;; Here's what I found:
  (report-problems))

(define (main . args)
  (unless test-pool (dbtest-init))
  (qase arg1
	("make" (build-db) (do-dbtest) (commit-all))
	("test" (do-dbtest) (cleanup-memory) (do-dbtest))
	("cleanup" (cleanup-dbtest-files)))
  (remove-file (string-append dbroot "dbtest.running")))

;; Add this symbol to the environment to make all-symbols consistent
;; with interactive fdscript
'(that) 

(when (bound? test-batch-mode)
  (if (file-exists? (string-append dbroot "dbtest.running"))
      (begin (lineout "Cleaning up from aborted test")
	     (cleanup-dbtest-files)))
  (let ((build-phase
	 (not (file-exists? (string-append dbroot "dbtest.pool")))))
    (if build-phase
	(start-test "dbtest.scm (build)")
	(start-test "dbtest.scm (use)  "))
    (unless test-pool (dbtest-init))
    (full-dbtest)
    (unless (or build-phase (eq? test-batch-mode 'test))
      (cleanup-dbtest-files))
    (when (file-exists? (string-append dbroot "dbtest.running"))
      (remove-file (string-append dbroot "dbtest.running")))))
(clear-env-changes!)
(close-pool test-pool)
(close-index test-index)

