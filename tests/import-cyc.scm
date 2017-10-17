;;; This reads the exported version of the CYC top level ontology.
;;; It sets up relations and slots and also guesses at the KIF name
;;;  for generated concepts.

(load-dll "fdtext")
(define noisy #t)
(define assertion-count 0)
(set+! stop-slots 'obj-name)
(set+! stop-slots 'source)
(use-autoindex! cyc-index)

(define cycfind
  '(macro (expr) (cons 'find-frames (cons 'cyc-indices (cdr expr)))))
(define (create-cyc-frame . slots)
  (let ((new (frame-create cyc-pool 'source cyc-source)))
    (set+! cyc-frames new)
    (do ((props slots (cddr props)))
	((null? props) new)
      (frame-add! new (car props) (cadr props)))))
(define (generate-kif-name original)
  (let ((segments (regex-fragment "[[:upper:]][[:lower:]]*" original)))
    (string->symbol
     (with-string-output
      (let ((words (if (equal? (car segments) "") (cdr segments)
		       segments)))
	(display (string-upcase (car words)))
	(unless (null? (cdr words))
	  (dolist (word (cdr words))
	    (unless (or (equal? word "-") (equal? word "")) (display "-"))
	    (unless (equal? word "") (display (string-upcase word))))))))))

(define (resolve-cyc-name name)
  (let ((frame (find-frames (amb cyc-index cyc-indices) 'cyc-name name)))
    (if (exists? frame) frame
	(create-cyc-frame 'cyc-name name))))
(define (declare-cyc-frame-from-string string)
  (let* ((name (subseq string 2 #f))
	 (symbol (string->symbol (string-upcase name))))
    (let ((frame (resolve-cyc-name symbol)))
      (unless (exists? (fget frame 'obj-name))
	(frame-set! frame 'obj-name name)
	;; Just in case someone refers to it this way
	(frame-add! frame 'cyc-name name)
	(frame-set! frame 'kif-name (generate-kif-name name))))))

(define cycref-prefix (string->symbol "#$"))
(define cycref-not-prefix (list (string->symbol "#$") 'NOT))

(define (cyc-name x)
  (if (frame? x) (try (fget x 'cyc-name) x) x))
(define (cyc-name x)
  (if (frame? x) (try (fget x 'obj-name) (fget x 'cyc-name) x) x))

(eval `(set! ,cycref-prefix
	     '(macro (expr) `(resolve-cyc-name ',(cadr expr)))))

(define (cyc-eval expr)
  (cond ((not (pair? expr)) (eval expr))
	((eq? (car expr) cycref-prefix)
	 (resolve-cyc-name (cadr expr)))
	(else (let ((args (map cyc-eval expr)))
		(cond ((and (= (length args) 2) (frame? (second args)))
		       (cyc-slot-eval (second args) (first args)))
		      (else
		       (cyc-nary-eval (first args) (rest args))))))))

(define (cyc-slot-eval frame slot)
  (try 
   ;; This should succeed if the slot has appropriately defined
   ;; get-methods specified for it.
   (fget frame slot) 
   ;; If it's not, we create a frame and give it the appropriate ISAs
   (let* ((new (create-cyc-frame
		   'obj-name `(,(cyc-name frame) ,(cyc-name slot))
		   #$isa (fget slot #$resultisa))))
     (frame-add! frame slot new)
     (frame-set! new 'cyc-name (list slot frame))
     (frame-set! new 'kif-name
		 (string->symbol
		  (with-string-output
		   (printout "(" (fget slot 'kif-name) " "
			     (fget frame 'kif-name) ")"))))
     new)))
(define (cyc-arg-slot predicate argc)
  (let ((name (string->symbol (stringout "ARG" argc "SLOT"))))
    (try (fget predicate name)
	 (let* ((frame (create-cyc-frame
			   'obj-name `(,(cyc-name predicate) ,name))))
	   (frame-add! predicate name frame)
	   frame))))
(define (cyc-nary-eval pred args)
  (try (index-get cyc-index (cons pred args))
       (let* ((name  `(,(cyc-name pred) ,@(map cyc-name args)))
	      (new (create-cyc-frame
		       'obj-name name #$isa (fget pred #$resultisa))))
	 (frame-add! new (cyc-arg-slot pred 0) pred)
	 (index-values cyc-index new 0 pred)
	 (let ((argc 1))
	   (dolist (arg args)
	     (let ((slot (cyc-arg-slot pred argc)))
	       (frame-add! new slot arg)
	       (index-values cyc-index new argc arg)
	       (set! argc (+ argc 1)))))
	 (when (frame? (car args)) (frame-add! (car args) pred new))
	 (index-values cyc-index new pred args)
	 new)))

(define (cyc-relation relation)
  (cond ((and (pair? relation)
	      (equal? (car relation) cycref-not-prefix))
	 (cyc-deny (map cyc-eval (cadr relation))))
	((and (pair? relation) (pair? (car relation))
	      (eq? (car (car relation)) cycref-prefix))
	 (cyc-assert (map cyc-eval relation)))
	(else
	 (lineout ";;; Evaluating " relation)
	 (lineout ";;; Yields " (eval relation)))))

(define (get-deny-slot x)
  (try (frame-get x 'not)
       (let ((denied (create-cyc-frame
	              'obj-name `(not ,(fget x 'obj-name)))))
	 (frame-add! denied 'not x)
	 (frame-add! x 'not denied)
	 denied)))

(define (cyc-assert args)
  (if (= (length args) 3)
      (begin (set! assertion-count (+ assertion-count 1))
	     (if noisy
		 (lineout ";" assertion-count " Assert "
			  (second args) " " (first args)))
	     (frame-add! (second args) (first args) (third args)))
      (cyc-eval args)))
(define (cyc-deny args)
  (let ((denial (get-deny-slot (first args))))
    (cyc-assert (cons denial (rest args)))))

(define (import-cyc-kb file pool index)
  (lineout ";; Precreating frame references for " file)
  (let* ((string (filestring file))
	 (refs (regex-gather "#\\$[[:alnum:]-]+" string)))
    (lineout ";; There are " (set-size refs) " frame references in " file)
    (grow-oid-table (set-size refs))
    (lineout ";; Creating " (set-size refs) " new frames")
    (declare-cyc-frame-from-string refs)
    (lineout ";; Reading relations")
    (call-with-input-file file
      (lambda (port)
	(do ((input (read port) (read port)) (count 1 (+ count 1)))
	    ((eof-object? input))
	  (cyc-relation input)
	  (when (zero? (remainder count 500))
	    (lineout ";; [" (timestring) "] " count " relations processed")
	    (framerd-stats)))))))

(set+! stop-slots (resolve-cyc-name 'comment))

'(import-cyc-kb "toplevel.cyc" cyc-pool cyc-index)

