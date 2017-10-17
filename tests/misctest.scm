;;; -*- Mode: fdscript; text-encoding: latin-1 -*-

(load-once (get-component "test-util.scm"))
(start-test "misctest.scm")

(testing 'procedure? '(procedure? car) #t)
(testing 'procedure? '(procedure? if) #t)
(testing 'procedure? '(procedure? '()) #f)
(testing 'procedure? '(procedure? 'car) #f)
(testing 'applicable? '(applicable? car) #t)
(testing 'applicable? '(applicable? if) #f)
(testing 'applicable? '(applicable? '()) #f)
(testing 'applicable? '(applicable? 'car) #f)
(testing 'special-form? '(special-form? 'if) #f)
(testing 'special-form? '(special-form? if) #t)
(testing 'special-form? '(special-form? car) #f)
(testing 'primitive? '(primitive? 'car) #f)
(testing 'primitive? '(primitive? get) #t)
(testing 'primitive? '(primitive? testing) #f)
(testing 'procedure-arity-car '(procedure-arity car) 1)
(testing 'procedure-arity-cons '(procedure-arity cons) 2)
(testing 'procedure-arity-fadd '(procedure-arity (eval 'fadd! fdinternals)) 3)
(testing 'primitive? '(primitive? get) #t)
(testing 'call/cc
	 '(call/cc (lambda (done)
		    (testing 'applicable-continuation (applicable? done) #t)
		    (testing 'continuation? (continuation? car) #f)
		    (testing 'continuation? (continuation? done) #t)
		    (done 3)))
	 3)

;;; Testing slotmaps

(define f1 (frame-create #f 'foo 3 'bar 8))
(define f2 (frame-create #f  'bar 8 'foo 3))
;; Note that this definition also (in dbtest.scm) using slotmaps as keys
(define f3 #[bar 8 foo 3])

(testing 'slotmap-copy '(eq? f1 (copy-lisp f1)) #f)
(testing 'slotmap-copy '(equal? f1 (copy-lisp f1)) #t)

(testing 'slotmap-compare '(eq? f1 f2) #f)
(testing 'slotmap-compare '(equal? f1 f2) #t)

(testing 'slotmap-compare '(eq? f1 f3) #f)
(testing 'slotmap-compare '(equal? f1 f3) #t)

;;; Testing packets

(define p1 ##"1f03")
(define p2 ##"c0bd4ec937cfd6780ea29c4ba6cf3074")
(define p3 ##"1f032")

(testing 'packet-length '(length p1) 2)
(testing 'packet-length '(length p2) 16)
(testing 'packet-length '(length p3) 3)
(testing 'packet-ref '(elt p1 1) 3)
(testing 'packet-ref '(elt p2 7) 120)
(testing 'packet-ref '(elt p3 0) 1)

;;; Testing timeout
(when (bound? with-time-limit)
  (testing 'with-time-limit
	   '(with-time-limit 1 (dotimes (i 1000000000)) #f)
	   #f))

;;; Does the declaration above really work?
(testing 'load-encoding '(equal? "föbar" "f\u00f6bar") #t)

(define memoization-index
  (make-file-index "memization.index" 1000))
(define (timc-checks)
  (testing 'apply-resolves-tail-calls
	   '(apply (lambda (x) (+ 1 x)) (list (choice 1 2)))
	   2 3)
  (testing 'real-0.1 '(real? 0.1) #t)
  (testing 'real-.1 '(real? .1) #t)
  (testing 'number-0.1 '(number? 0.1) #t)
  (testing 'real-1/12 '(real? (/ 1 12)) #t)
  (testing 'number-1/12 '(number? (/ 1 12)) #t)
  (testing 'number-.1 '(number? .1) #t)
  (testing 'index-cache-1 '(index-cache memoization-index 'a 'b) 'B)
  (testing 'index-cache-2 '(index-cache memoization-index 'a 'b) 'B)
  (testing 'index-cache-3 '(index-cache memoization-index 'a 'b) 'B)
  (testing 'subseq1-2 '(subseq '(1 2) 0 1) '(1))
  (testing 'subseq0-0 '(subseq '(1 2) 0 0) '())
  (testing 'subseq1-1 '(subseq '(1 2) 1 1) '()))
(timc-checks)

(define ht (make-hashtable))
(hashtable-set! ht "uro" (integer->char 0x20a0))

(define xmlstring
  (read-dtype-from-file (get-component "xmlstring.dtype")))
(define xmlparsed
  (read-dtype-from-file (get-component "xmlparsed.dtype")))
; XXX comment out until file exists.
(define xmlfilestring
  (filestring (get-component "xmldoc.xml")))
(testing 'convert-entities
	 '(convert-character-entities xmlfilestring ht)
	 xmlstring)
(testing 'xml-parser
	 ' (parse-xml (convert-character-entities xmlfilestring ht))
	 xmlparsed)

(clear-env-changes!)
(report-problems)



