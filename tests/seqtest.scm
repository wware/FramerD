;;; -*- Mode: fdscript; text-encoding: latin-1; -*-

(load-once (get-component "test-util.scm"))
(start-test "seqtest.scm")

(testing 'null-seq-1 '(subseq '(1 2 3) 0 0) '())
(testing 'null-seq-2 '(subseq '(1 2 3) 1 1) '())
(testing 'null-seq-3 '(subseq "123" 1 1) "")

(testing 'string-elt '(elt "meme" 3) #\e)
(testing 'string-elt '(elt "méme" 1) #\é)

(define s1 "francais")
(testing 'string-elt-set! '(let ((s (copy-lisp s1)))
			     (set-elt! s 4 #\ç)
			     s)
	 "français")

(define p1 (packet "cdfe33ba9c8833445a"))
(testing 'packet-ref '(packet-ref p1 2) 0x33)
(testing 'packet-subseq '(subseq p1 2 4) (packet "33ba"))
(testing 'packet-reverse '(reverse p1) (packet "5a4433889cba33fecd"))
(testing 'packet-position '(position 51 p1) 2)
(testing 'packet-search '(search (packet "ba9c") p1) 3)

(testing 'packet-elt '(elt p1 3) 0xba)
(testing 'packet-set-elt! '(let ((p2 (copy-lisp p1)))
			     (set-elt! p2 3 0x66)
			     (packet-ref p2 3))
	 0x66)

(define short-vec
  (short-vector 0 127 256 (* 256 16) (1+ (* 256 16)) (1- (* 256 16))))
(define int-vec
  (int-vector 0 127 256 (* 256 256)
		(1+ (* 256 256 16)) (1- (* 256 256 16))))
(define float-vec
  (float-vector 0.1 1.0 10.0 1000.0 1000000.53 1000000000000.0
		.0000000001 0.0))
(define double-vec
  (double-vector 0.1 1.0 10.0 1000.0 1000000.53 1000000000000000000.0
		 .000000000000000001 0.0))

(testing 'short-seq-length '(length short-vec) 6)
(testing 'short-seq-find-1 '(find 4096 short-vec) #t)
(testing 'short-seq-find-2 '(find 8192 short-vec) #f)
(testing 'short-seq-find-3 '(find 1048578 short-vec) #f)
(testing 'short-seq-position-1 '(position 4096 short-vec) 3)
(testing 'short-seq-position-2 '(position 17 short-vec) #f)
(testing 'short-seq-elt-1 '(elt short-vec 5) 4095)
(testing 'short-seq-search-1 '(search (vector 256 4096) short-vec)
	 2)
(testing 'short-seq-search-2 '(search (short-vector 256 4096) short-vec)
	 2)
(testing 'short-seq-search-3 '(search (int-vector 256 4096) short-vec)
	 2)
(testing 'short-set-elt-1
	 '(begin (set-elt! short-vec 3 17)
	   (elt short-vec 3))
	 17)

(testing 'int-seq-length '(length int-vec) 6)
(testing 'int-seq-find-1 '(find 1048577 int-vec) #t)
(testing 'int-seq-find-2 '(find 1048578 int-vec) #f)
(testing 'int-seq-position-1 '(position 65536 int-vec) 3)
(testing 'int-seq-position-2 '(position 17 int-vec) #f)
(testing 'int-seq-elt-1 '(elt int-vec 5)
	 1048575)
(testing 'int-seq-search-1 '(search (vector 65536 1048577) int-vec)
	 3)
(testing 'int-seq-search-2 '(search (int-vector 65536 1048577) int-vec)
	 3)
(testing 'int-seq-search-3 '(search (short-vector 127 256) int-vec)
	 1)
(testing 'int-set-elt-1
	 '(begin (set-elt! int-vec 3 17)
	   (elt int-vec 3))
	 17)

(testing 'float-seq-length '(length float-vec) 8)
(testing 'float-seq-find-1 '(find 0.1 float-vec) #t)
(testing 'float-seq-find-2 '(find 0.5 float-vec) #f)
(testing 'float-seq-position-1 '(position 1000000.53 float-vec) 4)
(testing 'float-seq-position-2 '(position 1000000.54 float-vec) #f)
;(testing 'float-seq-elt-1 '(elt float-vec 6) .000000000000000001)
(testing 'float-seq-search-1 '(search (vector 1.0 10.0) float-vec)
	 1)
(testing 'float-seq-search-2 '(search (float-vector 1.0 10.0) float-vec)
	 1)
(testing 'float-seq-search-3 '(search (double-vector 1.0 10.0) float-vec)
	 1)
;(testing 'float-set-elt-1
;	 '(begin (set-elt! float-vec 3 1000.15)
;	   (elt float-vec 3))
;	 1000.15)

(testing 'double-seq-length '(length double-vec) 8)
(testing 'double-seq-find-1 '(find 0.1 double-vec) #t)
(testing 'double-seq-find-2 '(find 0.5 double-vec) #f)
(testing 'double-seq-position-1 '(position 1000000.53 double-vec) 4)
(testing 'double-seq-position-2 '(position 1000000.54 double-vec) #f)
(testing 'double-seq-elt-1 '(elt double-vec 6)
	 .000000000000000001)
(testing 'double-seq-search-1 '(search (vector 1.0 10.0) double-vec)
	 1)
(testing 'double-seq-search-2 '(search (float-vector 1.0 10.0) float-vec)
	 1)
(testing 'double-seq-search-3 '(search (double-vector 1.0 10.0) float-vec)
	 1)
(testing 'double-set-elt-1
	 '(begin (set-elt! double-vec 3 1000.14159)
	   (elt double-vec 3))
	 1000.14159)

(testing 'subseq-empty-list '(subseq '() 0 0) '())

(testing 'list-search
	 '(search '(d e f) '(a b c d e f g h i j)) 3)
(testing 'string-search
	 '(search '".." "http://foo.bar.mit.edu/foo/../bar") 27)
(testing 'vec-search
	 '(search '#(d e f) '#(a b c d e f g h i j)) 3)
(testing 'unicode-string-search
	 '(search '".."
		  "http://foo.\u0135bar.mit\u0139.edu/foo/../bar") 29)

(clear-env-changes!)

(report-problems)
