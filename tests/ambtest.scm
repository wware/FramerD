;;; Testing non-determinism

(load-component "test-util.scm")
(start-test "ambtest.scm")

(define (nrange start end)
  (let ((answer {}))
    (dotimes (i (- end start))
      (set+! answer (+ start i)))
    answer))

(define (nrange-r start end)
  (if (= start end) {}
      (amb start (nrange-r (+ start 1) end))))

(define (srange cstart cend)
  (let ((start (char->integer cstart)) (end (char->integer cend)))
    (let ((answer {}))
      (dotimes (i (- end start))
	(set+! answer (string (integer->char (+ start i)))))
      answer)))

(unless (= (set-size (intersection (nrange 0 300) (nrange 200 800)))
	   100)
  (report-problem 'intersection "Problem with intersection"))
(testing 'intersection '(intersection (nrange 0 30) (nrange 20 40))
	 20 21 22 23 24 25 26 27 28 29)

(unless (= (set-size (union (nrange 0 300) (nrange 200 800)))
	   800)
  (report-problem 'union "Problem with union"))
(testing 'union '(union (nrange 0 5) (nrange 10 15))
	 0 1 2 3 4 10 11 12 13 14)

(unless (= (set-size (difference (nrange 200 800) (nrange 0 300)))
	   500)
  (report-problem 'difference "Problem with difference"))
(testing 'difference '(difference (nrange 0 10) (nrange 2 4))
	 0 1 4 5 6 7 8 9)

(define (pairup x) (cons x x))

(unless (= (set-size (pairup (nrange 0 10))) 10)
  (report-problem 'pairup "Problem with SPROC application"))
(testing 'pairup  '(pairup (nrange 0 5))
	 '(0 . 0) '(1 . 1) '(2 . 2) '(3 . 3) '(4 . 4))

(let ((x (nrange 1000 2000)))
  (unless (= (set-size x) 1000)
    (report-problem 'nd-let "LET doesn't retain sets")))

(unless (= (set-size (nrange 0 (nrange 0 20))) 19)
  (report-problem 'nrange-of-nrange
		  "Problem with element number in AMB on AMB sproc results"))
(testing 'nrange-of-nrange  '(nrange 0 (nrange 0 5))
	 0 1 2 3)

(unless (= (set-size (elts (elts '((a b c) (d e f) (a b c))))) 6)
  (report-problem 'elts-of-elts
		  "Problem with element number in AMB on AMB cproc results"))
(testing 'elts-of-elts '(elts (elts '((a b c) (d e f) (a b c))))
	 'a 'b 'c 'd 'e 'f)

(testing 'just-nrange '(nrange 10 20)
	 10 11 12 13 14 15 16 17 18 19)
(testing 'srange '(srange #\a #\z)
	 "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
	 "p" "q" "r" "s" "t" "u" "v" "w" "x" "y")
(testing 'srange '(srange #\0 #\9)
	 "0" "1" "2" "3" "4" "5" "6" "7" "8")
(testing 'nrange-r '(nrange-r 10 20)
	 10 11 12 13 14 15 16 17 18 19)

(testing 'choice<->list '(list->choices '(a b c d e))
	 'a 'b 'c 'd 'e)
(testing 'choice<->list '(list->choices (choice->list (choice 'x 'y 'z)))
	 'x 'y 'z)

(testing 'empty-choice '(empty? (choice))
	 #t)
(testing 'empty-intersection
	 '(empty? (intersection (choice 1 2 3) (choice 4 5 6)))
	 #t)
(testing 'not-empty-intersection
	 '(empty? (intersection (choice 1 2 3 4) (choice 4 5 6 7)))
	 #f)
(testing 'exists-intersection
	 '(exists? (intersection (choice 1 2 3 4) (choice 4 5 6 7)))
	 #t)
(testing 'not-exists-intersection
	 '(exists? (intersection (choice 1 2 3) (choice 4 5 6)))
	 #f)
(testing 'pick-one
	 '(< (pick-one (choice 1 2 3)) 4)
	 #t)

(begin (define state-list-var '())
       (define state-set-var (amb)))

(do-choices (each (nrange 0 200))
  (set! state-list-var (cons each state-list-var))
  (set+! state-set-var each))
(do-choices (each (nrange 0 50))
  (set! state-list-var (cons each state-list-var))
  (set+! state-set-var each))

;;; Test homogenous/heterogenous merges
(define homogeous-choices
  #({1 2 3} {"one" "two" "three"} {une deux trois}))
(define hresult {})
(testing 'homogenous-heterogenous-merge
	 '(begin (set! hresult (elt homogeous-choices (choice 0 1 2)))
	   (write-to-string hresult) #t)
	 #t)
(define (convert-arg x)
  (if (number? x) (* x 2)
    (if (string? x) (string->symbol x)
      (if (symbol? x) (symbol->string x)
	x))))
(testing 'self-modifying-do-choices
	 '(begin (do-choices (x hresult)
		   (set+! hresult (convert-arg x)))
	   hresult)
	 1 2 3 'une 'deux 'trois "one" "two" "three"
	 2 4 6 '|one| '|two| '|three| "UNE" "DEUX" "TROIS")

(testing 'do-choices (length state-list-var) 250)
(testing 'do-choices-set+! (set-size state-set-var) 200)
(testing 'exists '(exists (v (nrange 0 50))
		(= v (* 7 7)))
      #t)
(testing 'filter-choices '(filter-choices (v (nrange 0 20))
			    (even? v))
	 0 2 4 6 8 10 12 14 16 18)
(testing 'for-choices
	 '(for-choices (v (nrange 0 20)) (* 2 v))
	 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38)
(testing 'for-choices
	 '(for-choices (v (nrange 0 20)) (* 2 v))
	 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38)
(testing 'normal-let '(let ((x (amb 2 3 4))) (* x x))
	 4 6 8 9 12 16)

(testing 'count-choices '(count-choices (x (intersection (nrange 0 30)
					    (nrange 20 40)))
			  (even? x))
	 5)

(clear-env-changes!)
(report-problems)
