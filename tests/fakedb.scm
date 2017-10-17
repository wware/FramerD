(use-module! fdinternals)
(use-module! fdmaint)
(set-notify! #t)

(define proc-id "single")
(define pause-interval 0)
(define last-names
  (choice "Smith" "Jones" "Liu" "de Beers" "Johannsen"))
(define first-names
  (choice "Mary" "Bill" "Zhu" "Ingrid" "Phillip" "Seiko"))
(define ages
  (let ((ages {})) (dotimes (i 50) (set+! ages (+ i 10)))
       ages))
(define jobs
  (choice 'professor 'doctor 'dog-trainer 'house-cleaner
	  'starship-pilot 'poet 'wrestler 'auto-mechanic))
(define colors
  (choice 'blue 'red 'green 'white 'purple 'fucsia
	  'grey 'black 'aqua 'tan 'brown 'crimson 'yellow
	  'orange ))
(define eye-colors (choice 'green 'blue 'brown))
(define hair-colors (choice 'red 'blonde 'brunette))
(define heights
  (* 2.55 (+ (* 12 (choice 4 5 6)) (choice 0 1 2 3 4 5 6 7 8 9))))

(define (make-fake pool)
  (let ((first (pick-one first-names))
	(last (pick-one last-names)))
    (frame-create pool
	'obj-name (string-append first " " last)
	'first-name first 'last-name last
	'species 'human
	'age (pick-one ages)
	'job (choice (pick-one jobs) (pick-one jobs))
	'favorite-color (pick-one colors)
	'eye-color (pick-one eye-colors)
	'hair-color (pick-one hair-colors)
	'height (pick-one heights))))

(define fake-pool #f)
(define fake-index #f)

(define (make-fake-db)
  (dotimes (i 500)
    (notify "Made frame " (make-fake fake-pool)))
  (notify "=== Done making fake db"))

(define (test-choice frame)
  (unless (test frame 'height
		(get
		 (find-frames fake-index
		     'first-name (get frame 'first-name)
		     'last-name (get frame 'last-name))
		 'height))
    (notify "Fake DB problem with names for " frame))
  (unless (test frame 'height
		(get (find-frames fake-index
			 'species 'human
			 'first-name (get frame 'first-name)
			 'last-name (get frame 'last-name))
		     'height))
    (notify "Fake DB problem with species+names for " frame))
  (unless (test frame 'height
		(get
		 (find-frames fake-index
		     'first-name (get frame 'first-name)
		     'eye-color (get frame 'eye-color))
		 'height))
    (notify "Fake DB problem with " frame))
  (let ((slot (pick-one '{last-name height age hair-color favorite-color})))
    (unless (test frame slot
		  (get (find-frames fake-index
			   'first-name (get frame 'first-name)
			   'eye-color (get frame 'eye-color))
		       slot))
      (notify
	  "Fake DB problem with first-name and eye-color with "
	frame)))
  (let ((choices
	 (find-frames fake-index
	     'first-name (get frame 'first-name)
	     'eye-color (get frame 'eye-color))))
    (unless (= (set-size (get choices 'eye-color)) 1)
      (notify "Fake DB problem with eye-color " frame))))

(define (test-fake-db)
  (dotimes (j 3)
    (dotimes (i 25)
      (let ((frame (random-oid fake-pool)))
	(get frame 'obj-name)		; Just load it
	(notify "[client " proc-id "] Testing " frame)
	(test-choice frame)
	(sleep pause-interval))))
  (notify "=== Done testing " proc-id))

(define (fake-db-init)
  (set! fake-pool
	(if (bound? arg2) (use-pool arg2)
	    (if (file-exists? "fakedb.pool") (use-pool "fakedb.pool")
		(make-file-pool "fakedb.pool" 1000 @17/1000))))
  (set! fake-index
	(if (bound? arg2) (use-index arg2)
	    (if (file-exists? "fakedb.index") (use-index "fakedb.index")
		(make-file-index "fakedb.index" 10000))))
  (use-autoindex! fake-index)
  (set! %default-expanders #t)
  (if (bound? arg3) (set! proc-id arg3))
  (if (bound? arg4) (set! pause-interval (read-from-string arg4)))
  (write-dtype-to-file (timestring) (string-append proc-id ".running")))

(define (fake-db-done)
  (if (file-exists? (string-append proc-id ".running"))
      (remove-file (string-append proc-id ".running"))))

(define (fake-dbmain)
  (qase arg1
    ("init" (fake-db-init) (fake-db-done))
    ("make" (fake-db-init) (make-fake-db) (fake-db-done))
    ("test" (fake-db-init) (test-fake-db) (fake-db-done))
    ("clean"
     (when (file-exists? "fakedb.pool") (remove-file "fakedb.pool"))
     (when (file-exists? "fakedb.index") (remove-file "fakedb.index"))
     (when (file-exists? "fdtemp.locks") (remove-file "fdtemp.locks")))))

(define (main . args) (fake-dbmain))

