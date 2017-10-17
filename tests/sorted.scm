;;; -*- Mode: fdscript; -*-

(load-component "test-util.scm")
(start-test "sorted.scm")

(define sort-sample
  {33 34 134 33.2 33.1 34.5
      '("thirty" "three")
      1300000000000
      #("thirty" "one" "hundred")
      "thirty-three" "thirty-one"
      'thirty-three 'thirty-one
      #("thirty" "three" "hundred" "five")})

(testing 'sorted '(sorted sort-sample)
	 '#(33 33.1 33.2 34 34.500000 134 1300000000000
	       THIRTY-ONE THIRTY-THREE 
	       "thirty-one" "thirty-three"
	       ("thirty" "three") 
	       #("thirty" "one" "hundred")
	       #("thirty" "three" "hundred" "five")))
(testing 'sorted '(sorted sort-sample #f)
	 '#(33 33.1 33.2 34 34.500000 134 1300000000000
	       THIRTY-ONE THIRTY-THREE 
	       "thirty-one" "thirty-three"
	       ("thirty" "three") 
	       #("thirty" "one" "hundred")
	       #("thirty" "three" "hundred" "five")))

(define sort-seq-sample
  (filter-choices (elt sort-sample)
    (sequence? elt)))

(testing 'sorted '(sorted sort-seq-sample length)
	 '#(("thirty" "three")
	    #("thirty" "one" "hundred") 
	    #("thirty" "three" "hundred" "five")
	    "thirty-one" "thirty-three"))

(clear-env-changes!)
(report-problems)

