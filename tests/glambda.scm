;;; -*- Mode: fdscript; -*-

(load-once (get-component "test-util.scm"))
(start-test "glambda.scm")

(gdefine (width x)
  (choice (/ (area x) (height x))
	  (get x 'width)))
(gdefine (height x)
  (choice (/ (area x) (width x))
	  (get x 'height)))
(gdefine (area x)
  (choice (* (height x) (width x))
	  (get x 'area)))

(define s1 (frame-create #f 'width 30 'height 10))
(define s2 (frame-create #f 'area 30 'height 10))
(define s3 (frame-create #f 'area 30 'height 10 'width 5))

(testing 'glambda '(area s1) 300)
(testing 'glambda '(width s1) 30)
(testing 'glambda '(width s2) 3)
(testing 'glambda-amb '(width s3) 3 5)
(testing 'glambda-amb '(area s3) 30 50)
(testing 'glambda-amb '(height s3) 10 6)

(clear-goals!)
(clear-env-changes!)
