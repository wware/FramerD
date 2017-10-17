;;;
;;; You need to write a procedure named "run-benchmark" that takes
;;; two arguments.  The first is a string identifying the particular
;;; benchmark being run.  The second is a thunk, a procedure of no
;;; arguments, that will actually run the benchmark.
;;;

(define (run-benchmark benchmark-name benchmark-thunk)
  ;;; your code goes here
  (benchmark-thunk))

