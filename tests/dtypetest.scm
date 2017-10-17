;;;; Test writing of DTypes to disk files

(load-once (get-component "test-util.scm"))
(unless (bound? nrange) (load-component "ambtest.scm"))
(start-test "dtypetest.scm")

(write-dtype-to-file 33 "thirtythree")
(let ((p (fopen "thirtythree" "rb")))
  (unless (= (read-byte p) #x03)
    (lineout "Integer DType written funny"))
  (let ((first-byte (read-byte p)))
    (unless (zero? first-byte)
      (lineout "Integer data written funny")
      (if (= first-byte 33)
	  (lineout "Byte order incorrect")))
    (unless (and (zero? (read-byte p))
		 (zero? (read-byte p)))
      (lineout "Integer data written funny"))
    (unless (= (read-byte p) 33)
      (lineout "Integer data written funny")))
  (fclose p)) 
(remove-file "thirtythree") 

(let ((original (nrange 30 50)))
  (write-dtype-to-file original "thirty2fifty")
  (if (= (set-size (intersection original
				 (read-dtype-from-file
				  "thirty2fifty")))
	 (set-size original))
      (lineout "Sets seem to be dumped right")
      (lineout "Sets seem to be dumped wrong"))) 
(remove-file "thirty2fifty")

(define dtype-test-obj
  `(#[foo 3 bar 8] ##"123456789abcdef123456789"
    #t #f #\a () 9739 -3 3.1415
    ,(short-vector 34 35 1024)
    ,(int-vector 34 35 1024 150000)
    ,(float-vector 3.4 3.5 1000000000.05 .00000001)
    ,(double-vector 3.4 3.5 100000000000.05 .000000000000000001)
    ,@#((test) "te \" \" st" "" test #() b c)))

(testing 'write-dtype '(write-dtype-to-file dtype-test-obj "test.dtype")
	 254)
(testing 'read-dtype '(read-dtype-from-file "test.dtype")
	 dtype-test-obj)
(testing 'dtype-size '(dtype-size dtype-test-obj) 254)
(testing 'dtype-file-size '(file-size "test.dtype") 254)
(testing 'write-dtype-to-packet ;; We skip the slotmap since the order of slots may not be preserved
	 '(write-dtype-to-packet (cdr dtype-test-obj))
	 ##"080807000000022323080600000018313233343536373839616263646566313233343536373839010802010802000840000161080108030000260b0803fffffffd08410108400921cac083126f0841040600220023040008410310000000220000002300000400000249f0084105104059999a406000004e6e6b28322bcc7708410620400b333333333333400c00000000000042374876e8000ccd3c32725dd1d243ac09000000070807000000045445535401060000000974652022202220737406000000000700000004544553540900000000070000000142070000000143")
(testing 'length-write-dtype-to-packet
	 '(length (write-dtype-to-packet dtype-test-obj))
	 254)
(testing 'read/write-dtype-to-packet
	 '(read-dtype-from-packet (write-dtype-to-packet dtype-test-obj))
	 dtype-test-obj)

(define zs (zstring "abcdefghijklmnopqrstuvwxyz"))
(testing 'write-zstring '(write-dtype-to-file zs "ztest.dtype") 31)
(unless (eq? (read-dtype-from-file "ztest.dtype") zs)
  (report-problem 'read-zstring "Read zstring doesn't match"))

(remove-file "test.dtype")
(clear-env-changes!)
(report-problems)

