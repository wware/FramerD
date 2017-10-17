;;; -*- Mode: fdscript -*-

;;; Test unicode strings and symbols

(load-once (get-component "test-util.scm"))
(start-test "unicode-test.scm")

(testing 'char-code '(char->integer #\E) #x045)
(testing 'code-char '(integer->char #x045) #\E)
(testing 'unicode-chars '(char->integer #\u0345) #x0345)
(testing 'unicode-chars '(char->integer #\U00000345) #x0345)
(testing 'unicode-chars '(char->integer #\0x0345) #x0345)
(testing 'unicode-chars '(char->integer #\01505) #x0345)
(testing 'unicode-int2char '(integer->char #x0345) #\u0345)

;; This is a problem with comments that contain weird unicode escapes
;; like this one \uxxxx right here
(define string1 "foo\u0345bar")
(define string2 "foo\u0345bar")
(if (equal? string1 string2)
    (lineout "Unicode comparison works")
    (report-problem 'unicode-comparison "Unicode comparison broken"))
(if (= (write-dtype-to-file string1 "u1") 17)
    (lineout "Wrote unicode with right size")
    (report-problem 'unicode-write "Written unicode string has wrong size"))
(if (equal? string2 (read-dtype-from-file "u1"))
    (lineout "Unicode string DTYPEs work")
    (report-problem 'unicode-read
		    "Can't read UNICODE DTypes back in correctly"))
(if (= (write-dtype-to-file 'foo\u0345bar "u2") 17)
    (lineout "Wrote unicode symbol with right size")
    (report-problem 'unicode-write-symbol
		    "Written unicode symbol has wrong size"))
(if (eq? (read-dtype-from-file "u2") 'foo\u0345bar)
    (lineout "Unicode symbol DTYPEs work")
    (report-problem 'unicode-read
		    "Can't read UNICODE DTypes back in correctly"))

(if (eqv? (string-ref string1 3) #\u0345)
    (lineout "Unicode STRING-REF works")
    (report-problem 'unicode-string-ref "Unicode STRING-REF doesn't work"))
(if (eqv? (elt string1 3) #\0x0345)
    (lineout "Unicode ELT works")
    (report-problem 'unicode-string-elt "Unicode ELT doesn't work"))
(if (equal? (reverse string1) "rab\u0345oof")
    (lineout "Unicode reverse works")
    (report-problem 'unicode-string-reverse
		    "Unicode REVERSE doesn't work"))
(if (equal? (subseq string1 2 5) "o\u0345b")
    (lineout "Unicode subseq works")
    (report-problem 'unicode-string-subseq
		    "Unicode SUBSEQ doesn't work"))

(string-set! string2 3 #\\u0346)
(if (equal? string1 string2)
    (report-problem 'unicode-string-set "Unicode string-set! fails")
    (lineout "Unicode string-set! works"))
(string-set! string2 3 #\x)
(if (equal? string2 "fooxbar")
    (lineout "Unicode string-set! to ascii works")
    (report-problem 'unicode-string-set-ascii
		    "Unicode string-set! to ascii fails"))

(if (eq? 'fact 'fa\u0043t)
    (lineout "Unicode symbols seem to work")
    (report-problem 'unicode-symbols "Unicode symbols don't seem to work"))

(define thai-string "\u0c31")

(testing 'higher-unicode
	 '(string->packet thai-string "utf8")
	 ##"f0b1")

(define (föbar n)
  (if (= n 0) 1 (* n (föbar (- n 1)))))

(if (signals-error? (lineout 'föbar))
    (report-problem 'displaying-unicode-symbols
		    "Unicode symbols don't display right")
    (lineout "Unicode symbols display just fine"))
(if (= (föbar 5) 120)
    (lineout "ISO Latin 1 parsing seem to work")
    (report-problem 'prefixed-unicode-symbols
		    "Latin 1 symbols don't seem to work"))

(define (\u0345act n)
  (if (= n 0) 1 (* n (\u0345act (- n 1)))))

(if (= (\u0345act 5) 120)
    (lineout "Prefixed Unicode symbols seem to work")
    (report-problem 'prefixed-unicode-symbols
		    "prefixed Unicode symbols don't seem to work"))

(define (fa\u0345t n)
  (if (= n 0) 1 (* n (fa\u0345t (- n 1)))))

(if (= (fa\u0345t 5) 120)
    (lineout "Internal unicode symbols seem to work")
    (report-problem 'internal-unicode-symbols
		    "Internal Unicode symbols don't seem to work"))

(define (fac\u0345 n)
  (if (= n 0) 1 (* n (fac\u0345 (- n 1)))))

(if (= (fac\u0345 5) 120)
    (lineout "Terminal unicode symbols seem to work")
    (report-problem 'terminal-unicode-symbols
		    "Terminal Unicode symbols don't seem to work"))

(testing 'unicode-string-downcase
	 '(string-downcase "Foo\u03a0") "foo\u03c0")
(testing 'unicode-string-upcase
	 '(string-upcase "foo\u03c0") "FOO\u03a0")
(testing 'unicode-char-upperp '(char-upper-case? #\u03c0) #f)
(testing 'unicode-char-upperp '(char-upper-case? #\u03a0) #t)
(testing 'unicode-char-lowerp '(char-lower-case? #\u03c0) #t)
(testing 'unicode-char-lowerp '(char-lower-case? #\u03a0) #f)
(testing 'unicode-char-whitespacep '(char-whitespace? #\u03a0) #f)
(testing 'unicode-char-whitespacep '(char-whitespace? #\\u0020) #t)
(testing 'unicode-char-whitespacep '(char-whitespace? #\\u0009) #t)
(testing 'unicode-char-whitespacep '(char-whitespace? #\u0251) #f)
;; Doesn't work without full unicode tables
;;(testing 'unicode-char-whitespacep '(char-whitespace? #\u2003) #t)

(testing 'list<->string1 '(list->string (string->list "fo\u00f6"))
	 "fo\u00f6")
(testing 'list<->string2
	 '(list->string (map char-upcase (string->list "fo\u00f6")))
	 "FO\u00d6")

(testing 'char-base '(char-base #\é) #\e)
(testing 'char-lower-base '(char-lower-base #\é) #\e)
(testing 'char-lower-base '(char-lower-base #\Ö) #\o)
(testing 'string-base '(string-base "Ärrõçß") "Arrocß")
(testing 'string-lower-base '(string-lower-base "Ärrõß") "arroß")
(testing 'string-lower-base '(string-lower-base "çrrõß") "crroß")

(define string1 "öäß")
(string-fill! string1 #\k)
(testing 'string-fill-conversion '(length string1) 3)

(clear-env-changes!)
(report-problems)
