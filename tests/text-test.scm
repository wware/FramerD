;;; Testing text primitives

(load-once (get-component "test-util.scm"))
(start-test "text-test.scm")

(set-encoding! "iso-8859/1")

(testing 'has-suffix '(has-suffix ".scm" "foo.scm") #t)
(testing 'has-suffix '(has-suffix ".lsp" "foo.scm") #f)

(testing 'has-prefix '(has-prefix "foo" "foo.scm") #t)
(testing 'has-suffix '(has-suffix "bar" "foo.scm") #f)

(testing 'empty-string? '(empty-string? "") #t)
(testing 'empty-string? '(empty-string? "full") #f)

(testing 'lowercase? '(lowercase? "foo") #t)
(testing 'lowercase? '(lowercase? "Foo") #f)

(testing 'uppercase? '(uppercase? "ÇÖØ") #t)
(testing 'uppercase? '(uppercase? "çöø") #f)

(testing 'uppercase? '(uppercase? "FOO") #t)
(testing 'uppercase? '(uppercase? "Foo") #f)

(testing 'uppercase? '(uppercase? "ÇÖØ") #t)
(testing 'uppercase? '(uppercase? "çöø") #f)

(testing 'capitalized? '(capitalized? "FOO") #t)
(testing 'capitalized? '(capitalized? "Foo") #t)
(testing 'capitalized? '(capitalized? "foo") #f)

(testing 'capitalized? '(capitalized? "ÇOO") #t)
(testing 'capitalized? '(capitalized? "çoo") #f)

(testing 'numeric? '(numeric? "33") #t)
(testing 'numeric? '(numeric? "33.5") #f)
(testing 'numeric? '(numeric? "$33.50") #f)
(testing 'numeric? '(numeric? "thirty-three") #f)

(define sample-string
  "The book was over here by the bookcase
which was brightly colored. 3345-9877703-33333
is the other number")

(testing 'multi-line? '(multi-line? sample-string) #t)
(testing 'multi-line? '(multi-line? "what's my line") #f)

(testing 'alphabetic% '(alphabetic% sample-string) 66)
(testing 'whitespace% '(whitespace% sample-string) 15)

(testing 'string-subst
	 '(string-subst "xfooy" "foo" "bar") "xbary")

(testing 'english-stem '(english-stem "walked") "walk")
(testing 'english-stem '(english-stem "computation") "comput")
(testing 'english-stem '(english-stem "computing") "comput")
(testing 'english-stem '(english-stem "computer") "comput")

(define test-text
  "Bill J. Clinton met with Bill Gates, III in New Orleans, LA and San Diego.")
(define time-text
  "The squad travelled from last Friday to July 20, 1969.")
(define i18n-test-text
  "Danes smuggled Jews over the Öersund to Malmö.")
(testing 'get-refpoints '(refpoints test-text)
	 "Bill J. Clinton" "Bill Gates, III" "New Orleans, LA" "San Diego")
(testing 'i18n-get-refpoints '(refpoints i18n-test-text)
	 "Danes" "Jews" "Öersund" "Malmö")

(testing 'get-timepoints '(get-times time-text)
	 "Friday" "July 20, 1969")

(testing 'segment '(segment test-text)
	 '("Bill" "J" "Clinton" "met" "with" "Bill" "Gates" "III"
	   "in" "New" "Orleans" "LA" "and" "San" "Diego" ""))
(testing 'segment '(segment sample-string)
	 '("The" "book" "was" "over" "here" "by" "the"
	   "bookcase" "which" "was" "brightly" "colored"
	   "3345" "9877703" "33333" "is" "the" "other" "number"))

(testing 'tx-match '(tx-match "goo" "goo") #t)
(testing 'tx-match '(tx-match "goo" "good") #f)
(testing 'tx-matcher '(tx-matcher "goo" "good") 3)

(testing 'tx-matcher '(tx-matcher '(isalpha+) "good3+") 4)
(testing 'tx-matcher '(tx-matcher '(isalnum+) "good3+") 5)

(testing 'tx-matcher '(tx-matcher '#((isalnum+) (isspace+) "foo")
				       "baz3    foobar") 11)
(testing 'tx-match '(tx-match '#((isalnum+) (isspace+) "foo")
				   "baz3    foo")
	 #t)

(testing 'tx-match
	 '(tx-match
	  '#("(define" (isspace+) (lsymbol) (isspace+) (isdigit+) ")")
	  "(define foo-bar3 8)") #t)

(testing 'tx-match
	 '(tx-match '#("foo" (* "xy") "bar") "fooxybar") #t)
(testing 'tx-match
	 '(tx-match '#("foo" (* "xy") "bar") "fooxyxybar") #t)
(testing 'tx-match
	 '(tx-match '#("foo" (* "xy") "bar") "fooxxyybar") #f)
(testing 'tx-match
	 '(tx-match '#("foo" (* "xy") "bar") "foobar") #t)
(testing 'tx-match
	 '(tx-match '#("foo" (+ "xy") "bar") "foobar") #f)
(testing 'tx-match
	 '(tx-match '#("foo" (+ "xy") "bar") "fooxyxybar") #t)
(testing 'tx-match
	 '(tx-match '#("foo" (not "xy") "xy") "fooabcdefgxy") #t)
(testing 'tx-match
	 '(tx-match '#("foo" (not "xy") "xy") "fooabcdefg") #f)

(define expr-pat
  (tx-closure
   {(LSYMBOL)
    #("(" (* {#(expr-pat (isspace+)) expr-pat}) ")")
    }))
(testing 'tx-match
	 '(tx-match 'expr-pat "(foo bar baz)") #t)
(testing 'tx-match
	 '(tx-match 'expr-pat "(foo bar baz") #f)
(testing 'tx-match '(tx-match 'expr-pat "(+ 33 44)") #t)
(testing 'tx-match '(tx-match 'expr-pat "(+ (* 11 3) 44)") #t)
(testing 'tx-match '(tx-match 'expr-pat "(+ (* 11 3 44)") #f)

;;; Case sensitivity tests

(testing 'tx-match-ci '(tx-match "abc" "ABC") #t)
(testing 'tx-match-ci '(tx-match '(match-case "abc") "ABC") #f)
(testing 'tx-match-ci '(tx-match '(match-case #("abc" "def")) "ABCdef")
	 #f)
(testing 'tx-match-ci
	 '(tx-match '(match-case #((ignore-case "abc") "def")) "ABCdef")
	 #t)

(testing 'fragment-with-empty-separator
	 '(tx-fragment " somethhing made from words"
		       '(* (char-not " a\n\r\t")))
	 '(" " "" "somethhing" "" " " "" "m" "" "a" ""
	   "de" "" " " "" "from" "" " " "" "words" ""))
(testing 'fragment-with-empty-separator2
	 '(tx-fragment " somethhing mäde from words"
		       '(* (char-not " ä\n\r\t")))
	 '(" " "" "somethhing" "" " " "" "m" "" "\u00e4"
	   "" "de" "" " " "" "from" "" " " "" "words" ""))

(testing 'match-null-string-at-end
	 '(tx-fragment " somethhing made from http://words.com"
		       '#({"http://" "ftp://"}
			  (+ {(isalnum+) "."})
			  { "" #("/" (* (char-not "\"<> \t\n\r"))) }))
	 '("" " somethhing made from " "http://words.com" ""))

(testing 'i18n-fragment1
	 '(tx-fragment "na sowas Überfall"
		       '#((isupper) (* (isalnum)) (isupper)))
	 '("na sowas \u00dcberfall"))

(testing 'i18n-fragment2
	 '(tx-fragment "na sowas Überfall"
		       '#((isupper) (* (isalnum))))
	 '("" "na sowas " "\u00dcberfall" ""))

(testing 'word-search-1 '(tx-search '(word (isalpha+)) "word") 0)
(testing 'word-search-2 '(tx-search '(word (isalpha+)) " word") 1)
(testing 'word-search-3 '(tx-search '(word (isalpha+)) " word ") 1)
(testing 'word-search-4 '(tx-search '(word (isalpha+)) " word9 "))
(testing 'word-search-5 '(tx-search '(word (isalpha+)) " word9"))
(testing 'word-search-6 '(tx-search '(word (isalpha+)) "wörd") 0)
(testing 'word-search-7 '(tx-search '(word (isalpha+)) " åerö") 1)
(testing 'word-search-8 '(tx-search '(word (isalpha+)) " åerö ") 1)
(testing 'word-search-9 '(tx-search '(word (isalpha+)) " åerö9 "))
(testing 'word-search-10 '(tx-search '(word (isalpha+)) " åerö9"))
(testing 'word-fragment-1
	 '(tx-fragment "This is a test" '(word {"is" "a"}))
	 '("" "This " "is" " " "a" " test"))

(testing 'diacritic-insensitivity-test-1
	 (tx-match '(IGNORE-DIACRITICS "foobar") "foöbár")
	 #t)
(testing 'diacritic-insensitivity-test-2
	 (tx-matcher '(IGNORE-DIACRITICS "foo") "foöbár")
	 3)
(testing 'diacritic-insensitivity-test-3
	 (tx-search '(IGNORE-DIACRITICS "bar") "foöbár")
	 3)

(testing 'spacing-insensitivity-test-1
	 (tx-match '(IGNORE-SPACING "George Washington  Carver") "George 
  Washington 	Carver")
	 #t)
(testing 'spacing-insensitivity-test-2
	 (tx-matcher '(IGNORE-SPACING "George Washington  Carver") "George 
  Washington 	Carver was a great man")
	 28)
(testing 'spacing-insensitivity-test-3
	 (tx-search '(IGNORE-SPACING "George Washington  Carver") "who was George 
  Washington 	Carver really?")
	 8)
(testing 'spacing-insensitivity-test-4
	 (tx-gather '(IGNORE-SPACING "George Washington") "who was George 
  Washington 	Carver really?")
	 "George \n  Washington")

(testing 'match-canonical-test-4
	 (tx-gather '(CANONICAL "George Washington") "who was géöRgê WASHÍÑgtön anyway")
	 "g\u00e9\u00f6Rg\u00ea WASH\u00cd\u00d1gt\u00f6n")

;;; Extraction tests

(define url-pattern
  #("http://"
    (label host (char-not ":/"))
    (label port {"" #(":" (isdigit+))}) 
    (label dir (chunk #("/" (* #((char-not "/") "/")))))
    (label name {"" (char-not "./")})
    (label suffix (chunk {"" #("." (not> (eol)))}))))

(testing 'tx-null-string-extract
	 '(tx-extract #("" "" "" "foo" "" "") "foo")
	 #("" "" "" "foo" "" ""))

(testing 'tx-extract-1
	 '(tx-extract url-pattern "http://www.framerd.org/docs/index.html")
	 #("http://" 
	   (LABEL HOST "www.framerd.org") 
	   (LABEL PORT "") (LABEL DIR "/docs/") 
	   (LABEL NAME "index") 
	   (LABEL SUFFIX ".html")))
(testing 'tx-extract-2
	 '(tx-extract url-pattern "http://www.framerd.org/docs/index.")
	 #("http://" 
	   (LABEL HOST "www.framerd.org") 
	   (LABEL PORT "") (LABEL DIR "/docs/") 
	   (LABEL NAME "index") 
	   (LABEL SUFFIX ".")))
(testing 'tx-extract-3
	 '(tx-extract url-pattern "http://www.framerd.org/docs/index")
	 #("http://" 
	   (LABEL HOST "www.framerd.org") 
	   (LABEL PORT "") (LABEL DIR "/docs/") 
	   (LABEL NAME "index") 
	   (LABEL SUFFIX "")))	 
(testing 'tx-extract-4
	 '(tx-extract url-pattern "http://www.framerd.org/docs/")
	 #("http://" 
	   (LABEL HOST "www.framerd.org") 
	   (LABEL PORT "") (LABEL DIR "/docs/") 
	   (LABEL NAME "") 
	   (LABEL SUFFIX "")))

(testing 'textlet
	 '(textlet url-pattern "http://www.framerd.org/docs/"
		   (vector host port dir name))
	 #("www.framerd.org" "" "/docs/" ""))

;;; Unicode tests

(testing 'tx-matcher-unicode '(tx-matcher "xöx" "xöxöy") 3)
(testing 'tx-search-unicode '(tx-search "xöx" "fooÿxöxöy") 4)

(testing 'unicode-segment '(tx-segment "föö bár bèç")
	 '("föö" "bár" "bèç"))

;; This used to signal bad utf-8 string because it got lost in the
;; middle of the string.
(testing 'unicode-not-match '(tx-match '(* (not "(")) "ab\u00d1cd")
	 #t)

(define msg-text (filestring (get-component "rfc822.txt")))
(define msg (read-mime msg-text))

(testing 'parse-rfc822 '(equal? msg (read-dtype-from-file (get-component "rfc822.dtype")))
	 #t)
(testing 'fget-rfc822 '(get msg 'date) "Sun, 22 Apr 90 15:10:14 EDT")

(testing 'parse-timestring
	 '(breakup-timestamp (parse-timestring (get msg 'date)))
	 #[TYPE TIMESTAMP
		YEAR 1990
		MONTH 4
		DAY 22
		HOUR 15
		MINUTE 10
		SECOND 14
		TZOFF -14400]
	 )
(testing 'parse-timestring
	 '(write-to-string (parse-timestring (get msg 'date)))
	 "#<1990-04-22T15:10:14-4:00>")
(testing 'iso-timestring
	 '(iso-timestring (parse-timestring (get msg 'date)) 'utc)
	 "1990-04-22T19:10:14UTC")
(testing 'iso-timestring
	 '(breakup-timestamp (parse-timestring
			      (iso-timestring (parse-timestring (get msg 'date))
					      'utc)))
	 #[TYPE TIMESTAMP
		YEAR 1990
		MONTH 4
		DAY 22
		HOUR 19
		MINUTE 10
		SECOND 14
		TZOFF 0]
	 )

(testing 'md5 '(md5 sample-string)
	 ##"c0bd4ec937cfd6780ea29c4ba6cf3074")
(testing 'md5 '(md5 msg-text)
	 ##"445cb4036c4dfac7ecf51f540aaff47f")

(define msg-headers
  (car (tx-segment msg-text "\n\n")))
(define msg-body
  (apply string-append (cdddr (tx-fragment msg-text "\n\n"))))
(testing 'tx-search '(tx-search "\n\n" msg-text) 185)
(testing 'rfc822-tx '(get msg 'content) msg-body)

(define header-pat '(* {(char-not "\n") #("\n" (isspace))}))
(testing 'rfc822-tx-gather '(tx-gather header-pat msg-headers)
	 "Date: Sun, 22 Apr 90 15:10:14 EDT"
	 "Received: by media-lab (5.57/4.8)  id AA10965; Sun, 22 Apr 90 15:10:16 EDT"
	 "From: alanr"
	 "To: mt, dfr, kwh"
	 "Subject: funny line in a .sig\n   and in subject")
(testing 'rfc822-tx-gather
	 '(tx-extract header-pat (tx-gather header-pat msg-headers))
	 '(* "Date: Sun, 22 Apr 90 15:10:14 EDT")
	 '(* "Received: by media-lab (5.57/4.8)  id AA10965; Sun, 22 Apr 90 15:10:16 EDT")
	 '(* "From: alanr")
	 '(* "To: mt, dfr, kwh")
	 '(* "Subject: funny line in a .sig" #("
" " ") "  and in subject"))


(testing 'stdstring
	 '(stdstring "\t\tThe story began in a little town by the name of of\n\tSan Diego")
	 "the story began in a little town by the name of of san diego")
(testing 'stdspace
	 '(stdspace "\t\tThe story began in a little town by the name of of\n\tSan Diego")
	 "The story began in a little town by the name of of San Diego")

(clear-env-changes!)
(report-problems)




