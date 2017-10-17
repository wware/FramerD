;; -*- Emacs-Lisp -*-

;; Special support for interacting with FDScript
;; Based on code in comint.el, inf-lisp.el, scheme.el, and cmuscheme.el

;;; $Id: fdscript.el,v 1.20 2003/08/27 10:53:27 haase Exp $

(require 'comint)
(require 'inf-lisp)
(require 'scheme)

(defvar *fdscript-obarray* nil)


;;; Adding some Scheme indent methods

(defun setup-indents (fcn-symbol)
  (put 'let fcn-symbol 1) ; override bogus scheme definition
  (put 'when fcn-symbol 1)
  (put 'unless fcn-symbol 1)

  (put 'dolist fcn-symbol 1)
  (put 'table* fcn-symbol 1)
  (put 'th* fcn-symbol 1)
  (put 'td* fcn-symbol 1)
  (put 'span fcn-symbol 1)
  (put 'div fcn-symbol 1)
  (put 'font fcn-symbol 1)
  (put 'httpdoc fcn-symbol 0)
  (put 'htmldoc fcn-symbol 0)
  (put 'form fcn-symbol 1)
  (put 'font fcn-symbol 1)
  (put 'doslots fcn-symbol 1)
  (put 'do-pool fcn-symbol 1)
  (put 'do-pool-mt fcn-symbol 1)
  (put 'dotimes fcn-symbol 1)
  (put 'doseq fcn-symbol 1)
  (put 'do-choices fcn-symbol 1)
  (put 'do-choices-mt fcn-symbol 1)
  (put 'filter-choices fcn-symbol 1)
  (put 'hashset-filter fcn-symbol 1)
  (put 'hashset-accept fcn-symbol 1)
  (put 'for-choices fcn-symbol 1)
  (put 'while fcn-symbol 1)
  (put 'until fcn-symbol 1)

  (put 'unwind-protect fcn-symbol 1)
  (put 'on-errors fcn-symbol 1)

  (put 'printout-to fcn-symbol 1)
  (put 'printout fcn-symbol 0)
  (put 'lineout fcn-symbol 0)
  (put 'with-output fcn-symbol 1)
  (put 'with-output-to-string fcn-symbol 0)

  (put 'sdefine fcn-symbol 1)

  (put 'with-slots fcn-symbol 2)
  (put 'multiple-value-bind fcn-symbol 2)
  (put 'textlet fcn-symbol 2)

  (put 'qase fcn-symbol 1)

  (put 'pick fcn-symbol 1)
  (put 'find-frames fcn-symbol 1)
  (put 'frame-create fcn-symbol 1)
  (put 'modify-frame fcn-symbol 2)

  (put 'table* fcn-symbol 1)
  (put 'markup fcn-symbol 1)
  (put 'block-markup fcn-symbol 1)

  (put 'with-mutex-locked fcn-symbol 1))
(setup-indents 'lisp-indent-function)
(setup-indents 'scheme-indent-function)


;;; Completion

(defun fdscript-complete-symbol ()
  "Perform completion on the Scheme symbol preceding point.
That symbol is compared against the symbols that exist in the Scheme
obarray, and any additional characters determined by what is there
are inserted. All symbols with function definitions, values
or properties are considered."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(point)))
	 (pattern (buffer-substring beg end))
	 (completion (try-completion pattern *fdscript-obarray*)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern *fdscript-obarray*)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))


(setq *fdscript-obarray*
  (mapcar 'list
	  '("warn*" "cl-mismatch" "gcd" "gset!" "string-set!"
	    "fd:cachable-get" "complex?" "string-ci>=?"
	    "symbolic-link?" "fzap!" "close-output-port" 
	    "close-pool" "read-from-file" "remove-file"
	    "within-module" "in-module" "get-gid" "index-zap!"
	    "make-pool-snapshot" "printout-to" "any<?"
	    "char-lower-case?" "tx-subst" "modify-frame" "eval-once"
	    "stderr" "record-tag" "open-process" "fslots" "file-size"
	    "null?" "equal?" "use-frame" "cddadr" "sleep" "write-data"
	    "basename" "set-size" "make-xarg" "difftime"
	    "file-access-time" "list->choices" "let*" "or" "empty?"
	    "hashtable-probe" "label-pool!" "random-oid" "comment"
	    "string-stream-contents" "try" "write-int" "hashset-probe"
	    "char-ci>?" "swap-out-indices" "memusage"
	    "standard-module" "cdaddr" "letrec" "ftruncate"
	    "ru-footprint" "max" "drop!" "packet-set!" "list->set"
	    "set-notify!" "lowercase?" "get-pid" "record?" "substring"
	    "call/cc" "commit-indices" "module-export!" "hashset-elts"
	    "string-upcase" "frame?" "fd:car-get" "sequence?"
	    "disable-slot-cache!" "ilog" "send-email" "float/"
	    "revert-pool" "all-symbols" "string-ci<=?"
	    "index-get-size" "copy-text-file" "caar" "apply-to-values"
	    "car" "string>=?" "xf" "clear-goals!" "output-port?"
	    "list->vector" "write-char" "hashset-size" "+" "get-best"
	    "revert-all" "make-file-index" "char=?" "xml-get"
	    "hashset-compact" "open-input-file" "quote" "autoload"
	    "rename-file" "hashset?" "hashtable-grow" "get-month"
	    "elt" "proper-choice" "get-portno" "tx-matcher"
	    "index-stats" "dovector" "set-oid-value!" "make-mutex"
	    "get-season" "*" "with-output-to-file" "module?"
	    "tx-closure" "cddar" "oid-loaded?" "exact?" "suffixrule"
	    "arglist" "index-cache" "commit-pool" "string-fill!"
	    "packet->vector" "with-extended-args" "list->string"
	    "get-rlimit-max" "get-module-exports" "revert-pools"
	    "swap-out-index" "find-file" "dynamic-wind" "pick"
	    "hashtable->alist" "procedure-arity" "urlhead" "min" "-"
	    "cdadr" "%set!" "lookup-method" "char-downcase" "assv"
	    "open-processx" "hashtable-max" "return-error"
	    "breakup-timestamp" "rationalize" "memq" "difference"
	    "proper-choice?" "uppercase?" "procedure?"
	    "string-contains?" "timestamp-plus" "make-vector"
	    "trace-prune" "xml-getall" "%busy" "fd:multi-add" "choice"
	    "read-record" "fadd!" "hashtable-keys" "fdgetenv"
	    "in-pool?" "uint->packet" "dtype-size" "qase" "get"
	    "intersector" "vector->list" "sorted" "set-random-seed!"
	    "cache-index" "goal-procedure?" "char<=?"
	    "char-lower-base" "symbol-bound?" "open-record-stream"
	    "frame-add!" "member?" "gvalue" "intersection" "notify*"
	    "parse-html" "do" "index-size" "and" "string>?"
	    "read-from-string" "get-uid" "count-slots"
	    "write-dtype-to-packet" "ru-page-faults" "/" "xml-content"
	    "pset!" "overlaps?" "get-names" "qstring" "build-string"
	    "sin" "traced-load" "reset-file-pool" "empty-string?"
	    "assert!" "config-set!" "char-upper-case?" "ceiling"
	    "enable-slot-cache!" "%push" "ptr-type"
	    "hashset-contains?" "load-config" "signal-exception" "abs"
	    "inexact?" "cl-position" "character?" "exists"
	    "with-input-from-file" "zero?" "frame-get" "has-prefix"
	    "safe-module" "cdaadr" "segment"
	    "convert-character-entities" "pick-one" "cdddar"
	    "string-trim" "match->frame" "index-frame" "commit-all"
	    "get-working-directory" "doseq" "fget" "vector-set!"
	    "super-pool-base" "add-refstop!" "ru-user-time"
	    "filestring" "log" "lambda" "ru-runtime" "file-exists?"
	    "symbol->string" "modulo" "%logior" "current-output-port"
	    "watch" "string" "magnitude" "add-dtype-to-file" "char?"
	    "subjob-output" "close-index" "index-count"
	    "pool-contents" "alphabetic%" "exp" "seq->set"
	    "hashtable-zap!" "pool-label" "never-save" "trouble*"
	    "tx-segment" "parse-xml-strict" "get-hour" "fd:assoc-get"
	    "parse-htmlr" "hashtable-map" "find-frames" "commit-pools"
	    "read-mime" "make-symbolic-link" "record-data"
	    "fd:ix-drop" "subjob-input" "all-pools" "choices->list"
	    "is-one-of?" "gdefine" "set-stack-limit!" "positive?"
	    "stdout" "list?" "frame-annotate" "make-rectangular"
	    "control-frame-printing" "get-component" "cddaar"
	    "string-ci>?" "swap-out-all" "tset!" "iso-timestring"
	    "pool-elts" "eval" "third" "cdaar" "largest" "tload"
	    "string-subst" "urlstring" "fd:cached-test" "packet"
	    "find" "make-super-pool" "flush-output" "input-port?"
	    "write-to-string" "morphrule" "index?" "mkstemp" "cdadar"
	    "cos" "fd:inverse-get" "auto-cache-file-indices"
	    "tx-gather" "pwd" "set-car!" "environment?" "position"
	    "xml-attributes" "oid-difference" "qc" "imag-part"
	    "logger" "unset!" "comma-list" "check-errno"
	    "fd:cachable-test" "atan" "??" "printout-apply"
	    "write-dtype-to-file" "cadddr" "whitespace%" "identical?"
	    "lexpr?" "floor" "cond" "get-now" "swap-out-pool"
	    "index-drop!" "let" "assq" "fclose" "values" "for-choices"
	    "cl-count" "subjob-socket" "do-results" "letchoices"
	    "unpreload-file-index!" "packet->uint"
	    "call-with-output-file" "real?" "clock"
	    "read-dtype-from-file" "list-tail" "make-file-pool"
	    "set-in-env!" "remote-eval" "string=?" "char>?"
	    "ireadline" "string->symbol" "ru-data-size" "=>" "begin"
	    "on-error" "subjob-pid" "list-ref" "mismatch" "tan"
	    "choice-size" "open-string-stream" "pool-base"
	    "use-module!" "n-args" "dtype-server?" "inexact->string"
	    "prog1" "inexact->exact" "trace-fdscript" "filedata"
	    "with-output" "symbol?" "remove" "multi-line?" "just-oids"
	    "eqv?" "timestamp" "special-form?" "fourth" "cadadr"
	    "string-copy" "quasiquote" "call-with-values"
	    "with-file-output" "export-frames" "get-year"
	    "raise-exception" "caddr" "prin1" "system"
	    "get-scores-from-samples" "index-add!" "load-user-profile"
	    "padd!" "readably" "char-alphabetic?" "clear-env-changes!"
	    "later?" "exact->inexact" "preload-file-index!" "list*"
	    "caaddr" "getdirs" "vector-fill!" "make-record" "%get"
	    "signals-error?" "cddr" "file-owner" "hashtable-add!"
	    "pprint" "xmltag-namespace" "get-pool" "force" "sandbox"
	    "index-values" "open-socket" "read-spacing" "string-ci<?"
	    "malloc-stats" "define-if-needed" "safe-eval" "set-group!"
	    "hash-lisp" "index-get" "for-each" "ndcall" "reverse"
	    "getfiles" "grow-oid-table" "intern-index-values!"
	    "enabled-module" "get-timekeys" "vector" "%add!"
	    "copy-frame" "char->integer" "framerd-stats" "set-cdr!"
	    "cdaaar" "set-index-threshold!" "make-xmltag" "<"
	    "primitive?" "nd-lexpr?" "fopen-locked" "pget"
	    "set-locale!" "lineout" "warn" "hashtable-slots"
	    "prefetch" "subseq" "index-add" "eq?" "make-frame"
	    "sdefine" "capitalized?" "<=" "numerator" "xmltag?"
	    "retract!" "copy-lisp" "oids-loaded" "hashset-grow!"
	    "fragment" "%logand" "notify" "unless" "integer?"
	    "truncate" "file-modification-time" "satisfied?"
	    "elements" "caadr" "string->packet" "real-part"
	    "tx-fragment" "char-ci=?" "fd:multi-get" "copy-file"
	    "all-modules" "sorted-choice" "with-input" "ftest"
	    "fcheck" "?index" "fd:inherited-test" "odd?" "cdar" ">"
	    "peek-char" "even?" "print" "subjob" "integer->char"
	    "set+!" "acos" "read" "file-pool?" "make-string" ">="
	    "string<=?" "=" "fd:inherited-get" "tx-extract"
	    "inherits-value?" "ru-system-time" "cons"
	    "clear-slot-cache!" "backquote" "xmltag-name"
	    "module-contains?" "set-file-encoding!" "english-stem"
	    "randomize!" "datestring" "load-library" "cl-find"
	    "hashset-add!" "set-default-encoding!" "set-search-max!"
	    "super-pool-top" "write-line" "string-ref"
	    "with-time-limit" "string-length" "glambda"
	    "write-to-file" "subjob-args" "hashtable-set!"
	    "hashset-get" "compare-oids" "1-" "parse-timestring"
	    "denominator" "cadar" "close-input-port" "continuation?"
	    "subjob-errors" "remote-subjob" "use-pool"
	    "get-extended-arg" "d" "string->number" "define"
	    "refpoints" "caaadr" "sortby" "$$" "readline" "exists?"
	    "tx-search" "open-index" "import-frame" "caddar"
	    "frame-features" "read-exprs" "get-minute" "urlget"
	    "alist->hashtable" "doslots" "pool-freespace?" "fail"
	    "pool-freespace" "file-index?" "$?" "seq->choice"
	    "char-whitespace?" "%logxor" "filter-choices"
	    "oid-addr-low" "char-ci<=?" "fd:ix-test" "vector-subst"
	    "use-autoindex!" "copy-binary-file" "value-path?"
	    "char-upcase" "get-scores" "do-pool" "get-user-data"
	    "restricted-module" "cdr" "call-with-input-file"
	    "packet->string" "string?" "packet-ref"
	    "with-string-output" "write-dtype" "ru-nswaps"
	    "allocate-from-super-pool" "load-once" "equal"
	    "loaded-oids" "frame-set!" "allocate-oid" "get-mailids"
	    "oid-plus" "lcm" "fd:multi-test" "apply" "defframe"
	    "quoted-choice" "either" "load-encoding" "load-file"
	    "get-date" "fifth" "load-component" "set-encoding!"
	    "char>=?" "some?" "fd:multi-drop" "file-creation-time"
	    "printout" "string<?" "session-id" "set-console-encoding!"
	    "freeze-choice" "has-suffix" "get-day" "revert-index"
	    "set!" "length" "sqrt" "getenv" "=<" "random"
	    "seq->choices" "smallest" "if" "registered-super-pool?"
	    "revert-oid" "network-index?" "bound?" "caaar"
	    "get-config-file" "hashset-map" "read-dtype" "dirname"
	    "record-stream-data" "xml-tag" "with-file-input" "packet?"
	    "quotient" "string-base" "xtimestamp" "hashset-grow"
	    "swap-out" "frame-zap!" "primary-hostname" "get-osid"
	    "remote-procedure?" "read-dtype-from-packet" "get-module"
	    "caadar" "with-string-input" "hashtable-get"
	    "choice->list" "get-similar" "set-index-sizes!"
	    "network-pool?" "break" "catch-errors" "index!" "pool-id"
	    "char-ci>=?" "set-elt!" "asin" "parse-iso8601"
	    "choices->hashset" "fd:cached-get" "elts" "commit-oid"
	    "textlet" "##" "void?" "stdstring" "hashset-filter"
	    "parallel" "strip-margin" "random-seed" "rest"
	    "set-rlimit!" "dolist" "with-slots" "hashset-zap!"
	    "add-abbrev!" "start-subjob" "every?" "index-keys"
	    "parse-xml" "fopen" "char-base" "negative?" "never-save!"
	    "do-choices" "get-pools" "lookup-host" "parse-rfc822"
	    "mpcall" "case" "search" "current-input-port"
	    "find-similar" "thaw-choice" "pool?"
	    "auto-cache-file-pools" "timestamp-diff" "amb"
	    "fd:kleene-get" "expt" "make-oid" "inherit-values"
	    "use-server" "applicable?" "second" "exit"
	    "fd:inverse-test" "numeric?" "union" "%remove" "earlier?"
	    "cache-pool" "fdd" "set-session-mnemonic!" "cadaar"
	    "string-lower-base" "get-kernel-modules" "string-downcase"
	    "contains?" "get-second" "hashset-intern"
	    "call-with-current-continuation" "scache" "file-older?"
	    "hashtable?" "delay" "test" "round" "make-delay" "trouble"
	    "oid-modified?" "index-prefetch" "string->list" "memv"
	    "index-set!" "until" "known-encoding?" "eof-object?"
	    "hash-dtype" "get*" "display" "consusage" "cddddr" "oid?"
	    "cd" "get-monthnum" "assoc" "any>?" "write-byte" "spawn"
	    "number->string" "probe-symbol" "vector-length"
	    "vector-ref" "commit-index" "get-similar-scored"
	    "set-super-pool-aliasing!" "pool-load" "frame-slots"
	    "identical" "rational?" "autolink" "lookup-frame" "map"
	    "tx-match" "dtcall" "hashtable-increment!" "gather"
	    "swap-out-pools" "dotimes" "-1+" "cadr" "regular-file?"
	    "stdin-string" "get-best-from-samples" "member" "get-ppid"
	    "make-hashtable" "config-reset!" "open-output-string"
	    "char<?" "open-input-string" "fopen-encoded" "sortcar"
	    "pool-capacity" "first" "stdin" "get-file-position"
	    "super-pool-loading" "%test" "not" "pair?" "string-ci=?"
	    "export-frame" "vector->packet" "md5" "file-writable?"
	    "read-char" "ru-stack-size" "while" "use-module" "append"
	    "hashtable-size" "is-literal?" "load-oid" "when"
	    "boolean?" "vector?" "fullname" "revert-indices" "caaaar"
	    "number?" "close-subjob" "get-homedir" "add!" "fd:ix-get"
	    "parse-args" "procedure-arguments" "oid-addr-high" "fset!"
	    "writable?" "make-hashset" "get-rlimit" "readlink"
	    "recover-pool" "procedure-name" "1+" "set-file-position!"
	    "get-module-bindings" "char-ready?" "timestring" "cdddr"
	    "directory?" "hashtable-skim" "char-numeric?"
	    "unwind-protect" "all-indices" "abort" "cl-search"
	    "char-alphanumeric?" "in-same-pool?"
	    "hashtable-increment-existing!" "oid-value"
	    "procedure-body" "open-output-file" "stringout" "count"
	    "slambda" "fd:ix-add" "char-ci<?" "read-byte"
	    "disable-notifications!" "get-daytime" "hashset-slots"
	    "index-load" "restore-pool-snapshot" "frame-create"
	    "fail?" "load-dll" "parse-arg" "frame-test" "apropos"
	    "use-index" "write" "newline" "xarg" "string-append"
	    "mkdir" "values->hashset" "remainder" "load" "%add"
	    "traced" "resources" "hashset-elements" "config-add!"
	    "list" "get-file-size" "with-mutex-locked" "get-times"
	    "cgetenv" "return-exception")))


;;; Utilities

(defun is-backslashed-p ()
  "True if there are an odd number of backslash-type chars before the point"
  (let ((pt (point)))
    (while (eq (char-syntax (preceding-char)) ?\\ )
      (backward-char 1))
    (let ((ret (not (zerop (% (- pt (point)) 2)))))
      (goto-char pt)
      ret)))

(defun is-in-quotes-p ()
  "If the point is between quotes or on a closing quote returns
the location of the opening quote.  Otherwise returns nil."
  (let ( (pt (point)) (count 0) (open) )
      (while (> (point) (point-min))
	(progn 
	  (backward-char 1)
	  (if (and (= (following-char) ?\")
		   (not (is-backslashed-p)))
	      (progn (setq count (1+ count))
		     (if open nil (setq open (point)))))))
      (goto-char pt)
      (if (zerop (% count 2)) nil open)))

(defun back-and-up-sexp (how-many)
  "Back out to the beginning of the current symbol or balanced expression"
  (interactive "p")
  (let ((q (is-in-quotes-p)))
    (if q (goto-char q)
      (if (and (memq (char-syntax (following-char))
		     '(?w ?_)) ; word,symbol
	       (not (memq (char-syntax (preceding-char))
			  '(?\( ? )))) ; open-paren, whitespace
	  (progn (backward-sexp)
		 (backward-up-list (- how-many 1)))
	(backward-up-list how-many)))))


;;;; Inits

(defvar inferior-fdscript-mode-hook nil
  "*Hook for customising inferior-scheme mode.")
(defvar fdscript-prompt "fdscript:"
  "The default fdscript interaction buffer")
(defvar fdscript-buffer nil
  "The default fdscript interaction buffer")
(defvar inferior-fdscript-mode-map nil)
(defvar fdscript-mode-map nil)

(defvar fdscript-program-name (or (getenv "FDSCRIPT") "fdscript")
  "The executable filename for FDScript")

(setq lisp-source-modes
      (cons 'fdscript-mode (cons 'scheme-mode lisp-source-modes)))

;;;; Getting input from the buffer

(defun fdscript-get-input ()
  "Gets the current region or the input at the point"
  (if mark-active (buffer-substring (point) (mark))
    (let ((here (point)) (start nil) (end nil))
      (save-excursion
	;; "^(\\|^[:[[:alpha:]]+]"
	(search-backward-regexp "\\(\\(^\\[\\w+\\]\\)\\|\\(^(\\)\\)") 
	(if (looking-at "^\\[[^]]+]")  (goto-char (match-end 0)))
	(setq start (point))
	(forward-sexp)
	(setq end (point))
	(if (not (and (>= here start) (<= here end)))
	    (error "Can't find top level expression")
	  (buffer-substring start end))))))

;; If the point is after the process mark, this just sends the current
;; input; if it's above the process mark (in the history of the
;; buffer), it gets the expression that starts on the current line and
;; inserts it at the process mark, going there.
(defun fdscript-smart-send ()
  "Either sends the current expression (if it is at the end of the
buffer) to the process or copies the expression on the current line to
the end of the buffer and goes there."
  (interactive)
  (widen)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let ((pmark (process-mark proc)))
	(if (> (point) pmark)
	    (progn
	      (comint-send-input)
	      (comint-send-string proc "\0\n"))
	  (let ((input (fdscript-get-input)))
	    (goto-char pmark) (insert input)))))))

(defvar fdscript-module)
(defconst in-module-regexp
  "(in-module +'\\(\\(\\w\\|[/$.-_]\\)+\\)")

(defun fdscript-get-module-name ()
  "Returns the module name specified in the current buffer"
  (if (and (boundp 'fdscript-module) fdscript-module) fdscript-module
    (save-excursion
      (goto-char (point-min))
      (let* ((pos (search-forward-regexp in-module-regexp (point-max) t))
	     (name
	      (if pos
		  (buffer-substring (match-beginning 1) (match-end 1))
		"")))
	(if pos
	    (message "Search found module name %s at %d" name pos)
	  (message "Search failed to find module name"))
	(if pos
	    (progn (make-variable-buffer-local 'fdscript-module)
		   (setq fdscript-module name)))
	name))))

(defun at-expr-end-p (pmark)
  "Returns true if the point is at the end of the expr sitting
in front of the process mark."
  (and (> (point) pmark)
       (let ((parse-state (parse-partial-sexp pmark (point))))
	 (= (car parse-state) 0))))

(defun fdscript-smart-newline ()
  "Does a send if the point is at the end of an expression, a newline
and indent otherwise."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let ((pmark (process-mark proc)))
	(if (and pmark (at-expr-end-p pmark))
	    (fdscript-smart-send)
	  (progn (newline-and-indent)
		 (message "Incomplete expression")))))))

(defun fdscript-narrow-to-input ()
  "Either sends the current expression (if it is at the end of the
buffer) to the process or copies the expression on the current line to
the end of the buffer and goes there, narrowing the focus to just that
expression."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let ((pmark (process-mark proc)))
	(if (> (point) pmark)
	    (save-excursion
	      (goto-char pmark) (forward-sexp) (backward-sexp)
	      (narrow-to-region (point) (point-max)))
	  (let ((input (fdscript-get-input)))
	    (goto-char pmark) (insert input)
	    (fdscript-narrow-to-input)))))))


;;; History functions

(defun input-completions (ring)
  (let ((i 0)
	(limit (car (cdr ring)))
	(vec (cdr (cdr ring)))
	(completions '()))
    (while (< i limit)
      (setq completions (cons (list (elt vec i)) completions))
      (setq i (+ i 1)))
    completions))

(defun fdscript-previous-input (arg)
  "Inserts some previous input string, just like regular previous
input, but with an argument does a completing read rather than getting
the Nth previous."
  (interactive "P")
  (if arg
      (let ((entry (completing-read "Redo: "
				    (input-completions comint-input-ring)
				    nil t)))
	(insert entry))
    (comint-previous-input 1)))

(defun fdscript-goto-input ()
  "Moves the cursor to a past input in the buffer.  It does a
completing read for the input to find; ? will list past inputs."
  (interactive)
  (let ((entry (completing-read "Go back to: "
				(input-completions comint-input-ring)
				nil nil)))
    (let ((keep-searching t))
      (while keep-searching
	(search-backward entry)
	(save-excursion
	  (beginning-of-line)
	  (if (looking-at "^\\:[[:alpha:]]+\\]") (setq keep-searching nil)))))))


;;;; Loading FDScript files

(defvar fdscript-source-modes '(fdscript-mode scheme-mode))

(defun fdscript-process ()
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-fdscript-mode)
				      (current-buffer)
				    fdscript-buffer))))
    (or proc (error "No FDScript process"))))

(defun fdscript-send (string)
  (let ((module (fdscript-get-module-name))
	(process (fdscript-process)))
    (if (not (equal module ""))
	(message "Sending %d characters into the %s module"
		 (length string) module))
    (if (not (equal module ""))
	(comint-send-string
	 process (format "(within-module '%s\n" module)))
    (comint-send-string process string)
    (if (not (equal module ""))
	(comint-send-string process ")\0\n")
      (comint-send-string process "\0\n"))))

(defun fdscript-buffer-end-pos ()
  (save-excursion
    (set-buffer (process-buffer (fdscript-process)))
    (point-max)))
(defun fdscript-get-output (old-end)
  (save-excursion
    (set-buffer (process-buffer (fdscript-process)))
    (goto-char (point-max)) (beginning-of-line)
    (if (and (>= (point) old-end) (looking-at "^\\[[^]]+]"))
	(progn (backward-char)
	       (buffer-substring old-end (point)))
      (let ((text (buffer-substring old-end (point-max))))
	(if (looking-at "^\\[[^]]+]") "Nothing" text)))))

(defun fdscript-eval-last-expr ()
  "Sends the region or current top-level expression to FDScript"
  (interactive)
  (let ((old-end (fdscript-buffer-end-pos)))
    (if mark-active
	(fdscript-send (buffer-substring (mark) (point)))
      (save-excursion
	(backward-sexp 1)
	(let ((start (point)))
	  (forward-sexp 1)
	  (fdscript-send (buffer-substring start (point))))))
    (sleep-for .25)
    (message "Evaluated to: %s"
	      (fdscript-get-output old-end))))

(defun fdscript-eval-toplevel ()
  "Sends the region or current top-level expression to FDScript"
  (interactive)
  (if mark-active
      (fdscript-send (buffer-substring (mark) (point)))
    (save-excursion
      (re-search-backward "^(")
      (let ((start (point)))
	(forward-sexp 1)
	(fdscript-send (buffer-substring start (point)))))))

(defun fdscript-eval-current-buffer ()
  "Sends the region or current top-level expression to FDScript"
  (interactive)
  (fdscript-send (buffer-substring (point-min) (point-max)))
  (message "Buffer sent to FDScript"))

(defun fdscript-toggle-watch ()
  "Inserts or removes a WATCH expression around the current expression and
evaluates the current definition"
  (interactive)
  (if (search-backward "(watch" (point-min) t)
      (progn (forward-char 1) (forward-sexp 1)
	     (let ((start (point)) (string nil))
	       (forward-sexp)
	       (setq string (buffer-substring start (point)))
	       (backward-sexp 1) (back-and-up-sexp 1)
	       (insert string) (kill-sexp 1) (backward-sexp 1)))
    (progn (back-and-up-sexp 1)
	   (insert "(watch ") (forward-sexp) (insert ")"))))


;;; Loading files

(defvar fdscript-file-default nil)
(defvar inferior-fdscript-load-command "(load \"%s\")\0\n")
(defun fdscript-load-file (file-name)
  "Load a Lisp file into the inferior Lisp process."
  (interactive (comint-get-source "Load FDscript file: "
				  fdscript-file-default
				  lisp-source-modes t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq fdscript-file-default
	(cons (file-name-directory file-name)
	      (file-name-nondirectory file-name)))
  (let ((proc (or (and fdscript-buffer (get-buffer-process fdscript-buffer))
		  (progn (run-fdscript fdscript-program-name)
			 (fdscript-process)))))
    (comint-send-string proc
      (format inferior-fdscript-load-command file-name))))


;;;; Identifying and editing OIDs

(defvar *fdscript-unknown-oid-regexp*
  "@[0123456789abcdefABCDEF]+/[0123456789abcdefABCDEF]+"
  "Used to identify literal OID references")
(defvar *fdscript-known-oid-regexp* "@/[^/]+/[0123456789abcdefABCDEF]+"
  "Used to identify OID references in named pools.")

(defun fdscript-get-oid-at-point ()
  "Gets the OID at the point, including its name."
  (save-excursion
    (if (not (looking-at "@")) (search-backward "@"))
    (while (not (or (looking-at *fdscript-unknown-oid-regexp*)
		    (looking-at *fdscript-known-oid-regexp*)))
      (search-backward "@" nil t))
    (let ((start (match-beginning 0))
	  (end (match-end 0)))
      (goto-char end)
      (if (looking-at "[\"\({]") (forward-sexp))
      (buffer-substring start (point)))))

(defun fdscript-show-oid-at-point ()
  "Describes the OID at the point by sending it to the current process
and moving the cursor down to the description."
  (interactive)
  (let ((string (fdscript-get-oid-at-point))
	(proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let* ((pmark (process-mark proc)))
	(goto-char pmark)
	(insert "(xf ") (insert string) (insert ")")
	(comint-send-input)
	(comint-show-output))))) 

(defun fdscript-goto-process-mark ()
  "Goes to the input point of the buffer"
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let* ((pmark (process-mark proc)))
	(goto-char pmark)))))


;;;; Miscellaneous stuff

;; From HQM: Styled after the lisp-machine c-x c-;
(defun fdscript-comment-out-region (start end arg)
  (interactive "r\nP")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (if (equal arg '-) 
	  (if (equal (following-char) ?\;) (delete-char 1))
	  (insert ";"))
      (forward-line))))


;;;; Setup stuff

(defvar inferior-fdscript-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.")

(defun fdscript-input-filter (str)
  "Don't save anything matching inferior-fdscript-filter-regexp"
  (not (string-match inferior-fdscript-filter-regexp str)))

(defun fdscript-mode-commands (map)
  ;; gnu convention
  (define-key map "\M-\C-x" 'fdscript-eval-toplevel)
  (define-key map "\e\C-m" 'fdscript-eval-toplevel)
  (define-key map "\C-c\C-e" 'fdscript-eval-last-expr)
  (define-key map "\C-c\C-l" 'fdscript-load-file)
  (define-key map "\C-c\C-z" 'fdscript-eval-current-buffer)
  (define-key map "\C-c\C-w" 'fdscript-toggle-watch)
  (define-key map "\C-c\C-i" 'fdscript-complete-symbol)
  (define-key map "\C-c;" 'fdscript-comment-out-region)
  (define-key map "\C-x\C-e" 'fdscript-eval-last-expr)
  (define-key map "\C-x;" 'fdscript-comment-out-region)
  (define-key map "\C-x." 'fdscript-show-oid-at-point))

(cond ((not fdscript-mode-map)
       (setq fdscript-mode-map (copy-keymap scheme-mode-map))
       (fdscript-mode-commands fdscript-mode-map)))

(cond ((not inferior-fdscript-mode-map)
       (setq inferior-fdscript-mode-map (copy-keymap comint-mode-map))
       (scheme-mode-commands inferior-fdscript-mode-map)
       (fdscript-mode-commands inferior-fdscript-mode-map)
       (define-key inferior-fdscript-mode-map "\ep" 'fdscript-previous-input)
       (define-key inferior-fdscript-mode-map "\eg" 'fdscript-goto-input)
       (define-key inferior-fdscript-mode-map "\C-m" 'fdscript-smart-newline)
       (define-key inferior-fdscript-mode-map "\e\C-m" 'fdscript-smart-send)
       (define-key inferior-fdscript-mode-map "\C-cn" 'fdscript-narrow-to-input)
       (define-key inferior-fdscript-mode-map "\C-cq" 'fdscript-narrow-to-input)
       (define-key inferior-fdscript-mode-map "\C-c\C-z"
	 'fdscript-goto-process-mark)))

(defun start-fdscript (cmd)
  "Run an inferior FDScript process, input and output via buffer *fdscript*.
If there is a process already running in `*fdscript*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `fdscript-program-name').  Runs the hooks `inferior-fdscript-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run FDScript: " fdscript-program-name)
		       fdscript-program-name)))
  (if (not (comint-check-proc "*fdscript*"))
      (let ((cmdlist (convert-args-to-list cmd)))
	(save-excursion
	  (set-buffer (apply 'make-comint "fdscript" (car cmdlist)
			     nil (cdr cmdlist)))
	  (inferior-fdscript-mode))))
  (setq fdscript-program-name cmd)
  (setq fdscript-buffer "*fdscript*"))
;;;###autoload
(defun run-fdscript (cmd)
  (interactive (list (if current-prefix-arg
			 (read-string "Run FDScript: " fdscript-program-name)
		       fdscript-program-name)))
  (start-fdscript cmd)
  (pop-to-buffer "*fdscript*"))
;;;###autoload
(defalias 'fdscript 'run-fdscript)

(defun fdscript-mode-variables ()
  (setq comment-start-skip "\;+[ \t\.]*")
  (make-local-variable 'fill-prefix)
  (modify-syntax-entry ?{ "(")
  (modify-syntax-entry ?} ")")
  (setq major-mode 'fdscript-mode)
  (setq mode-name "FDX"))

(defun convert-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (convert-args-to-list (substring string (+ 1 where)
						 (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		 (convert-args-to-list (substring string pos
						 (length string)))))))))

;;;###autoload
(defun fdscript-mode ()
  "Goes into fdscript-mode."
  (interactive)
  (scheme-mode)
  (fdscript-mode-variables)
  (use-local-map fdscript-mode-map)
  (set-input-method default-input-method)
  (run-hooks 'scheme-mode-hook)
  (run-hooks 'fdscript-mode-hook))

(defun inferior-fdscript-mode ()
  "Major mode for interacting with an inferior FDScript process.

The following commands are available:
\\{inferior-fdscript-mode-map}

A Scheme process can be fired up with M-x fdscript or M-x run-fdscript.

Customisation: Entry to this mode runs the hooks on comint-mode-hook,
scheme-mode-hook, fdscript-mode-hook, inferior-scheme-mode-hook, 
and inferior-fdscript-mode-hook (in that order).

You can send text to the inferior Scheme process from other buffers containing
Scheme source.  

For information on running multiple processes in multiple buffers, see
documentation for variable scheme-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  ;; Customise in inferior-scheme-mode-hook
  (setq comint-prompt-regexp "^[^:\n]*:+ *")
  (scheme-mode-variables)
  (fdscript-mode-variables)
  (setq major-mode 'inferior-fdscript-mode)
  (setq mode-name "FDScript")
  (setq mode-line-process '(":%s"))
  (use-local-map inferior-fdscript-mode-map)
  (setq comint-input-filter (function fdscript-input-filter))
  (setq comint-get-old-input (function fdscript-get-input))
  (make-local-variable 'fdscript-prompt)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line))
  (run-hooks 'inferior-scheme-mode-hook)
  (run-hooks 'inferior-fdscript-mode-hook))

(setq auto-mode-alist
      (append '(("\\.fdx" . fdscript-mode)
		("\\.fdz" . fdscript-mode)
		("\\.fdcgi" . fdscript-mode))
	      auto-mode-alist))

;;; $Log: fdscript.el,v $
;;; Revision 1.20  2003/08/27 10:53:27  haase
;;; Merged 2.4 patches into trunk, started 2.5
;;;
;;; Revision 1.19.2.2  2002/09/29 11:35:39  haase
;;; Fixes to HTML generation and elisp indentations and installation
;;;
;;; Revision 1.19.2.1  2002/07/31 21:34:30  haase
;;; Some more indentation rules
;;;
;;; Revision 1.19  2002/07/23 21:40:58  haase
;;; Fixes for RPM building
;;;
;;; Revision 1.18  2002/07/16 18:53:14  haase
;;; Removed == tests, replaced with simple =
;;;
;;; Revision 1.17  2002/07/16 15:27:04  haase
;;; Fixed buffer module extraction for new in-module syntax
;;;
;;; Revision 1.16  2002/07/10 02:54:24  haase
;;; Tried to add fdscript.el with right mode info
;;;
;;; Revision 1.14  2002/07/10 02:50:18  haase
;;; Change to indentation, attempt to change mode
;;;
;;; Revision 1.13  2002/07/01 02:38:10  haase
;;; More attempts to fix indentation
;;;
;;; Revision 1.12  2002/06/24 14:56:42  haase
;;; Added more HTML generator indents
;;;
;;; Revision 1.11  2002/04/27 20:35:33  haase
;;; Added -MT macros to indentation rules
;;;
;;; Revision 1.10  2002/04/19 00:16:41  haase
;;; Added multiple-value-bind indentation
;;;
;;; Revision 1.9  2002/04/02 21:44:17  haase
;;; Added Id and log entries to fdscript.el
;;;
