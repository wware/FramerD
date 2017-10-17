(defun select-strings-with-suffix (stringlist suffix)
  (let ((len (length suffix)) (strings '()))
    (while (consp stringlist)
      (let* ((string (car stringlist)) (slen (length string)))
	(if (and (> (length string) len)
		 (equal suffix (substring string (- slen len))))
	    (setq strings (cons string strings))))
      (setq stringlist (cdr stringlist)))
    strings))
(defun select-shortest-string (stringlist)
  (let* ((shortest (car stringlist)) (len (length shortest)))
    (while (consp stringlist)
      (if (< (length (car stringlist)) len)
	  (setq shortest (car stringlist) len (length shortest)))
      (setq stringlist (cdr stringlist)))
    shortest))
(defun find-elisp-dir ()
  (if (locate-library "fdscript")
      (file-name-directory (locate-library "fdscript"))
      (select-shortest-string
       (or (select-strings-with-suffix load-path "site-lisp")
	   (select-strings-with-suffix load-path "site-lisp/")
	   (select-strings-with-suffix load-path "lisp")
	   (select-strings-with-suffix load-path "lisp/")))))
(defun fdscript-compile ()
  (byte-compile-file "etc/fdscript.el"))
(defun fdscript-install ()
  (let ((elisp-dir (find-elisp-dir)))
    (message "Copying files to %s" elisp-dir)
    (copy-file "etc/fdscript.el" (concat elisp-dir "/fdscript.el") t)
    (copy-file "etc/fdscript.elc" (concat elisp-dir "/fdscript.elc")
	       t)))
