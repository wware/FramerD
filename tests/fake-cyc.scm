(define-if-needed regex-gather (autolink "fdtext"))

(define (remove-if-neccessary file)
  (when (file-exists? file) (remove-file file)))

(remove-if-neccessary "cyc.pool")
(remove-if-neccessary "cyc.pool.LCK")
(remove-if-neccessary "cyc.index")
(remove-if-neccessary "cyc.index.LCK")

(define cyc-pool (make-file-pool "cyc.pool" 16000 @17/0))
(define cyc-index (make-file-index "cyc.index" 64000))
(define cyc-indices cyc-index)
(define cyc-frames (amb))

(auto-cache-file-indices)
(auto-cache-file-pools)

(define cyc-source
  (frame-create cyc-pool
		'obj-name "CYC Upper Ontology, Copyright (C) Cyccorp 1997")) 

