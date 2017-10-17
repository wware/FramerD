;; Handling inverses
;;  Key problem is that we don't want to have to modify a concept
;;   to refer to it, and if we want to store indices directly, we'll
;;   end up doing that
;;  Here's a strategy: (i)
;;    concepts are in pools and we use being in the same pool as
;;    a test of whether we store a slot
;;  Here's another: (ii)
;;    we have the notion of a "current pool" and we use slots to
;;    within the current pool and search otherwise
;;  Here's another: (iii)
;;    we declare the "core pool" and use slots within it

;; For all of these, we need to do a search anyway, since we could have
;;  semantic plugins which will affect the higher level predicates;
;;  thus, we are only making a decision about whether or not we
;;  *store* the inverse.  For that, strategy (i) is probably a good
;;  one, which lets us be fast in our local ontology.

;; How do we actually make things be fast?  Well, we need to do a
;; search in any case, because of plugins.  So we can only really be
;; fast on tests (gets will do the search anyway).  To do that, we
;; will want to have OR clauses in the test methods which first do
;; slot gets and then do searches.

;; Question: do we want to have all slots possibly use searches?  This
;; would let us add (for instance) genls assertions to the core ontology.

;;; Slot inferences needed by CYC
;; isa
;;   tests through genls
;;   gathers through search and specls
;; instances
;;   tests through isa slots and genls
;;   gathers through isa search and specls
;; genls
;;   (no!) asserts speczns
;;   test is transitive closure
;; specls
;;   gathers through slots and search
;; hasattributes
;;   asserted by attribute slots
;;   tests through genlattributes
;; isnot
;;   who knows?
;; arg1isa
;;   used to validate binary predicates
;; arg1genl...
;;   when arguments are collections, this requires some genls
;; arg2isa
;; arg3isa
;; arg4isa
;; arg5isa
;; arg6isa
;; argsisa
;; argsgenl
;; genlPreds
;;   One implies the other, use to check assertions, but do the
;;    inverse assertion lazily (?)
;; specznPreds
;;   To test X, get specznPreds and try them
;; genlInverse
;;   One asserts the other with arguments inverted
;; genlAttributes

;;; Mapping predicates into frames
;;; Unary predicates are handled by isa/instances
;;; Binary predicates are handled by slots
;;; (Attributes are like enums used as arg2isas for binary predicates)
;;; N-Ary predicates are handled as follows:
;;;   P(a1,a2,...) produces
;;;   a slot SP, a collection CP, and slotids P-arg1, P-arg2, ...
;;; SP(a1) = x, x isa CP, P-arg1(x)=a1, P-arg2(x)=a2, ...
;;; Therefore, SP is the inverse of P-arg1

;;; Preface
;(define cyc-pool (use-pool "cyc.pool"))
;(define cyc-index (use-index "cyc.index"))
;(define cyc-indices cyc-index)
;(load "use-cyc.scm")

;;; genls
(define genls (find-frames cyc-indices 'cyc-name 'genls))
(define specls
  (try (find-frames cyc-indices 'cyc-name 'specls)
       (frame-create cyc-pool
	   'obj-name "specls" 'cyc-name 'specls
	   'kif-name 'subclasses)))
(define isa (find-frames cyc-indices 'cyc-name 'isa))
(define instances
  (try (find-frames cyc-indices 'cyc-name 'instances)
       (frame-create cyc-pool
	   'obj-name '"instances" 'cyc-name 'instances
	   'kif-name 'instances)))
(define has-attributes (find-frames cyc-indices 'cyc-name 'hasattributes))
(define collection-inits
  (try (find-frames cyc-indices 'cyc-name 'hasattributes)
       (frame-create cyc-pool
	   'obj-name '|CollectionInits|
	   'cyc-name 'collectioninits
	   'kif-name 'collection-inits)))
;  get-methods
;    data
;    (fget data slotid)
;    (find-frames cyc-indices specls frame)
;    (fget (find-frames cyc-indices specls frame) slotid)
(fadd! genls 'get-methods 'data)
(fadd! genls 'get-methods '(fget data slotid))
(fadd! genls 'get-methods `(find-frames cyc-indices ,specls slotid))
(fadd! genls 'get-methods `(fget (find-frames cyc-indices ,specls slotid)
				 slotid))
;  test-methods
;    (or (contains? value data)
;        (exists (x data) (ftest x slotid value))
;	(exists (x (find-frames cyc-indices specls frame))
;          (ftest x slotid value)))
(fadd! genls 'test-methods
       `(or (contains? value data)
	    (exists (x data) (ftest x slotid value))
	    (exists (x (find-frames cyc-indices ,specls frame))
		    (or (eq? x value) (ftest x slotid value)))))
;  add-effects
;    (if (same-pool? frame value)
;        (fadd! value specls frame))
;    (index-add! cyc-indices (cons slotid value) frame)
(fadd! genls 'add-effects       
       `(if (same-pool? frame value)
	    (fadd! value ,specls frame)))
;;; specls
;  get-methods
;    data
;    (fget data slotid)
;    (find-frames cyc-indices genls frame)
;    (fget (find-frames cyc-indices genls frame) slotid)
(fadd! specls 'get-methods 'data)
(fadd! specls 'get-methods '(fget data slotid))
(fadd! specls 'get-methods `(find-frames cyc-indices ,genls frame))
(fadd! specls 'get-methods `(fget (find-frames cyc-indices ,genls frame)
				 slotid))
;  test-methods
;    (or (contains? value data)
;        (exists (x data) (ftest x slotid value)))
(fadd! specls 'test-methods
       `(or (contains? value data)
	    (exists (x data) (ftest x slotid value))
	    (exists (x (find-frames cyc-indices ,genls frame))
		    (or (eq? x value) (ftest x slotid value)))))
;  add-effects
;    (if (same-pool? frame value)
;        (fadd! value genls frame))
;    (index-add! cyc-indices (cons slotid value) frame)
(fadd! specls 'add-effects       
       `(if (same-pool? frame value)
	    (fadd! value ,genls frame)))
;;; isa
;  get-methods
;    data (fget data genls)
;    (find-frames cyc-indices instances frame)
;    (fget (find-frames cyc-indices instances frame) genls)
(fadd! isa 'get-methods 'data)
(fadd! isa 'get-methods `(fget data ,genls))
(fadd! isa 'get-methods
       `(find-frames cyc-indices ,instances frame))
(fadd! isa 'get-methods
       `(fget (find-frames cyc-indices ,instances frame) ,genls))
;  test-methods
;    (or (contains? value data)
;	 (ftest frame slotid (fget value genls))
;        (contains? value (find-frames cyc-indices instances frame))
;        (ftest frame slotid 
;              (fget (find-frames cyc-indices instances frame) genls)))
(fadd! isa 'test-methods
       `(or (contains? value data)
	    (ftest frame slotid (fget value ,genls))
	    (contains? value (find-frames cyc-indices ,instances frame))
	    (ftest frame slotid 
		  (fget (find-frames cyc-indices ,instances frame) ,genls))))
;  add-methods
;    (do-choices (each (fget value collection-inits))
;      (frame-add! frame (car each) (cdr each)))
(fadd! isa 'add-methods
       `(do-choices (each (fget value ,collection-inits))
	  (frame-add! frame (car each) (cdr each))))
;;; instances
;  get-methods
;    data 
;    (fget (fget frame specls) instances)
;    (find-frames cyc-indices isa frame)
;    (find-frames cyc-indices isa (fget frame specls))
(fadd! instances 'get-methods 'data)
(fadd! instances 'get-methods `(fget (fget frame specls) ,instances))
(fadd! instances 'get-methods
       `(find-frames cyc-indices ,isa frame))
(fadd! instances 'get-methods
       `(find-frames cyc-indices ,isa (fget frame ,specls)))
;  test-methods
;    (or (contains? value data)
;	 (ftest (fget frame genls) slotid value)
;        (contains? value (find-frames cyc-indices isa frame))
;        (ftest (find-frames cyc-indices isa frame) slotid value))
(fadd! instances 'test-methods
       `(or (contains? value data)
	    (ftest (fget frame ,genls) slotid value)
	    (contains? value (find-frames cyc-indices ,isa frame))
	    (ftest (find-frames cyc-indices ,isa frame) slotid value)))
;  add-effects
;    (if (in-same-pool? frame value)
;        (fadd! value isa frame)
;      (do-choices (each (fget frame collection-inits))
;        (frame-add! value (car each) (cdr each))))
;    (index-add! cyc-index (cons slotid value) frame)
(fadd! instances 'add-effects
       `(if (in-same-pool? frame value) (fadd! value ,isa frame)))
(fadd! instances 'add-effects
       `(do-choices (each (fget frame ,collection-inits))
	  (frame-add! value (car each) (cdr each))))
;;; has-attributes
;  get-methods
;  test-methods
;  add-effects
;  (index-add! cyc-index (cons slotid value) frame)
;;; collection-inits
;   get-methods
;    data (fget (fget frame genls) slotid)
(fadd! collection-inits 'get-methods 'data)
(fadd! collection-inits 'get-methods `(fget (fget frame ,genls) slotid))
;   add-methods
;    (frame-add! (fget frame instances)
;		 (car value) (cdr value))
;   remove-methods
;    (frame-remove! (fget frame instances)
; 		    (car value) (cdr value))
(fadd! collection-inits 'add-effects
       `(frame-add! (fget frame ,instances) (car value) (cdr value)))
(fadd! collection-inits 'remove-effects
       `(frame-remove! (fget frame ,instances) (car value) (cdr value)))
;
;;; general slotid fcns
;
;   get-methods
;     data
;     (fget frame (fget slotid speclpreds))
;     (find-frames cyc-indices (pget slotid speclsinverse) frame)
;   test-methods
;     (or (contains? value data)
;	 (ftest frame (fget slotid genlpreds) value))
;   add-effects
;     (index-add! cyc-index (cons slotid value) frame)
;   validate-method
;     (ftest frame isa (fget slotid arg1isa))
;     (ftest frame genls (fget slotid arg1genls))
;     (ftest value isa (fget slotid resultisa))
;     (ftest value genls (fget slotid resultgenls))

;(frame-add #$foo)  

