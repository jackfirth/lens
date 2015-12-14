#lang racket/base

(provide id-append)

(require racket/list
         racket/syntax
         syntax/srcloc)

;; orig : Syntax -> Syntax
(define (orig stx)
  (syntax-property stx 'original-for-check-syntax #t))

;; Sub-Range-Binder-Prop = (Treeof (Vector Id Nat Nat Real Real Id Nat Nat Real Real))
;; Binder-Proc = Id -> Sub-Range-Binder-Prop

;; make-binder-proc : Id Nat -> Binder-Proc
(define ((make-binder-proc base n) id)
  (vector (syntax-local-introduce id)
          n (syntax-span base) 0.5 0.5
          (syntax-local-introduce base)
          0 (syntax-span base) 0.5 0.5))

;; get-sub-range-binders : Id (Listof Binder-Proc) -> Sub-Range-Binder-Prop
(define (get-sub-range-binders id binder-procs)
  (for/list ([binder-proc (in-list binder-procs)])
    (binder-proc id)))

;; empty-id : Syntax -> Id
(define (empty-id ctxt)
  (datum->syntax ctxt '||))

(define appended-id-prop (gensym 'appended-id))

;; id-append : #:context Syntax Identifier ... -> (values Identifier Sub-Range-Binder-Prop)
;; a wrapper around id-append* that keeps track of identifiers that
;; are themselves appended from other identifiers
(define (id-append #:context ctxt . ids)
  (define ids*
    (append*
     (for/list ([id (in-list ids)])
       ;; appended : (U #false (Listof Id))
       (define appended (syntax-property id appended-id-prop))
       (cond [appended appended]
             [else (list id)]))))
  (define-values [id sub-range-binders]
    (apply id-append* #:context ctxt ids*))
  (values (syntax-property id appended-id-prop ids*)
          sub-range-binders))

;; id-append* : #:context Syntax Identifier ... -> (values Identifier Sub-Range-Binder-Prop)
(define (id-append* #:context ctxt . ids)
  ;; binder-procs : (Listof Binder-Proc)
  (define-values [id n binder-procs]
    (for/fold ([id1 (empty-id ctxt)] [n 0] [binder-procs '()])
              ([id2 (in-list ids)])
      (values (format-id ctxt "~a~a" id1 id2)
              (+ n (syntax-span id2))
              (cons (make-binder-proc id2 n) binder-procs))))
  (define id* (orig id))
  (values id*
          (get-sub-range-binders id* binder-procs)))


      

