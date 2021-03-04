#lang racket/base
(require racket/stxparam
         (for-syntax racket/base))

(provide define-monad
         do-it
         return)

(begin-for-syntax
  (struct monad (return-id bind-id)))

(define-syntax (define-monad stx)
  (syntax-case stx ()
    [(_ id
        #:return return-proc-expr
        #:bind bind-proc-expr)
     #'(begin
         (define m-return return-proc-expr)
         (define m-bind bind-proc-expr)
         (define-syntax id (monad #'m-return #'m-bind)))]))

(define-syntax-parameter return
  (lambda (stx)
    (raise-syntax-error #f "illegal outisde of `do-it`" stx)))

(define-syntax (do-it stx)
  (syntax-case stx ()
    [(_ id . clauses)
     (let ([m (and (identifier? #'id)
                   (syntax-local-value #'id (lambda () #f)))])
       (unless (monad? m)
         (raise-syntax-error #f "not a monad" stx #'id))
       #`(syntax-parameterize ([return (make-rename-transformer
                                        (quote-syntax #,(monad-return-id m)))])
           #,(let parse ([clauses #'clauses])
               (syntax-case clauses (define define* when unless)
                 [((define* id rhs) . rest)
                  #`(#,(monad-bind-id m)
                     rhs
                     (lambda (id)
                       #,(parse #'rest)))]
                 [((define id rhs) . rest)
                  #`(let ([id rhs])
                      #,(parse #'rest))]
                 [(expr)
                  #`expr]
                 [(expr . rest)
                  #`(#,(monad-bind-id m)
                     expr
                     (lambda (dummy)
                       #,(parse #'rest)))]))))]))
    
