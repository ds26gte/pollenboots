#lang racket

(provide (all-defined-out))

(define (pyret #:style [style #f] . elems)
  `(tt ([class "pyretexpr"]) ,@elems))

(define pyret-id pyret)
(define tt pyret)

(define (examples #:show-try-it [show-try-it #f] . elems)
  `(pre () ,@elems))

(define (verbatim #:style [style #f] #:show-try-it [show-try-it #f] . elems)
  ; (printf "@@@ doing verbatim ~s\n" elems)
  (define attribs
    (if style `([class ,style]) `()))
  `(pre ,attribs ,@elems))

(define codedisp verbatim)

(define pyret-block verbatim)

(define (data-spec2 name deps clauses)
  ; (printf "doing data-spec3 ~s ~s ~s\n" name deps clauses)
  `(pre () (tt () ,(format "~a~a:"
                    name
                    (if deps (format "<~a>" (add-between deps ", ")) "")))
          "\n"
          (div ()
                ,@(add-between
                    (map
                      (λ (clause)
                        `(tt () "   | " ,clause))
                      clauses) "\n"))
          (tt () "end")))

(define (singleton-spec2 cname name)
  `(span () "| " ,name))

(define (constructor-spec cname name args)
  `(span () ,name
         "(" ,@(add-between
                 (map (λ (arg) `(span () ,(first arg) " :: "
                                         ,(second (third arg))))
                      args)
                 ", ")
         ")"))
