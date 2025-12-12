#lang racket

(provide (all-defined-out))

(define (pyret #:style [style #f] . elems)
  `(tt ([class "pyretexpr"]) ,@elems))

(define pyret-id pyret)
(define tt pyret)

(define (examples #:show-try-it [show-try-it #f] . elems)
  (if show-try-it
      `(div ()
            (pre () ,@elems)
            (a ([class "show-embed"]
                [code ,(string-join elems " ")])
               "(Try it!)"))
      `(pre () ,@elems)))

(define (verbatim #:style [style "nothing_special"] #:show-try-it [show-try-it #f] . elems)
  ; (printf "@@@ doing verbatim ~s\n" elems)
  (define attribs `([class ,style]))
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

(define (function #:contract [contract ""] #:args [args #f]
                  #:return [return "return"]
                  #:examples [examples "examples"]
                  #:alt-docstrings [alt-docstrings "alt-docstrings"]
                  name . elems)
  ; (printf "function ~a args are ~s, contract = ~s\n" name args contract)
  `(div ()
        (pre ([class "pyret-display"])
             ,name " :: "
             ,(if args
                  `(span ()
                        "(" ,@(add-between (map (λ (arg)
                                                  (let ([arg (first arg)]
                                                        [type (second arg)])
                                                    ; (printf "arg/type are ~s, ~s\n" arg type)
                                                    (cond [type
                                                            `(span () ,arg " :: " ,type)]
                                                          [(list? contract)
                                                           ; (printf "contract = ~s\n" contract)
                                                           (let ([n (length contract)])
                                                             (let ([res
                                                                     (if (>= n 3)
                                                                         `(span () ,arg " -> " ,(third contract))
                                                                         `(span () "() -> " ,(second contract)))])
                                                               ; (printf "done V\n")
                                                               res
                                                               ))]
                                                          [else
                                                            arg])))
                                                args) ", ")
                        ")"
                        )
                  contract))
        ,@elems))
