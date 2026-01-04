#lang racket

(provide (all-defined-out))

(define (pyret #:style [style #f] . elems)
  `(tt ([class "pyretexpr"]) ,@elems))

(define pyret-id pyret)
(define tt pyret)

(define (examples #:show-try-it [show-try-it #f] . elems)
  (if show-try-it
      `(div ()
            (p () (b () "Examples:"))
            (pre () ,@elems)
            (a ([class "show-embed"]
                [code ,(string-join elems " ")])
               "(Try it!)"))
      `(div ()
            (p () (b () "Examples:"))
            (pre () ,@elems))))

(define (verbatim #:style [style "nothing_special"] #:show-try-it [show-try-it #f] . elems)
  ; (printf "@@@ doing verbatim ~s\n" elems)
  (define attribs `([class ,style]))
  `(pre ,attribs ,@elems))

(define codedisp verbatim)

(define pyret-block verbatim)

(define (data-spec name . elems)
  ; (printf "*** data-spec ~s ~s \n" name elems)
  `(div ()
        (pre () "data-spec")
        ,@elems))

(define (variants . elems)
  `(div ()
        ,@elems))

(define (shared . elems)
  `(div ()
        ,@elems))

(define (constr-spec name . elems)
  `(div ()
        (pre () ,name)
        ,@elems))

(define (members . elems)
  `(div ()
        ,@elems))

(define (with-members . elems)
  `(div ()
        ,@elems))

(define (member-spec #:type [type #f] #:contract [contract #f] fname . elems)
  `(div ()
        (pre () ,fname)
        ,@elems))

; (define (data-spec name type-vars variants shared)
;   ; (printf "*** data-spec ~s ~s ~s \n" name type-varss variants  shared )
;   `(pre () (tt () ,(format "~a~a:"
;                     name
;                     (if deps (format "<~a>" (add-between deps ", ")) "")))
;           "\n"
;           (div ()
;                 ,@(add-between
;                     (map
;                       (lambda (clause)
;                         `(tt () "   | " ,clause))
;                       clauses) "\n"))
;           (tt () "end")))

(define (data-spec2 #:no-toc [no-toc #f] name deps clauses)
  ; (printf "*** doing data-spec2 ~s deps=~s ~s\n" name deps clauses)
  `(pre ([class "pyret-display"])
        (span () ,(format "data ~a~a:"
                    name
                    ; (make-gloss name)
                    (if (and deps (cons? deps))
                        (format "<~a>" (apply string-append (add-between deps ", ")))
                        "")))
          "\n"
         (div ()
                ,@(add-between
                    (map
                      (lambda (clause)
                        `(tt () "   | " ,clause))
                      clauses) "\n"))
          (tt () "end")))

(define (singleton-spec2 cname name)
  `(span () ,name))

(define (constructor-spec cname name args)
  ; (printf "*** constructor-spec cname= ~s name= ~s args= ~s\n" cname name args)
  (let ([x
          `(span () ,name
                 "(" ,@(add-between
                         (map (lambda (arg)
                                (define fname (first arg))
                                (define contract (second (third arg)))
                                ; (printf "fname= ~s contract= ~s\n" fname contract)
                                `(span () ,fname " :: " ,contract))
                              args)
                         ", ")
                 ")")])
    ; (printf "*** x= ~s\n" x)
    x))

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
                        "(" ,@(add-between (map (lambda (arg)
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
                                                                         `(span () ,arg " :: " ,(third contract))
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
