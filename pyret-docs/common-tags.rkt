#lang racket

(require pollen/tag)

(provide (all-defined-out))

(define ul
  (default-tag-function 'ul #:class "list-group"))

(define li
  (default-tag-function 'li #:class "list-group-item"))

(define (nested . elts)
  `(p () ,@elts))

(define (hyperlink url . elts)
  `(a ((href ,url)) ,@elts))

(define link hyperlink)

(define (image file)
  `(img ([src ,file])))

(define (pyret . elems)
  `(tt ([class "pyretexpr"]) ,@elems))

(define pyret-id pyret)
(define tt pyret)

(define (examples #:show-try-it [show-try-it #f] . elems)
  `(pre () ,@elems))

(define (function #:contract [contract "contract"] #:return [return "return"] #:alt-docstrings [alt-docstrings "alt-docstrings"] . elems)
  ; (printf "function elems are ~s\n" elems)
  `(div ([class "function"]) ,@elems " :: " ,contract))

(define (form a b . elems)
  ; (printf "doing form a = ~s\nb = ~s\nelems = ~s\n" a b elems)
  `(div ()
        (div ([class "function"]) ,b)
        ,@elems))


(define (type-spec type-name . elems)
  `(div ()
        (div ([class "function"]) ,type-name)
        ,@elems))

(define (a-arrow . elems)
  (let* ([n (length elems)]
        [range (last elems)]
        [domain (take elems (- n 1))])
    `(span () "(" ,(string-join domain ", ") ")" " -> " ,range)))

(define (a-app base . typs)
  (string-append base "<" (string-join typs ", ") ">"))

; (define (a-arrow from to)
;   `(span () ,from " -> " ,to))

(define A "Any")
(define N "Number")
(define EN "Exactnum")
(define RN "Roughnum")
(define S "String")
(define No "Nothing")
(define B "Boolean")

(define (L-of typ) (a-app "List" typ))

(define eq "EqualityResult")
(define eqfun `(a-arrow ,A ,A ,B))
(define eq3fun `(a-arrow ,A ,A ,eq))
