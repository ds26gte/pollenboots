#lang racket

(require txexpr)
(require pollen/decode)
(require pollen/misc/tutorial)
(require pollen/tag)

(provide (all-defined-out))

(define (root . elts)
  (txexpr 'root empty
          (decode-elements elts
                           #:txexpr-elements-proc decode-paragraphs
                           #:string-proc (compose1 smart-quotes smart-dashes))))

(define (strong . elts)
  ;render strong as em
  (txexpr 'em empty elts))

(define-tag-function (strong-og attrs elts)
                     `(strong ,attrs ,@elts))

(define-tag-function (new-em attrs elts)
                     `(em ,attrs ,@elts))


(define ul
  (default-tag-function 'ul #:class "list-group"))

(define li
  (default-tag-function 'li #:class "list-group-item"))

