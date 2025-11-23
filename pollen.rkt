#lang racket

(require txexpr)
(require pollen/decode)
(require pollen/misc/tutorial)
(require pollen/tag)
(require racket/date)

(provide (all-defined-out))

(define (single-newline-is-just-space x)
  (cond [(and (txexpr? x) (member (get-tag x) '(br))) " "]
        [(list? x) (map single-newline-is-just-space x)]
        [else x]))

(define (root . elts)
  (txexpr 'root empty
          (single-newline-is-just-space
            (decode-elements elts
                             #:txexpr-elements-proc decode-paragraphs
                             #:string-proc (compose1 smart-quotes smart-dashes)))))

(define (strong . elts)
  ;render strong as em
  (txexpr 'em empty elts))

(define author "ds26gte")
(define-tag-function (br attrs elts)
                     `(span ,attrs ,@elts))

(define-tag-function (strong-og attrs elts)
                     `(strong ,attrs ,@elts))

(define-tag-function (new-em attrs elts)
                     `(em ,attrs ,@elts))

(define ul
  (default-tag-function 'ul #:class "list-group"))

(define li
  (default-tag-function 'li #:class "list-group-item"))

(define (get-date)
  (date->string (current-date)))

(define (hyperlink url . elts)
  `(a ((href ,url)) ,@elts))

(define link hyperlink)
