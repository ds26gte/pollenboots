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
