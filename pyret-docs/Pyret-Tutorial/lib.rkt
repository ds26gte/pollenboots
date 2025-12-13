#lang racket/base

(require "../common-tags.rkt")

(provide [all-defined-out])

;;;; <LOCAL ADDITIONS>

(define (show-url u) (hyperlink u u))
