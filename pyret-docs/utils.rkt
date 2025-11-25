#lang racket

(require txexpr)

(provide (all-defined-out))

(define (sluggify xxx)
  (string-replace xxx " " "-"))

(define counter 0)

(define (get-counter)
  (set! counter (+ counter 1))
  (string-append "-" (number->string counter)))

(define (change-tag tx from to)
  (define-values (tx1 _)
    (splitf-txexpr tx
      (λ (tx) (and (txexpr? tx) (eq? (get-tag tx) from)))
      (λ (tx) (txexpr to (get-attrs tx) (get-elements tx)))))
  tx1)

; (define (tag=? tx tag)
;   (and (txexpr? tx) (equal? (get-tag tx) tag)))

(define (extract-tags tx tags)
  (define-values (_ txs)
    (splitf-txexpr tx
      (λ (tx) (and (txexpr? tx)
                   (member (get-tag tx) tags)))))
  txs)

(define (remove-tag tx tag)
  (define-values (tx1 _)
    (splitf-txexpr tx
      (λ (tx) (and (txexpr? tx) (eq? (get-tag tx) tag)))))
  tx1)

(define *distinguishing-part-of-containing-directory* "pyret-docs/")

(define (from-top-dir pname)
  ; (printf "doing from-top-dir ~s\n" pname)
  (regexp-replace (string-append ".*" *distinguishing-part-of-containing-directory*)
                  pname ""))

(define (point-to-top-dir pname)
  (when (symbol? pname)
    (set! pname (symbol->string pname)))
  (let ([up-dir ""])
    (for ([c pname])
      (when (char=? c #\/) (set! up-dir (string-append up-dir "../"))))
    up-dir))

(define (add-top-dir up-dir pname)
  (when (symbol? pname)
    (set! pname (symbol->string pname)))
  (if (not pname) pname
      (format "~a~a" up-dir pname)))

(define (h-tag-at-depth n)
  (string->symbol (format "h~a" n)))
