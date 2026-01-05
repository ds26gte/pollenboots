#lang racket

(require txexpr)
(require pollen/core)
(require pollen/setup)

(provide (all-defined-out))


(define (sluggify* terms)
  (string-join (map sluggify terms) "-"))

(define (sluggify term)
  (let ([s ""])
    (cond [(string? term) (set! s term)]
          [(list? term) (set! s (sluggify* (rest term)))]
          [else (set! s "unnamed")])
    (string-replace s " " "-")))

(define counter 0)

(define (get-counter)
  (set! counter (+ counter 1))
  (string-append "-" (number->string counter)))

(define (change-tag tx from to)
  (define-values (tx1 _)
    (splitf-txexpr tx
      (lambda (tx) (and (txexpr? tx) (eq? (get-tag tx) from)))
      (lambda (tx) (txexpr to (get-attrs tx) (get-elements tx)))))
  tx1)

; (define (tag=? tx tag)
;   (and (txexpr? tx) (equal? (get-tag tx) tag)))

(define (extract-tags tx tags)
  ; (printf "doing extract-tags ~s ~s\n" tx tags)
  (define-values (_ txs)
    (splitf-txexpr tx
      (lambda (tx) (and (txexpr? tx)
                   (member (get-tag tx) tags)))))
  txs)

(define (remove-tag tx tag)
  (define-values (tx1 _)
    (splitf-txexpr tx
      (lambda (tx) (and (txexpr? tx) (eq? (get-tag tx) tag)))))
  tx1)

(define *distinguishing-part-of-containing-directory* "pyret-docs/")

(define (from-project-root pname)
  (regexp-replace (format ".*~a" *distinguishing-part-of-containing-directory*)
                  pname ""))

(define (to-project-root pname)
  ; (printf "*** to-project-root ~s\n" pname)
  (let ([x ""])
    (let loop ([i (- (string-length pname) 1)])
      (unless (< i 0)
        (when (char=? (string-ref pname i) #\/)
          (set! x (string-append x "../")))
        (loop (- i 1))))
    x))

(define (point-to-project-root pname)
  (when (symbol? pname)
    (set! pname (symbol->string pname)))
  (let ([up-dir ""])
    (for ([c pname])
      (when (char=? c #\/) (set! up-dir (string-append up-dir "../"))))
    up-dir))

(define (prefix-dir up-dir pname)
  (when (symbol? pname)
    (set! pname (symbol->string pname)))
  (if (not pname) pname
      (format "~a~a" up-dir pname)))

(define (h-tag-at-depth n)
  (string->symbol (format "h~a" n)))


(define (make-gloss item-alpha [item-sluggified #f] [item-typeset #f])
  (unless item-sluggified
    (set! item-sluggified (string-append (sluggify item-alpha) (get-counter))))
  (unless item-typeset
    (set! item-typeset item-alpha))
  `(span ()
         (a ([name ,item-sluggified]))
         (gloss-1 ,item-alpha ,item-sluggified ,item-typeset)))

(define (ref-gloss item-alpha [item #f] #:mod [mod #f])
  (unless mod (set! mod "nodoc"))
  (unless item (set! item item-alpha))
  ; (printf "*** ref-gloss ~s ~s\n" item-alpha item)
  `(ref-gloss-1 () ,item-alpha ,mod ,item))

(define (ref-mod-gloss mod item)
  (ref-gloss item #:mod mod))

; (define (in-link item)
;   (printf "*** in-link ~s\n" item)
;   `(xxref-1 () ,item))

;true globals

(define *project-root* (current-project-root))

(define *globals-file* (build-path *project-root* "globals.rkt"))

(define (read-globals)
  (if (file-exists? *globals-file*)
      (call-with-input-file *globals-file* read)
      '()))

(define (calc-here-path-from-project-root)

  (define here-path-source (select-from-metas 'here-path (current-metas)))
  (define here-path-html (regexp-replace "\\.poly.pm$" here-path-source ".html"))
  (define here-path-from-project-root (from-project-root here-path-html))

  here-path-from-project-root)

(define (write-globals)
  ; (printf "*** write-globals\n")
  (define *saved-items* '(glossary xref))
  (call-with-output-file *globals-file*
    (lambda (o)
      (fprintf o "(\n")
      (for ([item *saved-items*])
        (define item-file (build-path *project-root* (format "_~a.rkt" item)))
        (when (file-exists? item-file)
          (fprintf o "(~a\n" item)
          (call-with-input-file item-file
            (lambda (i)
              (let loop ()
                (let ([x (read i)])
                  (unless (eof-object? x)
                    (write x o) (newline o)
                    (loop))))))
          (delete-file item-file)
          (fprintf o ")\n")
          ))
      (fprintf o ")\n")

      )
    #:exists 'replace))

(define pollen-postlude write-globals)
