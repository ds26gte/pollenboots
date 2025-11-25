#lang racket

(require txexpr)
(require pollen/core)
(require pollen/decode)

(require "utils.rkt")
(require "common-tags.rkt")

(provide (all-defined-out))

(define (gloss item)
  ; (printf "doing gloss ~s\n" item)
  (let ([item-sluggified (string-append (sluggify item) (get-counter))])
    `(span ()
           (a ([name ,item-sluggified]))
           (gloss-1 ,item-sluggified ,item))))

(define (output-glossary)
  `(output-glossary-1 ()))

;Glossary

(define (find-glossary-racket-file)
  (define here-path-source (select-from-metas 'here-path (current-metas)))
  (define here-path-html (regexp-replace "\\.poly.pm$" here-path-source ".html"))
  (define here-path-from-top-dir (from-top-dir here-path-html))
  (define top-dir (point-to-top-dir here-path-from-top-dir))
  (define glossary-racket-file (add-top-dir top-dir "glossary.rkt"))
  (values here-path-from-top-dir glossary-racket-file)
  )

(define (output-glossary-func)
  ; (printf "*** doing output-glossary-func\n")
  (define-values (_ glossary-racket-file) (find-glossary-racket-file))

  (define glossary-entries '())

  (when (file-exists? glossary-racket-file)
    (call-with-input-file glossary-racket-file
      (λ (i)
        (let loop ()
          (let ([x (read i)])
            (unless (eof-object? x)
              (set! glossary-entries (cons x glossary-entries))
              (loop)))))))

  ; (printf "final glossary-entries = ~s\n" glossary-entries)

  (define sorted-glossary
    (sort glossary-entries
          (λ (a b)
            (string<? (second a) (second b)))))

  (define glossary-items '())

  (for ([entry sorted-glossary])
    (set! glossary-items
      (cons `(li () (a ([href ,(second entry)]) ,(first entry)))
            glossary-items)))

  `(ul () ,@(reverse glossary-items))
  )

(define (glossary-handler doc)

  (define-values (here-path-from-top-dir glossary-racket-file) (find-glossary-racket-file))

  ; (printf "doing glossary in ~s\n" here-path-from-top-dir)

  ; (define reset-glossary-tags
  ;   (extract-tags doc '(output-glossary-1)))

  ; (when (pair? reset-glossary-tags)
  ;   ; (printf "*** delete old grf\n")
  ;   (when (file-exists? glossary-racket-file)
  ;     (delete-file glossary-racket-file)))

  (define glossary-entries  '())

  (define-values (doc-without-glossary-defs glossary-defs)
    (splitf-txexpr doc
      (λ (tx) (and (txexpr? tx) (eq? (get-tag tx) 'gloss-1)))))

  (for ([tx glossary-defs])
    (let* ([item-values (get-elements tx)]
           [item-sluggified (car item-values)]
           [item (cadr item-values)])
      (set! glossary-entries
        (cons (list item
                    (string-append here-path-from-top-dir "#" item-sluggified))
              glossary-entries))))

  (when (pair? glossary-entries)

    ; (printf "*** glossary-entries = ~s\n" glossary-entries)

    (call-with-output-file glossary-racket-file
      (λ (o)
        (for ([x glossary-entries])
          (write x o)
          (newline o)))
      #:exists 'append)

    ; (printf "glosses written\n")
    )

  (define (replace-output-glossary tx)
    (case (get-tag tx)
      [(output-glossary-1)
       (output-glossary-func)]
      [else tx]))

  (decode doc-without-glossary-defs #:txexpr-proc replace-output-glossary)
  )
