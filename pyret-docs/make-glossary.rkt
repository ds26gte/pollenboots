#lang racket

(require txexpr)
(require pollen/core)
(require pollen/decode)

(require "utils.rkt")
(require "common-tags.rkt")

(provide (all-defined-out))

(define (make-generated-tag)
  (string-append "generated-tag" (get-counter)))

(define (make-index-element style content tag plainseq entryseq desc)
  ; (printf "*** make-index-element ~s ~s ~s ~s ~s ~s\n" style content tag plainseq entryseq desc)
  (define alpha-tag (first plainseq))
  (define tag-1 (second tag))
  `(span ()
         (a ([name ,tag-1]))
         (gloss-1 ,alpha-tag ,tag-1 ,(first entryseq))))

(define (gloss item)
  (let ([item-sluggified (string-append (sluggify item) (get-counter))])
    `(span ()
           (a ([name ,item-sluggified]))
           (gloss-1 ,item ,item-sluggified ,item))))

(define (output-glossary)
  `(output-glossary-1 ()))

;Glossary

(define (output-glossary-func)

  (define *globals-list* (read-globals))

  (define glossary-entries (let ([a (assoc 'glossary *globals-list*)])
                             (if a (cdr a) empty)))

  (define sorted-glossary
    (sort glossary-entries
          (λ (a b)
            (string<? (first a) (first b)))))

  (define glossary-items '())

  ; (printf "*** IV\n")

  (for ([entry sorted-glossary])
    (set! glossary-items
      (cons `(li () (a ([href ,(second entry)]) ,(first entry)))
            glossary-items)))

  `(ul () ,@(reverse glossary-items))
  )

(define (glossary-handler doc)

  (define here-path-from-project-root (calc-here-path-from-project-root))

  (define glossary-entries empty)

  (define-values (doc-without-glossary-defs glossary-defs)
    (splitf-txexpr doc
      (λ (tx) (and (txexpr? tx) (eq? (get-tag tx) 'gloss-1)))))

  (for ([tx glossary-defs])
    (let* ([item-values (get-elements tx)]
           [item-sluggified (second item-values)]
           [item (third item-values)])
      (set! glossary-entries
        (cons (list item
                    (string-append here-path-from-project-root "#" item-sluggified))
              glossary-entries))))

  (when (pair? glossary-entries)

    (call-with-output-file (build-path *project-root* "_glossary.rkt")
      (λ (o)
        (for ([x glossary-entries])
          (write x o)
          (newline o)))
      #:exists 'append)

    )

  (define (replace-output-glossary tx)
    (case (get-tag tx)
      [(output-glossary-1)
       (output-glossary-func)]
      [else tx]))

  (decode doc-without-glossary-defs #:txexpr-proc replace-output-glossary)
  )
