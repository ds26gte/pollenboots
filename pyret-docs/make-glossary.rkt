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
  ; using, for now: tag plainseq entryseq
  ; (printf "*** make-index-element ~s ~s ~s\n" tag plainseq entryseq)
  (define alpha-tag (first plainseq))
  (define tag-1 (second tag))
  (make-gloss alpha-tag tag-1 (first entryseq)))

(define gloss make-gloss)

(define (custom-index-block)
  `(output-glossary-1 ()))

;Glossary



(define (glossary-handler doc)

  (define curr-mod "nodoc")

  (define here-path-from-project-root (calc-here-path-from-project-root))

  (define project-root-from-here-path (to-project-root here-path-from-project-root))

  (define glossary-entries '())

  (define-values (doc-without-module-tag module-tag)
    (splitf-txexpr doc
      (lambda (tx) (and (txexpr? tx) (eq? (get-tag tx) 'module-tag-1)))))

  (unless (null? module-tag)
    (set! curr-mod
      (car (get-elements (car module-tag)))))

  (set! doc doc-without-module-tag)

  (define-values (doc-without-glossary-defs glossary-defs)
    (splitf-txexpr doc
      (lambda (tx) (and (txexpr? tx) (eq? (get-tag tx) 'gloss-1)))))

  ; (printf "*** here-path-from-project-root = ~s\n" here-path-from-project-root)
  ; (printf "*** project-root-from-here-path = ~s\n" project-root-from-here-path)

  (for ([tx glossary-defs])
    (let* ([item-values (get-elements tx)]
           [item-alpha (first item-values)]
           [item-sluggified (second item-values)]
           [item (third item-values)])
      (set! glossary-entries
        (cons (list (string-append curr-mod ":" item-alpha) item-alpha item
                    (string-append here-path-from-project-root "#" item-sluggified))
              glossary-entries))))

  (when (pair? glossary-entries)

    (call-with-output-file (build-path *project-root* "_glossary.rkt")
      (lambda (o)
        (for ([x glossary-entries])
          (write x o)
          (newline o)))
      #:exists 'append)

    )

  (define *sorted-glossary* '())

  (define (read-glossary)
    (define globals-list (read-globals))

    (let ([a (assoc 'glossary globals-list)])
      (when a (set! *sorted-glossary* (cdr a))))

    )

  (read-glossary)

  (define (output-glossary-func)

    (define glossary-items '())

    ; (printf "*** IV\n")

    (for ([entry *sorted-glossary*])
      (set! glossary-items
        (cons `(li () (a ([href ,(fourth entry)]) ,(third entry)))
              glossary-items)))

    `(ul () ,@(reverse glossary-items))

    )

  (define (output-ref-gloss xx)
    ; (printf "*** output-ref-gloss ~s ~s\n" xx curr-mod)
    ; (printf "*** *sorted-glossary* = ~s\n" *sorted-glossary*)
    (let* ([item-alpha (first xx)]
           [item-mod (let ([y (second xx)])
                       (if (string=? y "nodoc") curr-mod y))]
           [item (third xx)]
           [mod:item (string-append item-mod ":" item-alpha)]
           [items-gloss (assoc mod:item *sorted-glossary*)]
           [href (if (list? items-gloss) (fourth items-gloss) "missing_gloss")]
           )
      ; (printf "(*** mod:item = ~s\n" mod:item)
      (set! href (string-append project-root-from-here-path href))
      ; (printf "*** items-gloss = ~s\n" items-gloss)
      ; (printf "*** href = ~s\n" href)
      `(a ([href ,href]) ,item)))

  (define (process-glossary tx)
    (case (get-tag tx)
      [(output-glossary-1)
       (output-glossary-func)]
      [(ref-gloss-1) (output-ref-gloss (get-elements tx))]
      [else tx]))

  (decode doc-without-glossary-defs #:txexpr-proc process-glossary)
  )
