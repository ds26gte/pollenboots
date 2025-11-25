#lang racket

(require txexpr)
(require pollen/core)
(require pollen/decode)

(require "utils.rkt")

(provide (all-defined-out))

;ToC

(define (table-of-contents)
  ; (txexpr 'table-of-contents-1 '() '())
  `(table-of-contents-1 ())
  )

; ToDo: if table-of-contents called, don't insert include-section bodily, otherwise do

(define (toc-handler doc)
  ; (printf "** doing toc-handler ~s \n"  false )

  ; (printf "$$ resetting toc-entries\n")
  (define toc-entries (make-parameter '()))

  (define (collect-toc-entries doc)
    ; (printf "doing collect-toc-entries ~s\n"  false )

    (define (collect-toc-entries-from-include-section file)
      ; (printf "doing collect-toc-entries-from-include-section ~s\n" file)
      (let* ([basename (regexp-replace "\\.poly.pm$" file "")]
             [html-filename (string-append basename ".html")]
             [idoc (get-doc file)])
        (define-values (_ section-txs)
          (splitf-txexpr idoc
            (λ (x) (and (txexpr? x)
                        (string=? (attr-ref x 'tocentry "no") "yes")))))
        (for ([tx section-txs])
          (let ([level (attr-ref tx 'toclevel)]
                [sharp-id (string-append html-filename "#" (attr-ref tx 'id))]
                [title (get-elements tx)])
            (toc-entries (cons (list level sharp-id title) (toc-entries)))))))

    (define section-txs
      (extract-tags (validate-txexpr doc)
                    '(title-1 section-1 include-section-1)))

    (for ([tx section-txs])
      (case (get-tag tx)
        [(title-1 section-1)
         (let ([level (attr-ref tx 'level)]
               [id (attr-ref tx 'id)]
               [title (get-elements tx)])
           ; (printf "- collecting section-1 ~s ~s\n" level id)
           (toc-entries (cons (list level (string-append "#" id) title) (toc-entries))))]
        [(include-section-1)
         (let ([file (attr-ref tx 'incfile)])
           ; (printf "- collecting include-section-1 from ~s\n" idoc)
           (collect-toc-entries-from-include-section file))]))
    )

  ; (printf "## calling collect-toc-entries\n")

  (collect-toc-entries doc)

  ; (printf "## done with collect-toc-entries = ~s\n" (toc-entries))

  ; (printf "calling replace-sections\n")

  ; (printf "tocentries = ~s\n" (toc-entries))

  (define toc-used? false)

  (define (replace-sections tx)
    ; (printf "doing replace-sections ~s\n" tx)

    (define (output-toc)
      (let ([tocitems '()])
        (for ([toc-entry (toc-entries)])
          ; (printf "adding tocentry ~s\n" toc-entry)
          (set! tocitems
            (cons `(li ([class ,(string-append "indent" (first toc-entry))])
                       (a ([href ,(second toc-entry)]) ,@(third toc-entry)))
                  tocitems)))
        `(ul ([class "toclist"]) ,@tocitems)))

    (case (get-tag tx)
      [(table-of-contents-1)
       (set! toc-used? true)
       (output-toc)]
      [(title-1)
       `(h1 ([id ,(attr-ref tx 'id)] [toclevel "1"] [tocentry "yes"])
            ,@(get-elements tx))]
      [(section-1)
       (let ([level (attr-ref tx 'level)])
         (define h-tag (h-tag-at-depth level))
         `(,h-tag ([id ,(attr-ref tx 'id)] [toclevel ,level] [tocentry "yes"])
                  ,@(get-elements tx)))]
      [(include-section-1)
       (if toc-used? `(span ([class "includesection"]))
           (let* ([incfile (attr-ref tx 'incfile)]
                  [idoc (get-doc incfile)]
                  [idoc (change-tag idoc 'root 'div)]
                  [idoc (change-tag idoc 'title 'h1)]
                  )
             ; (printf "*** idoc = ~s\n" idoc)
             idoc
             ; (get-doc incfile)
             ; `(div () ,@(get-elements idoc))
             ))]

      [else tx]))

  ; (printf "calling decode-elements ~s ~s\n" (first doc) (second doc))

  (decode doc #:txexpr-proc replace-sections))

