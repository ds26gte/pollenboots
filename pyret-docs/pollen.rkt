#lang racket

(require txexpr)
(require pollen/core)
(require pollen/decode)
(require pollen/misc/tutorial)
(require pollen/tag)
(require racket/date)

; (require "paragraphs.rkt")

(provide (all-defined-out))

; (provide (all-from-out "paragraphs.rkt"))

(define (decode-paragraphs-1 xx)
  (define (br-becomes-space xx)
    (decode-linebreaks xx " "))
  (decode-paragraphs xx #:linebreak-proc br-becomes-space))

(define (tag=? tx tag)
  (and (txexpr? tx) (equal? (get-tag tx) tag)))

(define (doc-title doc)
  ; (printf "doc is now ~s\n" doc)
  (or (select 'title doc)
      (select 'h1 doc)
      "Untitled"))

(define author "ds26gte")

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

(define (nested . elts)
  `(p () ,@elts))

(define (hyperlink url . elts)
  `(a ((href ,url)) ,@elts))

(define link hyperlink)

(define (create-up-dir pname)
  (set! pname (symbol->string pname))
  (let ([up-dir ""])
    (for ([c pname])
      (when (char=? c #\/) (set! up-dir (string-append up-dir "../"))))
    up-dir))

(define (add-up-dir up-dir pname)
  (if (not pname) pname
      (string->symbol
        (format "~a~a" up-dir pname))))

(define (include-section file)
  `(include-section-1 ([incfile ,file])))

; sections

(define (h-tag-at-depth n)
  (string->symbol (format "h~a" n)))

(define (sluggify xxx)
  (string-replace xxx " " "-"))

(define (section-at-depth n title)
  (define title-sluggified (sluggify title))
  (cond [(not (number? n))
         `(h5 ([id ,title-sluggified]) ,title)]
        [else
          (define level (number->string n))
          `(section-1 ([level ,level] [id ,title-sluggified]) ,title)]))

(define (section title) (section-at-depth 2 title))
(define (subsection title) (section-at-depth 3 title))
(define (subsubsection title) (section-at-depth 4 title))

(define (subsubsub*section title) (section-at-depth #f title))

(define (title tytle #:version [version "0"])
  ; (printf "doing title of  ~s\n"  tytle)
  (define title-sluggified (sluggify  tytle))
  `(title-1 ([id ,title-sluggified] ) ,tytle))

; ToC

(define (table-of-contents)
  ; (txexpr 'table-of-contents-1 '() '())
  `(table-of-contents-1 ())
  )

; ToDo: if table-of-contents called, don't insert include-section bodily, otherwise do

(define (change-tag tx from to)
  (define-values (tx1 _)
    (splitf-txexpr tx
      (λ (tx) (and (txexpr? tx) (eq? (get-tag tx) from)))
      (λ (tx) (txexpr to (get-attrs tx) (get-elements tx)))))
  tx1)

(define (remove-tag tx tag)
  (define-values (tx1 _)
    (splitf-txexpr tx
      (λ (tx) (and (txexpr? tx) (eq? (get-tag tx) tag)))))
  tx1)

(define (extract-tags tx tags)
  (define-values (_ txs)
    (splitf-txexpr tx
      (λ (tx) (and (txexpr? tx)
                   (member (get-tag tx) tags)))))
  txs)

(define (toc-handler doc)
  ; (printf "** doing toc-handler ~s \n"  false )

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
        [(title)
         (let ([level "1"]
               [id (attr-ref tx 'id)]
               [tytle (get-elements tx)])
           (toc-entries (cons (list level (string-append "#" id) tytle) (toc-entries))))]
        [(section-1)
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

  (define (output-toc)
    (let ([tocitems '()])
      (for ([toc-entry (toc-entries)])
        ; (printf "adding tocentry ~s\n" toc-entry)
        (set! tocitems
          (cons `(li ([class ,(string-append "indent" (first toc-entry))])
                     (a ([href ,(second toc-entry)]) ,@(third toc-entry)))
                tocitems)))
      `(ul ([class "toclist"]) ,@tocitems)))

  ; (printf "calling replace-sections\n")

  ; (printf "tocentries = ~s\n" (toc-entries))

  (define toc-used? false)

  (define (replace-sections tx)
    ; (printf "doing replace-sections ~s\n" tx)

    (case (get-tag tx)
      [(table-of-contents-1)
       (set! toc-used? true)
       (output-toc)]
      [(title-1)
       `(title ([id ,(attr-ref tx 'id)] [toclevel "1"] [tocentry "yes"])
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

(define (root . elts)
  ; (printf "* doing root\n")
  ; (printf "* doing root ~s ~s\n" (first elts) (second elts))
  ; (printf "* doing root ~s\n" elts)
  (let* ([doc `(root ,@elts)]
         [doc (toc-handler doc)])
    ; (printf "starting root decode of ~s\n" doc)
    (decode doc ;decode-elements elts?
            #:txexpr-elements-proc decode-paragraphs-1
            #:exclude-tags '(pre)
            #:string-proc (compose1 smart-quotes smart-dashes))))
