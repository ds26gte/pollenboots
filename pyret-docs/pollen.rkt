#lang racket

(require txexpr)
(require pollen/core)
; (require pollen/file)
(require pollen/decode)
(require pollen/misc/tutorial)
(require pollen/tag)
; (require pollen/setup)
(require racket/date)

(require "utils.rkt")
(require "common-tags.rkt")
(require "nice-paragraphs.rkt")

(require "toc.rkt")
(require "make-glossary.rkt")

; (printf "## current-metas is ~s\n" (current-metas))

; (printf "## processing ~s\n" here)

(provide (all-defined-out))

(provide (all-from-out "utils.rkt"
                       "common-tags.rkt"
                       "nice-paragraphs.rkt"
                       "toc.rkt"
                       "make-glossary.rkt"))

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

(define (get-date)
  (date->string (current-date)))

(define (include-section file)
  `(include-section-1 ([incfile ,file])))

; sections

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
  `(title-1 ([level "1"] [id ,title-sluggified]) ,tytle))

(define (root . elts)
  (let* ([doc `(root ,@elts)]
         [doc (toc-handler doc)]
         [doc (glossary-handler doc)])
    ; (printf "starting root decode of ~s\n" doc)
    (decode doc ;decode-elements elts?
            #:txexpr-elements-proc decode-paragraphs-1
            #:exclude-tags '(pre)
            #:string-proc (compose1 smart-quotes smart-dashes))))
