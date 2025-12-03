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
(require "make-xref.rkt")

; (printf "## current-metas is ~s\n" (current-metas))

; (printf "## processing ~s\n" here)

(provide (all-defined-out))

(provide (all-from-out "utils.rkt"
                       "common-tags.rkt"
                       "nice-paragraphs.rkt"
                       "toc.rkt"
                       "make-glossary.rkt"
                       "make-xref.rkt"))

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

(define (section-at-depth n title-elems #:tag [tag #f])
  (define title-sluggified (or tag (sluggify title-elems)))
  (cond [(not (number? n))
         `(h5 ([id ,title-sluggified]) ,@title-elems)]
        [else
          (define level (number->string n))
          `(section-1 ([level ,level] [id ,title-sluggified]) ,@title-elems)]))

(define (section #:tag [tag #f] . titlex) (section-at-depth #:tag tag 2 titlex))
(define (subsection #:tag [tag #f] . titlex) (section-at-depth #:tag tag 3 titlex))
(define (subsubsection #:tag [tag #f] . titlex) (section-at-depth #:tag tag 4 titlex))

(define (subsubsub*section #:tag [tag #f] . titlex) (section-at-depth #:tag tag #f titlex))

(define (title #:tag [tag #f] #:version [version "0"] . titlex)
  (define title-sluggified (or tag (sluggify titlex)))
  `(title-1 ([level "1"] [id ,title-sluggified]) ,@titlex))

(define (root . elts)
  (let* ([doc `(root ,@elts)]
         [doc (toc-handler doc)]
         [doc (glossary-handler doc)]
         [doc (xref-handler doc)])
    ; (printf "starting root decode of ~s\n" doc)
    (decode doc ;decode-elements elts?
            #:txexpr-elements-proc decode-paragraphs-1
            #:exclude-tags '(pre)
            #:string-proc (compose1 smart-quotes smart-dashes))))
