#lang racket

(require txexpr)
(require pollen/core)
(require pollen/tag)

(require racket/date)

(require "utils.rkt")

(provide (all-defined-out))

(define (doc-title doc)
  ; (printf "doc is now ~s\n" doc)
  (or (select 'title doc)
      (select 'h1 doc)
      "Untitled"))

(define (emph . elems)
  `(i () ,@elems))

(define (code . elems)
  `(code ([class "uncolored-code"]) ,@elems))

(define (author . elems)
  `(div ([class "author"]) "by " ,@elems))

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
  (define title-sluggified (or tag (sluggify* title-elems)))
  (cond [(not (number? n))
         `(h5 ([id ,title-sluggified]) ,@title-elems)]
        [else
          (define level (number->string n))
          `(section-1 ([level ,level] [id ,title-sluggified]) ,@title-elems)]))

(define (section #:tag [tag #f] #:tag-prefix [tag-prefix #f] . titlex)
  (section-at-depth #:tag tag 2 titlex))
(define (subsection #:tag [tag #f] . titlex) (section-at-depth #:tag tag 3 titlex))
(define (subsubsection #:tag [tag #f] . titlex) (section-at-depth #:tag tag 4 titlex))

(define (subsubsub*section #:tag [tag #f] . titlex) (section-at-depth #:tag tag #f titlex))

(define (title #:tag [tag #f] #:version [version "0"]
               #:friendly-title [friendly-title #f]
               #:noimport [noimport #f]
               #:style [style #f]
               . title-terms)
  (define title-1
    (cond [friendly-title (list friendly-title)]
          [else title-terms]))
  (define title-sluggified
    (cond [tag tag]
          [friendly-title (sluggify friendly-title)]
          [else (sluggify* title-terms)]))
  `(title-1 ([level "1"] [id ,title-sluggified]) ,@title-1))

(define (docmodule #:noimport [noimport #f] #:friendly-title [friendly-title #f] tag . body)
  `(div ()
       ,(apply title #:tag tag #:friendly-title friendly-title '())
       ,@body))

(define ul
  (default-tag-function 'ul #:class "list-group"))

(define li
  (default-tag-function 'li #:class "list-group-item"))

(define (nested #:style [style ""] . elems)
  `(p () ,@elems))

(define (para #:style [style #f]. elems)
  (define attribs
    (if style `([class ,style]) `()))
  `(p ,attribs ,@elems))

(define (hyperlink url . elems)
  `(a ((href ,url)) ,@elems))

(define link hyperlink)

(define (image #:scale [scale 1] file)
  `(img ([src ,file])))

(define (pyret-method . ign-for-now)
  "pyret-method")

(define (collection-doc #:contract [contract #f]
                        #:show-ellipses [show-ellipses #f]
                        . ign-for-now)
  "collection-doc")

(define (ignore . ign) "")

(define (doc-internal #:stack-unsafe [stack-unsafe #f] . elems)
  (if stack-unsafe
      `(div ()
            (span ([class "margin-note"])
                  "!→ means this function is not stack safe")
            (pre () ,@elems))
      `(div ()
            (pre () ,@elems)
            )))

(define (margin-note* . elems)
  `(span ([class "margin-note"])
         ,@elems))

(define note margin-note*)

(define (cpo-only . elems)
  `(div ([class "CPO"]) (div ([class "cpo-icon"]) ,@elems)))

(define (vscode-only . elems)
  `(div ([class "VSCode"]) (div ([class "vscode-icon"]) ,@elems)))

(define (cli-only . elems)
  `(div ([class "CLI"]) (div ([class "cli-icon"]) ,@elems)))

(define (vscode-cli-only . elems)
  `(div ([class "VSCodeCLI"]) (div ([class "vscode-cli-icon"]) ,@elems)))

(define (tabular #:sep [sep #f] #:column-properties [column-properties #f] #:style [style #f]
                 . rows)
  ; (printf "doing tabular of ~s\n" rows)
  ; (for-each (λ (row) (printf "doing row...\n")
  ;             (for-each (λ (cell) (printf "cell is ~s\n" cell)) row))
  ;           rows)
  `(table ()
     ,@(for/list ([row (car rows)])
         `(tr ()
            ,@(for/list ([cell row])
                `(td ()
                     (span () ,(if (list? cell) (car cell) cell))))))))

(define (form a b . elems)
  ; (printf "doing form a = ~s\nb = ~s\nelems = ~s\n" a b elems)
  `(div ()
        ,(make-gloss a)
        (pre ([class "pyret-display"]) ,b)
        ,@elems))

(define (value name typ . elems)
  `(div ()
        ,(make-gloss name)
        (pre ([class "pyret-display"])
             ,name " :: " ,typ)
        ,@elems))

(define (type-spec #:alias [alias #f] type-name tyvars . body)
  ; (printf "### type-spec ~s ~s ~s\n" type-name tyvars body)
  (if (list? tyvars)
      (set! type-name (string-append type-name
                        "<"
                        (apply string-append (add-between tyvars ", "))
                        ">"))
      (set! body (cons tyvars body)))
  `(div ()
        (div ([class "pyret-display"]) ,type-name)
        ,(make-gloss type-name)
        ,@body))

(define (a-arrow . typs)
  ; (printf "*** doing a-arrow of ~s\n" typs)
  (set! typs
    (filter (λ (typ) (not (equal? typ "Brand"))) typs))
  (set! typs
    (map (λ (typ) (if (null? typ) "()" typ)) typs))
  (when (= (length typs) 1)
    (set! typs (cons "()" typs)))
  (let ([res
          `(span () ,@(add-between typs ", " #:before-last " -> "))])
    ; (printf "*** a-arrow produced ~s\n" res)
    res))

(define (a-app base typs . ign-for-now)
  ; (printf "doing a-app ~s ~s\n" base typs)
  (when (list? typs)
    (set! typs (string-join typs ", ")))
  (string-append base "<" typs ">"))

(define (a-tuple . ign-for-now)
  "a-tuple")

(define (a-id x . ign) x)


(define A "Any")
(define N "Number")
(define EN "Exactnum")
(define RN "Roughnum")
(define S "String")
(define No "Nothing")
(define B "Boolean")

(define (L-of typ) (a-app "List" typ))
(define (A-of typ) (a-app "Array" typ))
(define (O-of typ) (a-app "Option" typ))

(define eq "EqualityResult")
(define eqfun `(a-arrow ,A ,A ,B))
(define eq3fun `(a-arrow ,A ,A ,eq))

(define T "EqualityResult")
(define EQ "EqualityResult")
(define TA "Table")
(define L "List")

(define equal-always-op `(code "=="))
(define equal-now-op `(code "=~"))
(define identical-op `(code "<=>"))

(define (singleton-doc typename1 fieldname typename . elems)
  `(div ()
        ,(make-gloss fieldname)
        (div ([class "pyret-display"]) ,fieldname " :: " ,typename)
        ,@elems))

(define (constructor-doc #:private [private #f] typename1 fieldname args typename . elems)
  ; (printf "constructor-doc args = ~s\n" args)
  `(div ()
        ,(make-gloss fieldname)
        (div ([class "pyret-display"])
             ,fieldname " :: "
             (span () "(" ,(first (first args)) " :: " ,(first (third (first args))) ")")
             " -> " ,typename)
        ,@elems))

(define (method-doc #:alt-docstrings [alt-docstrings #f] #:contract [contract "contract"]
                    #:args [args "args"] #:return [return "return"]
                    data-name var-name name
                    . elems)
  (unless contract (set! contract "contract"))
  `(div () (tt () ,(string-append "." name) " :: " ,contract))
  )

(define (method-spec #:params [params #f] #:contract [contract #f] #:return [return #f]
                     #:doc [doc #f]
                     #:args [args #f] #:alt-docstrings [alt-docstrings #f]
                     #:examples [examples '()] name . elems)
  ; (printf "*** method-spec ~s ~s\n" name elems)
  `(div () (tt () ,(string-append "." name)) ,@elems))

(define (repl-examples . elems)
  ; (printf "*** repl-examples ~s\n" elems)
  `(div ()
        ,@(map (λ (elem)
                 `(div ()
                      (pre () (caar elem))
                      ,(cadr elem)))
               elems)))

(define (colorful-function-series)
  ; (printf "*** colorful-function-series\n")
  `(pre () "colorful-function-series"))

(define (a-chart-window)
  ; (printf "*** a-chart-window\n")
  `(pre () "a-chart-window"))
