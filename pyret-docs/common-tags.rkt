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

(define (itemlist . elems)
  `(ul () ,@elems))

(define (item . elems)
  `(li () ,@elems))

(define ul
  (default-tag-function 'ul #:class "list-group"))

(define li
  (default-tag-function 'li #:class "list-group-item"))

(define (nested #:style [style #f] . elems)
  (define attribs
    (if style `([class "insetpara nested"]) `([class "nested"])))
  `(div ,attribs ,@elems))

(define (para #:style [style #f]. elems)
  (define attribs
    (if style `([class ,style]) `()))
  `(p ,attribs ,@elems))

(define (hyperlink url . elems)
  `(a ((href ,url)) ,@elems))

(define link hyperlink)

(define (image #:scale [scale 1] file)
  `(img ([src ,file])))



(define (pyret-method ign1 x . z)
  (define name (if (null? z) x (car z)))
  `(tt () ,(string-append "." name)))

(define (collection-doc #:contract [contract #f]
                        #:show-ellipses [show-ellipses #f]
                        . ign-for-now)
  "collection-doc")

(define (collection-doc-2 name #:args [args ""] #:return [return ""])
  ; (printf "collection-doc-2 args = ~s\n" args)
  (if (and (list? args) (list? (car args)))
      `(pre ([class "pyret-display"])
            "[" ,name ": " ,@(add-between args ", ") ", ...] -> "
            ,return)
      `(pre ([class "pyret-display"])
            "[" ,name ": " ,args ", ...] -> "
            ,return)))

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

(define margin-note margin-note*)

(define (cpo-only . elems)
  `(div ([class "CPO"]) (div ([class "cpo-icon"]) ,@elems)))

(define (vscode-only . elems)
  `(div ([class "VSCode"]) (div ([class "vscode-icon"]) ,@elems)))

(define (cli-only . elems)
  `(div ([class "CLI"]) (div ([class "cli-icon"]) ,@elems)))

(define (vscode-cli-only . elems)
  `(div ([class "VSCodeCLI"]) (div ([class "vscode-cli-icon"]) ,@elems)))

(define (tabular #:sep [sep #f]
                 #:column-properties [column-properties #f]
                 #:row-properties [row-properties #f]
                 #:style [style #f]
                 . rows)
  ; (printf "doing tabular of ~s\n" rows)
  ; (for-each (lambda (row) (printf "doing row...\n")
  ;             (for-each (lambda (cell) (printf "cell is ~s\n" cell)) row))
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

(define (value #:style [style ""] name typ . elems)
  `(div ()
        ,(make-gloss name)
        (pre ([class "pyret-display"])
             ,name " :: " ,typ)
        ,@elems))

(define (type-spec #:alias [alias #f] #:private [private #f] type-name tyvars . body)
  ; (printf "### type-spec ~s ~s \n" type-name tyvars )
  (define og-type-name type-name)
  (cond [alias
          ; (printf "alias = ~s\n" alias)
          ; (set! type-name (string-append type-name " = " alias))
          ; (set! type-name (format "~a = ~a" type-name alias))
          `(div () ,(make-gloss og-type-name)
                (pre ([class "pyret-display"])
                     ,(ref-gloss og-type-name type-name))
                ,@body) ]
        [else
          (if (list? tyvars)
              (when (cons? tyvars)
                (set! type-name (string-append type-name
                                  "<"
                                  (apply string-append (add-between tyvars ", "))
                                  ">")))
              (set! body (cons  tyvars body)))
          `(div ()
                ,(make-gloss og-type-name)
                (pre ([class "pyret-display"]) ,(ref-gloss og-type-name type-name))
                ,@body)]))

(define (a-ftype . typs)
  (let* ([arg-typs (drop-right typs 1)]
        [ret-typ (car (take-right typs 1))]
        [length-args (length arg-typs)])
    (cond [(= length-args 0)
           `(span () "() -> " ,ret-typ)]
          [(> length-args 1)
           `(span () "(" (span () ,@(add-between arg-typs ", ")) ") -> "
                  ,ret-typ)]
          [else `(span () (span () ,@(add-between arg-typs ", ")) " -> " ,ret-typ)])))

(define (p-a-ftype . typs)
  `(span () "(" ,(apply a-ftype typs) ")"))

(define (a-arrow . typs)
  ; (printf "*** doing a-arrow of ~s\n" typs)
  (set! typs
    (filter (lambda (typ) (not (equal? typ "Brand"))) typs))
  (set! typs
    (map (lambda (typ) (if (null? typ) "()" typ)) typs))
  (when (= (length typs) 1)
    (set! typs (cons "()" typs)))
  (let ([res
          `(span () ,@(add-between typs ", " #:before-last " -> "))])
    ; (printf "*** a-arrow produced ~s\n" res)
    res))

(define (a-app base . typs)
  ; (printf "doing a-app base= ~s typs= ~s\n" base typs )
  (set! typs (map (lambda (typ)
                    (if (list? typ)
                        (if (and (> (length typ) 1) (memq (first typ) '(ref-gloss-1 span)))
                            typ
                            `(span () ,@(add-between typ ", ")))
                        typ)) typs))
  ; (printf "*** typs is now ~s\n" typs)
  (set! typs `(span () ,@(add-between typs ", ")))
  (let ([x `(span () ,base "<" ,typs ">")])
    ; (printf "a-app ~s ~s ==> ~s\n" base typs x)
    x))

(define (a-tuple . fields)
  `(span () "{" ,@(add-between fields ", ") "}"))

(define (a-id x . ign) (ref-gloss x))

(define A "Any")
(define N (ref-gloss "Number"))
(define EN "Exactnum")
(define RN "Roughnum")
(define S (ref-gloss "String"))
(define No "Nothing")
(define B (ref-gloss "Boolean"))

(define (L-of typ) (a-app "List" typ))
(define (S-of typ) (a-app "Set" typ))
(define (A-of typ) (a-app "Array" typ))
(define (O-of typ) (a-app "Option" typ))
(define (E-of typ1 typ2) (a-app (ref-gloss "Either") typ1 typ2))
(define (P-of typ1 typ2) (a-app "Pick" typ1 typ2))

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

(define (singleton-doc #:style [style ""] typename1 fieldname typename . elems)
  `(div ()
        ,(make-gloss fieldname)
        (pre ([class "pyret-display"]) ,fieldname " :: " ,typename)
        ,@elems))

(define (a-var-type val typ)
  `(span ([class "pyret-content"]) ,val " :: " ,typ))

(define (p-a-var-type val typ)
  `(span ([class "pyret-content"]) "(" ,val " :: " ,typ ")"))

(define (constructor-doc #:private [private #f] #:style [style #f] typename1 fieldname args typename . elems)
  ; (printf "constructor-doc typename1= ~s fieldname= ~s args= ~s typename= ~s elems= ~s\n" typename1 fieldname args typename elems)
  (let ([x
          `(div ()
                ,(make-gloss fieldname)
                (pre ([class "pyret-display"])
                     ,(ref-gloss fieldname) " :: ("
                     ,@(add-between
                         (map (lambda (arg)
                                `(span () ,(first arg) " :: " ,(second (third arg))))
                              args) ", ")
                     ") -> " ,typename)
                ,@elems
                (p) ;make-gloss seems to insert p on top, so match it with one on bottom
                )])
    ; (printf "x = ~s\n" x)
    x))

(define (method-doc #:alt-docstrings [alt-docstrings #f] #:contract [contract #f]
                    #:args [args "args"] #:return [return "return"]
                    data-name var-name name
                    . elems)
  ; (printf "*** method-doc\n")
  (unless contract (set! contract "contract"))
  `(div ([class "pyret-display"]) (tt () ,(string-append "." name) " :: " ,contract))
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
        ,@(map (lambda (elem)
                 `(div ()
                      (pre () ,(caar elem))
                      ,(cadr elem)))
               elems)))

(define (colorful-function-series)
  ; (printf "*** colorful-function-series\n")
  `(pre () "colorful-function-series"))

(define (a-chart-window)
  ; (printf "*** a-chart-window\n")
  `(pre () "a-chart-window"))

(define (a-record . fields)
  ; (printf "*** a-record ~s\n" fields)
  (append  '(span ())
           (list "{")
           (add-between fields ", ")
           (list "}")))

  ; (string-append "{"
  ;   (apply string-append (add-between fields ", ")) "}")

(define (a-field name type . desc)
  ; (printf "*** a-field ~s ~s ~s\n" name type desc)
  `(span () ,name " :: " ,type))

  ; (string-append name " :: " type)

(define (append-gen-docs . desc)
  "")
