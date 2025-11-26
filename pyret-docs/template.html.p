<!doctype html>
<html lang="en">
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-sRIl4kxILFvY47J16cr9ZwB07vP4J8+LH7qKQnuqkuIAvNWLzeN8tE5YBujZqJLB" crossorigin="anonymous">
  <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/js/bootstrap.bundle.min.js" integrity="sha384-FKyoEForCGlyvwx9Hj09JcYn3nv7wiPVlz7YYwJrWVcXK/BmnVDxM+D2scQbITxI" crossorigin="anonymous"></script>
  <style>
     li.indent2 {
       text-indent: 2em;
     }
     ul.toclist {
       list-style-type: none;
     }
     .pyretexpr {
       color: cornflowerblue;
     }
     .function {
       font-family: monospace;
       background-color: #6bccdf;
     }
  </style>
  <head>
    <title>◊(doc-title doc)</title>
  </head>
  <body>
    <div class="container">
    ◊(->html doc)
    ◊(define top-dir (point-to-project-root here))
    ◊(define prev-page (prefix-dir top-dir (previous here)))
    ◊(define next-page (prefix-dir top-dir (next here)))
    <hr/>
    The current page is ◊|here|.
    ◊when/splice[prev-page]{
    The previous page is <a href = "◊prev-page">◊|prev-page|</a>.
    }
    ◊when/splice[next-page]{
    The next page is <a href = "◊next-page">◊|next-page|</a>.
    }
    </div>
  </body>
</html>
