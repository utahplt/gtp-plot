#lang info
(define collection "gtp-plot")
(define deps '("base" "draw-lib" "scribble-abbrevs" "scribble-lib" "math-lib" "pict-lib" "plot-lib" "reprovide-lang" "gtp-util"))
(define build-deps '("rackunit-lib" "racket-doc" "scribble-doc" "pict-lib" "pict-doc" "plot-doc" "rackunit-abbrevs" "typed-racket-doc" "gtp-util"))
(define pkg-desc "For plotting gradual typing performance data")
(define version "0.6")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/gtp-plot.scrbl" () (omit-start))))
(define raco-commands '(("gtp-plot" (submod gtp-plot/private/raco main) "Plot a dataset" #f)))
