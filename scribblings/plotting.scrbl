#lang scribble/manual

@require[
  scribble/example
  racket/runtime-path
  gtp-plot/typed-racket-info
  gtp-plot/reticulated-info
  gtp-plot/plot
  (for-label
    gtp-plot/configuration-info
    gtp-plot/performance-info
    gtp-plot/reticulated-info
    gtp-plot/sample-info
    gtp-plot/typed-racket-info
    gtp-plot/util
    pict
    plot/utils
    racket/base
    racket/contract
    racket/runtime-path
    (only-in racket/math natural?))]

@; -----------------------------------------------------------------------------

@title[#:tag "gtp-plotting"]{Plot}

@defmodule[gtp-plot/plot]

The examples in this section use the following context:

@racketblock[
  (require gtp-plot/typed-racket-info
           gtp-plot/reticulated-info
           gtp-plot/plot
           racket/runtime-path)
  (define-runtime-path mbta-data "./docs/data/mbta-v6.2.rktd")
  (define mbta (make-typed-racket-info mbta-data))
  (define-runtime-path sample_fsm-data "./docs/data/sample_fsm/")
  (define sample_fsm (make-reticulated-info sample_fsm-data))
  (*OVERHEAD-PLOT-HEIGHT* 200)
  (*OVERHEAD-PLOT-WIDTH* 400)
  (*FONT-SIZE* 14)
]
@(define-runtime-path mbta-data "./data/mbta-v6.2.rktd")
@(define mbta (make-typed-racket-info mbta-data))
@(define-runtime-path sample_fsm-data "./data/sample_fsm/")
@(define sample_fsm (make-reticulated-info sample_fsm-data))
@(*OVERHEAD-PLOT-HEIGHT* 200)
@(*OVERHEAD-PLOT-WIDTH* 400)
@(*FONT-SIZE* 14)

@(define-syntax-rule (render-demo sexp)
   @list[@racketblock[sexp] sexp])


For command-line options, run @exec{raco gtp-plot --help}.

@defproc[(overhead-plot [pi* (treeof performance-info?)]) pict?]{
  Plots the performance overhead of gradual typing.
  More precisely, answers the question: "what percent of gradually-typed
   configurations run with at most @math{D}x overhead?"
   where @math{D} is a real number on the @math{x}-axis.

  This function is intended for @tech{exhaustive performance info} structures.

  @render-demo[(overhead-plot mbta)]
}

@defproc[(exact-runtime-plot [pi (treeof performance-info?)]) pict?]{
  Plots all runtimes for all configurations.
  The @math{x}-axis is the number of type annotations in the configuration,
   the @math{y}-axis is its running time.

  One configuration @racket[_cfg] renders @racket[_N] points, where
   @racket[_N] is the length of the list @racket[(configuration-info->runtime* _cfg)].
  These points are spread across the @math{x}-axis bucket that represents
   configurations with @racket[_T] type annotations, where @racket[_T] is the
   value of @racket[(configuration-info->num-types _cfg)].

  @render-demo[(exact-runtime-plot mbta)]
}

@defproc[(samples-plot [si sample-info?]) pict?]{
  Similar to @racket[overhead-plot], but intended for @tech{SRA performance info} structures.
  Plots a 95% confidence interval for the number of configurations that run
   with at most @math{D}x overhead.

  @render-demo[(parameterize ([*OVERHEAD-SHOW-RATIO* #f]) (samples-plot sample_fsm))]
}

@defproc[(relative-scatterplot [pi-x performance-info?] [pi-y performance-info?]) pict?]{
  Plots points @math{(X, Y)} where @math{X} is the overhead of a configuration
   in @racket[pi-x] and @math{Y} is the overhead of the same configuration in
   @racket[pi-y].
  Assumes @racket[pi-x] and @racket[pi-y] are two datasets for the same benchmark.

  Points along the line @math{X=Y} represent configurations with equal overhead
   in both datasets.
  Points below the line are faster in @racket[pi-x] and points to the left of
   the line are faster in @racket[pi-y].

  @render-demo[(relative-scatterplot mbta mbta)]

  @history[#:added "0.2"]
}

@section{Experimental Plotting Functions}

The functions in this section make strange plots.
The appearance of these plots is subject to change.

@defproc[(cloud-plot [pi performance-info?]) pict?]{
  Plots a heatmap showing the order-of-magnitude of the overhead
   of each configuration.

  Intended for @tech{exhaustive performance info} structures.

  @render-demo[(cloud-plot mbta)]
}

@defproc[(discrete-overhead-plot [pi performance-info?] [D* (listof nonnegative-real/c)]) pict?]{
  Plots a histogram of the number of @math{D}-deliverable configurations for
   each @math{D} in the given list.

  @render-demo[(discrete-overhead-plot mbta '(1.2 4 10))]
}

@defproc[(rectangle-plot [pi performance-info?]) pict?]{
  Plots a thermometer showing the proportion of configurations that run
   within @math{D}x overhead, where @math{D} is the value of @racket[(*STANDARD-D*)].

  Intended for @tech{exhaustive performance info} structures.

  @render-demo[(rectangle-plot mbta)]
}

@defproc[(relative-overhead-cdf [pi-base performance-info?] [pi-new performance-info?]) pict?]{
  Plot a CDF with the relative overhead of @racket[pi-new] vs. @racket[pi-base].
  More precisely:
  @itemlist[
  @item{
    finds the configurations that are common to @racket[pi-new] and @racket[pi-base];
  }
  @item{
    groups the configurations by the quotient of their @racket[pi-new] runtime
     and @racket[pi-base] runtime;
  }
  @item{
    plots a line of points @math{(x,y)}, which means: @math{y%} of the common
     configurations are at most @math{x} times slower on @racket[pi-new]
  }
  ]

  If @racket[pi-old] and @racket[pi-new] are the same, then the relative overhead
   of each configuration is 1.

  @render-demo[(relative-overhead-cdf mbta mbta)]
}

@defproc[(grid-plot [make-plot (-> any/c pict?)] [data* (listof any/c)]) pict?]{
  Build plots for a sequence of datasets and arrange the plots into a grid.

  @render-demo[
    (parameterize ([*FONT-SIZE* 8]
                   [*GRID-NUM-COLUMNS* 2]
                   [*GRID-Y* #f]
                   [*OVERHEAD-PLOT-HEIGHT* 100]
                   [*OVERHEAD-SHOW-RATIO* #f])
      (grid-plot overhead-plot (list mbta mbta mbta)))]
}


@section{Plot Parameters}

@defparam[*BAR-WIDTH* bar-width nonnegative-real/c #:value 0.1]{
  Controls width of bars in a @racket[discrete-overhead-plot].
}

@defparam[*DECORATIONS-COLOR* pc plot-color/c #:value 0]{
  Sets color of some plot decorations

  @history[#:added "0.2"]
}

@defparam[*FONT-SIZE* font-size exact-positive-integer? #:value 10]{
  Controls font size of text in plots.
}

@defparam[*GRID-X* grid-x (or/c #f natural?) #:value 600]{
  Controls the width of a @racket[grid-plot].
  If @racket[#false], the width of the grid is at least @racket[(* (*GRID-NUM-COLUMNS*) (*OVERHEAD-PLOT-WIDTH*))].
}

@defparam[*GRID-Y* grid-y (or/c #f natural?) #:value 1300]{
  Controls the height of a @racket[grid-plot].
  If @racket[#false], the height of the grid is at least @racket[(* (quotient _N (*GRID-NUM-COLUMNS*)) (*OVERHEAD-PLOT-WIDTH*))],
   where @racket[_N] is the number of plots in the grid.
}

@defparam[*GRID-X-SKIP* grid-x-skip natural? #:value 30]{
  Horizontal space between plots on a @racket[grid-plot].
}

@defparam[*GRID-Y-SKIP* grid-y-skip natural? #:value 6]{
  Vertical space between plots on a @racket[grid-plot].
}

@defparam[*GRID-NUM-COLUMNS* grid-num-columns exact-positive-integer? #:value 3]{
  Number of columns on a @racket[grid-plot].
}

@defparam[*INTERVAL-ALPHA* ia nonnegative-real/c #:value 1]{
  Sets the transparency of shaded plot regions.
}

@defparam[*OVERHEAD-FREEZE-BODY* freeze? boolean? #:value #f]{
  When true, plotting functions will call @racket[freeze] on rendered plots
   before returning.
  This is useful for plots with a large number of elements.
}

@defparam[*OVERHEAD-LEGEND?* show-legend? boolean? #:value #true]{
  When @racket[#false], hide dataset name and axis labels.
}

@defparam[*OVERHEAD-LINE-COLOR* line-color plot-color/c #:value 3]{
  Sets color of solid lines used in plots.
}

@defparam[*OVERHEAD-LINE-WIDTH* line-width nonnegative-real/c #:value 1]{
  Sets width of solid lines used in plots.
}

@defparam[*OVERHEAD-MAX* x-max exact-positive-integer? #:value 10]{
  Sets maximum @math{x}-value in overhead plots.
}

@deftogether[(
  @defparam[*OVERHEAD-PLOT-HEIGHT* plot-height nonnegative-real/c #:value 300]
  @defparam[*OVERHEAD-PLOT-WIDTH* plot-height nonnegative-real/c #:value 600]
)]{
  Sets height and width of plots.
}

@defparam[*OVERHEAD-SAMPLES* num-samples exact-positive-integer? #:value 20]{
  Number of points used to draw the solid color line in overhead plots.
}

@defparam[*OVERHEAD-SHOW-RATIO* show? boolean? #:value #t]{
  If true, plots come with a typed/baseline ratio.
}

@defparam[*POINT-ALPHA* pa nonnegative-real/c #:value 0.4]{
  Sets translucency of points.
}

@defparam[*POINT-COLOR* pc plot-color/c #:value 2]{
  Sets color of points.
}

@defparam[*POINT-SIZE* ps exact-positive-integer? #:value 3]{
  Sets size of points.
}

@defparam[*POINT-SYMBOL* ps point-sym/c #:value 'fullcircle]{
  Symbol used to draw points.
}

@defparam[*SAMPLE-COLOR* sc plot-color/c #:value "chocolate"]{
  Color used to draw sample plots.
}

@defparam[*STANDARD-D* D (or/c #f positive?) #:value #f]{
  A default overhead value.
}

