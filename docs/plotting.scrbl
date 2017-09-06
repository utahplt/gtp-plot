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
    racket/base
    racket/contract
    racket/runtime-path
    (only-in pict pict?)
    (only-in racket/math natural?))]

@; -----------------------------------------------------------------------------

@title[#:tag "gtp-plotting"]{Plot}

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


@defmodule[gtp-plot/plot]

For command-line options, run @exec{raco gtp-plot --help}.

@defproc[(overhead-plot [pi* (treeof performance-info?)]) pict?]{
  Plots the performance overhead of gradual typing.
  More precisely, answers the question: "what percent of gradually-typed
   configurations run with at most @math{D}x overhead?"
   where @math{D} is a real number on the @math{x}-axis.

  This function is intended for @tech{exhaustive performance info} structures.

  @render-demo[(overhead-plot mbta)]
}

@defproc[(exact-runtime-plot [pi performance-info?]) pict?]{
  Plots all runtimes for all configurations.
  The @math{x}-axis is the number of type annotations in the configuration,
   the @math{y}-axis is its running time.

  One configuration @racket[_cfg] renders @racket[_N] points, where
   @racket[_N] is the length of the list @racket[(configuration-info->runtime* _cfg)].
  These points are spread across the @math{x}-axis bucket that represents
   configurations with @racket[_T] type annotations, where @racket[_T] is the
   value of @racket[(configuration->num-types _cfg)].

  @render-demo[(exact-runtime-plot mbta)]
}

@defproc[(samples-plot [si sample-info?]) pict?]{
  Similar to @racket[overhead-plot], but intended for @tech{SRA performance info} structures.
  Plots a 95% confidence interval for the number of configurations that run
   with at most @math{D}x overhead.

  @render-demo[(parameterize ([*OVERHEAD-SHOW-RATIO* #f]) (samples-plot sample_fsm))]
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

@defproc[(rectangle-plot [pi performance-info?]) pict?]{
  Plots a thermometer showing the proportion of configurations that run
   within @math{D}x overhead, where @math{D} is the value of @racket[(*STANDARD-D*)].

  Intended for @tech{exhaustive performance info} structures.

  @render-demo[(rectangle-plot mbta)]
}

