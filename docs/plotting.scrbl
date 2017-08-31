#lang scribble/manual

@require[
  scribble/example
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
    (only-in pict pict?)
    (only-in racket/math natural?))]

@(define my-eval (make-base-eval '(require gtp-plot/plot)))

@; -----------------------------------------------------------------------------

@title[#:tag "gtp-plotting"]{Plot}

@defmodule[gtp-plot/plot]

For command-line options, run @exec{raco gtp-plot --help}.

@defproc[(overhead-plot [pi* (treeof performance-info?)]) pict?]{
  Plots the performance overhead of gradual typing.
  More precisely, answers the question: "what percent of gradually-typed
   configurations run with at most @emph{D}x overhead?".

@;  @examples[#:eval plot-eval
@;    (overhead-plot 
@;  ]
}

@defproc[(exact-runtime-plot [pi performance-info?]) pict?]{
}

@defproc[(samples-plot [pi performance-info?]) pict?]{
}

@defproc[(validate-samples-plot [pi performance-info?]) pict?]{
}


@section{Experimental Plotting Functions}

The functions in this section make strange plots.
The appearance of these plots is subject to change.

@defproc[(cloud-plot [pi performance-info?]) pict?]{
}

@defproc[(rectangle-plot [pi performance-info?]) pict?]{
}

