#lang scribble/manual
@require[
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
    (only-in racket/math natural?)
    (only-in openssl/md5 md5))]

@title[#:tag "top"]{GTP plot}
@author{Ben Greenman}

Tools for visualizing the performance of a gradual typing system.
If you have performance data, this package should help you plot it.


@section{Introduction: Gradual Typing Performance}

A gradual typing system lets programs mix @tech{dynamically typed} and
 @tech{statically typed} code.
@itemlist[
@item{
  @deftech{dynamically typed} code avoids type errors at run-time,
   using run-time checks.
}
@item{
  @deftech{statically typed} code avoids type errors at compile-time,
   using ahead-of-time checks.
}
]

Typed Racket is a gradual typing system because programs can mix
 statically-typed @racketmodname[typed/racket] code
 with dynamically-typed @racketmodname[racket] code.

The main challenge of gradual typing is: @emph{how to protect the assumptions
 of statically typed code from dynamically typed code?}
One way to do this is to use run-time checks:
@itemlist[
@item{
  if statically typed code is expecting a value with type @racket[_T],
}
@item{
  and receives a dynamically typed value @racket[_v],
}
@item{
  then perform a run-time check, e.g., @racket[(if (_T? _v) _v (error 'bad-type))]
}
]

A run-time check like @racket[_T?] makes the program safe, but it also
 makes it slow.
How slow?
Depends on the number of checks like @racket[_T?] and the cost of each check.

The purpose of this package is to help answer the question of "how slow".


@subsection{Performance Evaluation Assumptions}

This package assumes that its clients have:

@itemlist[
@item{
  a program,
}
@item{
  a set of gradually-typed @tech{configurations} of the program,
}
@item{
  data on the performance of each configuration.
}
]

The data must be in one of the built-in data formats.
@; TODO or add parser


@include-section{data-definition.scrbl}
@include-section{plotting.scrbl}
@include-section{parsing.scrbl}
@include-section{supporting.scrbl}

@section[#:tag "gtp-glossary"]{GTP Glossary}

(These aren't definitions, these are examples.)

A @deftech{typeable component} in Typed Racket is a module.
A typeable component in Reticulated is 

A @deftech{configuration} is a gradually-typed program.
Given a fully-typed program with @racket[_N] type annotations,
 there are @racket[(expt 2 _N)] configurations that have fewer annotations
 than the fully-typed configuration.

