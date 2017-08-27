#lang scribble/manual
@require[
  (for-label
    racket/base
    racket/contract
    (only-in openssl/md5 md5))]

@title{GTP plot}
@author{Ben Greenman}

@defmodule[gtp-plot]{
  Tools for visualizing the performance of a gradual typing system.
  If you have performance data, this package should help you plot it.
}


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
@margin-note{See @secref{glossary} for more definitions.}

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

The data must be in one of the @secref{built-in data formats}.
@; TODO or supply a parser


@section{Data Definitions}

TBA

@defmodule[gtp-plot/configuration-info]{}

@defstruct[configuration-info ([id any/c] [num-types natural?] [runtime* nonnegative-real/c]) #:prefab]{
  A @deftech{configuration info} structure describes the performance of
   one gradually-typed configuration of a program.
}

@defproc[(configuration-info->id [cfg configuration-info?]) any/c]{
  Returns the configuration's identifier.
  The identifier should describe the type annotations in the configuration,
   relative to the possible type annotations in the program.
}

@defproc[(configuration-info->num-types [cfg configuration-info?]) natural?]{
  Return the number of type annotations in the configuration.
}

@defproc[(configuration-info->runtime* [cfg configuration-info?]) (listof nonnegative-real/c)]{
  Return the running times associated with the configuration.
}

@defproc[(configuration-info->mean-runtime [cfg configuration-info?]) nonnegative-real/c]{
  Return the mean running time associated with the configuration.
}

@; ---
@defmodule[gtp-plot/performance-info]{
  
}

@defmodule[gtp-plot/sample-info]{
  Represents sampled data
}

@defproc[(sample-info? [x any/c]) boolean?]{
  Predicate for @deftech{sample info} structures.
}

@defproc[(make-sample-info [pi performance-info?] [samples (listof path-string?)]) sample-info?]{
  Make a @tech{sample info} structure from a @tech{performance info} structure
   and sampled datasets.
  Logs an @racket['error]-level message to @racket[gtp-plot-logger] if
   the sampled datasets do not all contain the same number of configurations.
}

@defproc[(sample-info->sample-size [si sample-info?]) natural?]{
  Count the number of configurations in each sample in the given @tech{sample info}
   structure.
}

@defproc[(sample-info->performance-info* [si sample-info?]) (listof performance-info?)]{
  Expand a @tech{sample info} structure to a list of @tech{performance info} structures.
  Each structure represents one sample of configurations.
}

@; -----------------------------------------------------------------------------
@section[#:tag "gtp-plotting"]{Plot}



@; -----------------------------------------------------------------------------
@section[#:tag "built-in-data-formats"]{Data Formats}

@; -----------------------------------------------------------------------------
@section[#:tag "gtp-support]{Support}

@defmodule[gtp-plot/system]{
  Convenience API for making system calls.
}

See also @racketmodname[racket/system].

@defproc[(shell [cmd path-string?] [arg* (or/c path-string? (listof path-string?))] ...) string?]{
  Finds the executable that @racket[cmd] denotes, then invokes it with the given arguments.
  Returns a string containing all output from the system call.
  Raises an @racket[exn:fail:user?] exception if the executable exits uncleanly.
}

@defproc[(md5sum [filename path-string?]) string?]{
  Same as @racket[(call-with-input-file filename md5)].
}

@defmodule[gtp-plot/util]{
  TBA
}

@defproc[(nonnegative-real/c [x any/c]) boolean?]{
  Flat contract for non-negative real numbers.
}

@; -----------------------------------------------------------------------------
@section[#:tag "gtp-glossary"]{GTP Glossary}
