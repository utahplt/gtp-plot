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

@defmodule[gtp-plot/typed-racket-info]

@defproc[(typed-racket-data? [ps path-string?]) boolean?]{
  A Typed Racket dataset:
  @itemlist[
  @item{
    lives in a file named @filepath{NAME-vX-OTHER.rktd},
     where @filepath{NAME} is the name of the program
     and @filepath{vX} is a Racket version number,
     and @filepath{OTHER} is any string (used to distinguish this data from other data with the same prefix).
  }
  @item{
    contains a Racket vector with @racket[(expt 2 _N)] elements (for some natural @racket[_N]);
     entries in the vector are lists of runtimes.
  }
  ]

  Example data:
  @verbatim{
    ;; Example Typed Racket GTP dataset
    #((1328)
    (42)
    (8) (1))
  }
}

@defproc[(make-typed-racket-info [ps typed-racket-data?]) typed-racket-info?]{
  Build a @tech{performance info} structure from a Typed Racket dataset.
}

@defproc[(typed-racket-info? [x any/c]) boolean?]{
  Predicate for Typed Racket datasets.
}


@defmodule[gtp-plot/reticulated-info]

@defproc[(reticulated-data? [ps path-string?]) boolean?]{
  A Reticulated dataset:
  @itemlist[
  @item{
    lives in a directory with a name like @filepath{NAME-OTHER},
     where @filepath{NAME} is the name of the program
     and @filepath{OTHER} is any string (used to distinguish from other data with the same prefix);
  }
  @item{
    contains a file named @filepath{NAME-python.tab},
     where each line has a floating point number;
  }
  @item{
    either:
    @itemlist[
    @item{
      contains a @tech{Reticulated data file} named @filepath{NAME.tab};
    }
    @item{
      or a compressed @tech{Reticulated data file} named @filepath{NAME.tab.gz};
    }
    @item{
      or a file @filepath{NAME-retic-typed.tab}, a file @filepath{NAME-retic-untyped.tab}, and a sequence of files named @filepath{sample-NUM.tab} (where @filepath{NUM} is a sequence of digits).
    }
    ]
  }
  ]

  A @deftech{Reticulated data file} describes the running time of configurations.
  Each line in a data file has the format:
  @verbatim{
    ID	NUM-TYPES	[TIME, ...]
  }
  where:
  @itemlist[
  @item{
    @tt{ID} is a hyphen-separated sequence of digits (aka, a configuration id),
  }
  @item{
    @tt{NUM-TYPES} is a natural number, and
  }
  @item{
    @tt{TIME} is a positive floating-point number.
  }
  ]

  Two lines from an example @tech{Reticulated data file}:
  @verbatim{
    13-0-2-7-7	9	[2.096725507, 2.134529453, 2.088266101, 2.1067140589999998, 2.10622448, 2.0677214, 2.1665523010000003, 2.0715632559999997, 2.1208832060000002, 2.068036723, 2.1042167780000005, 2.16479823, 2.044298769, 2.103464629, 2.078716434, 2.186982715, 2.096798928, 2.0726237019999996, 2.157770511, 2.090259546, 2.075583323, 2.174675618, 2.0699040870000003, 2.103918654, 2.055894848, 2.082991775, 2.0737523390000003, 2.1017382270000002, 2.077696195, 2.111215734, 2.074503726, 2.054741759, 2.089147954, 2.02801765, 2.088754364, 2.071260097, 2.1402858630000003, 2.062951518, 2.105140241, 2.067884635, ]
    0-1-18-7-3	11	[2.153331553, 2.217980131, 2.0789024329999997, 2.1293707979999996, 2.1426694129999997, 2.092677587, 2.113197282, 2.14277049, 2.209211169, 2.203949155, 2.172405017, 2.162534869, 2.1305041740000004, 2.135432786, 2.089712676, 2.0872703400000003, 2.066304234, 2.109497946, 2.1316141550000003, 2.139971094, 2.175670083, 2.184991424, 2.1510539939999997, 2.080801851, 2.13615165, 2.188604235, 2.142089205, 2.073263371, 2.1423583660000003, 2.165489262, 2.16493533, 2.137211132, 2.161438015, 2.1128076470000003, 2.128613767, 2.203010819, 2.124908037, 2.102236509, 2.245594379, 2.127644857, ]
  }

  Sorry for the complex format, it's little-thought-out and a little historical.
}

@defproc[(make-reticulated-info [ps reticulated-data?]) reticulated-info?]{
  Build a @tech{performance info} structure from a Typed Racket dataset.
}

@defproc[(reticulated-info? [x any/c]) boolean?]{
  Predicate for Reticulated datasets.
}

@defproc[(reticulated-info->sample-info [ri reticulated-info?]) sample-info?]{
  Extend the given info structure with the sample datasets for the benchmark,
   if any.

  Avoid this function.
  It is likely to disappear in a future release.
}


@; -----------------------------------------------------------------------------
@section[#:tag "gtp-support"]{Support}

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
