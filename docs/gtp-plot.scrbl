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
  @deftech{dynamically typed} code avoids @tech{type errors} at @tech{run-time},
   using @tech{run-time checks}.
}
@item{
  @deftech{statically typed} code avoids @tech{type errors} at @tech{compile-time},
   using @tech{ahead-of-time checks}.
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
@defmodule[gtp-plot/performance-info]

@defidform[performance-info]{
  A struct type, exported to allow subtyping.

  A @deftech{performance info} struct contains performance data for one
   gradually typed program.
}

@defproc[(performance-info? [x any/c]) boolean?]{
  Predicate for @tech{performance info} structures.
}

@defproc[(make-performance-info [name symbol?]
                                [#:src src path-string?]
                                [#:num-units num-units natural?]
                                [#:baseline-runtime baseline-runtime nonnegative-real/c]
                                [#:untyped-runtime untyped-runtime nonnegative-real/c]
                                [#:typed-runtime typed-runtime nonnegative-real/c]
                                [#:make-in-configurations make-in-configurations (-> performance-info? (sequence/c configuration-info?))])
         performance-info?]{
  Builds a @tech{performance info} structure for the data for a gradually-typed program.

  @itemlist[
  @item{
    @racket[name] is the name of the program;
  }
  @item{
    @racket[src] is a performance data for the program,
     this file can be in any format that @racket[make-in-configurations] understands;
  }
  @item{
    @racket[num-units] is the number of @tech{typeable components} in the program;
  }
  @item{
    @racket[baseline-runtime] is the performance of the given program without any gradual typing;
  }
  @item{
    @racket[untyped-runtime] is the performance of the program with no type annotations,
     this may be the same as the baseline runtime;
  }
  @item{
    @racket[typed-runtime] is the performance of the program when fully-typed;
  }
  @item{
    @racket[make-in-configurations] is a function that accepts a @tech{performance info}
     structure (specifically, itself) and returns a sequence with the data
     for each configuration in the program.
  }
  ]
}

@deftogether[(
  @defproc[(performance-info->name [pi performance-info?]) symbol?]
  @defproc[(performance-info->src [pi performance-info?]) path-string?]
  @defproc[(performance-info->num-units [pi performance-info?]) natural?]
  @defproc[(performance-info->baseline-runtime [pi performance-info?]) nonnegative-real/c]
  @defproc[(performance-info->untyped-runtime [pi performance-info?]) nonnegative-real/c]
  @defproc[(performance-info->typed-runtime [pi performance-info?]) nonnegative-real/c]
)]{
  Getter functions.
}

@defproc[(performance-info->num-configurations [pi performance-info?]) natural?]{
  Return the number of configurations in the program.
  Same as @racket[(expt 2 (performance-info->num-units _pi))].
}

@defproc[(in-configurations [pi performance-info?]) (sequence/c configuration-info?)]{
  Returns all configuration data from the given dataset.
}

@defproc[(deliverable [D nonnegative-real/c]) (-> performance-info? natural?)]{
  Returns a predicate that counts the number of @racket[D]-@emph{deliverable} configurations
   in a program.

  A configuration is @racket[D]-@emph{deliverable} if its performance is at most
   @racket[D]x slower than the baseline runtime.
}

@defproc*[(
  [(overhead [pi performance-info?] [t nonnegative-real/c]) nonnegative-real/c]
  [(overhead [pi performance-info?]) (-> nonnegative-real/c nonnegative-real/c)]
)]{
  @racket[(overhead pi t)] returns the overhead of the running time @racket[t]
   relative to the baseline runtime of the given program.
  The second form is a curried version of the first.
}

@defproc[(count-configuration [pi performance-info?] [f (-> configuration-info? any)]) natural?]{
  Counts the number of configurations that satisfy the given predicate.
}

@defproc[(filter-configurations [pi performance-info?] [f (-> nonnegative-real/c any)]) (listof configuration-info?)]{
  Returns a list of configurations that satisfy the given performance predicate.
}

@deftogether[(
  @defproc[(max-overhead [pi performance-info?]) nonnegative-real/c]
  @defproc[(mean-overhead [pi performance-info?]) nonnegative-real/c]
  @defproc[(min-overhead [pi performance-info?]) nonnegative-real/c]
)]{
  Return the minimum, average, and maximum overheads in the given dataset.
}

@deftogether[(
  @defproc[(typed/baseline-ratio [pi performance-info?]) nonnegative-real/c]
  @defproc[(typed/untyped-ratio [pi performance-info?]) nonnegative-real/c]
  @defproc[(untyped/baseline-ratio [pi performance-info?]) nonnegative-real/c]
)]{
  Return a performance ratio.
  For example, the typed/untyped ratio is the performance of the fully-typed
   configuration divided by the performance of the untyped configuration.
}

@defproc[(fold/mean-runtime [pi performance-info?] [f (-> any/c nonnegative-real/c any)] [#:init init (or/c #f (-> nonnegative-real/c any)) #f]) any]{
  Folds over a dataset.

  If @racket[init] is @racket[#false], then the initial accumulator is the
   mean running time of some configuration and @racket[f] must return a nonnegative real number.
  If @racket[init] is a procedure, then the initial accumulator is the
   result of applying @racket[init] to an arbitrary configuration.
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

TODO


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

@defmodule[gtp-plot/util]

@defproc[(nonnegative-real/c [x any/c]) boolean?]{
  Flat contract for non-negative real numbers.
}

@defproc[(confidence-interval [r* (listof real?)] [#:cv confidence-value nonnegative-real/c 1.96]) (cons/c real? real?)]{
  Return a 95% confidence interval for the given numbers at the given confidence value.
}

@defproc[(tab-split [str string?]) (listof string?)]{
  Split a string by tab characters.
}

@defproc[(tab-join [str* (listof string?)]) string?]{
  Join a list of strings by tab characters.
}

@defproc[(path-string->string [ps path-string?]) string?]{
  Convert a path or string to a string.
}

@defproc[(ensure-directory [ps path-string?]) void?]{
  If the given directory exists, do nothing.
  Otherwise, create it.
}

@defproc[(rnd [r real?]) string?]{
  Round the given number to two decimal places.
}

@defproc[(pct [a real?] [b real?]) real?]{
  Same as @racket[(* 100 (/ a b))].
}

@defproc[(log2 [n natural?]) natural?]{
  Compute the base-2 logarithm of a number.

  Assumes @racket[n] is a power of 2.
}

@defproc[(order-of-magnitude [n real?]) natural?]{
  Count the number of digits in the given number.
}

@defproc[(file-remove-extension [ps path-string?]) path-string?]{
  Remove the extension from the given filename.
}

@defproc[(save-pict [out-path path-string?] [p pict?]) boolean?]{
  Write the given pict to the given filename in @tt{.png} format.
}

@defproc[(columnize [x* list?] [num-cols natural?]) (listof list?)]{
  Divide a list into almost-equally-sized lists.
}

@defproc[(force/cpu-time [thunk (-> any)]) (values any/c natural?)]{
  Force the given thunk and record its running time.
  Return both the result of the thunk and the CPU time (as reported by @racket[time-apply]).
}

@defproc[(natural->bitstring [n natural?] [#:pad pad natural?]) string?]{
  Return a binary representation of @racket[n] with @racket[pad] bits.
}

@defproc[(bitstring->natural [str string?]) natural?]{
  Parse a string of @racket{1} and @racket{0} digits as a binary number,
   return the base-10 representation of the parsed number.
}

@defproc[(count-zero-bits [str string?]) natural?]{
  Count the number of @racket[#\0] characters in a string of @racket{1} and
   @racket{0} digits.
}

@deftogether[(
  @defidform[gtp-plot-logger]
  @defidform[log-gtp-plot-debug]
  @defidform[log-gtp-plot-info]
  @defidform[log-gtp-plot-warning]
  @defidform[log-gtp-plot-error]
  @defidform[log-gtp-plot-fatal]
)]{
  Subscribe to @tt{gtp-logger} for diagnostics.

  See also: @racket[define-logger].
}

@; -----------------------------------------------------------------------------

@section[#:tag "gtp-glossary"]{GTP Glossary}
