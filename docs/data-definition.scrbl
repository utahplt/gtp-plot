#lang scribble/manual

@require[
  gtp-plot/configuration-info
  (for-label
    gtp-plot/configuration-info
    gtp-plot/performance-info
    gtp-plot/sample-info
    gtp-plot/util
    racket/base
    racket/contract
    (only-in racket/math natural?))]

@title[#:tag "gtp-data-definition"]{Data Definitions}

@section[#:tag "gtp-configuration-info"]{Configuration Info}
@defmodule[gtp-plot/configuration-info]

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


@section[#:tag "gtp-performance-info"]{Performance Info}
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



@section[#:tag "gtp-sample-info"]{Sample Info}
@defmodule[gtp-plot/sample-info]

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

