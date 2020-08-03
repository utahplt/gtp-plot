#lang scribble/manual

@require[
  gtp-plot/configuration-info
  scribble/example
  (for-label
    gtp-plot/configuration-info
    gtp-plot/performance-info
    gtp-plot/sample-info
    gtp-plot/util
    racket/base
    racket/contract
    (only-in racket/sequence sequence/c)
    (only-in racket/math natural?))]

@title[#:tag "gtp-data-definition"]{Data Definitions}

@section[#:tag "gtp-configuration-info"]{Configuration Info}
@defmodule[gtp-plot/configuration-info]

@defstruct[configuration-info ([id any/c] [num-types natural?] [runtime* nonnegative-real/c]) #:prefab]{
  A @deftech{configuration info} structure describes the performance of
   one gradually-typed configuration of a program.

  The @racket[id] field is an identifier for the configuration; the purpose
   is to distinguish one configuration from other gradually-typed versions
   of the same program.
  The @racket[num-types] field is the number of type annotations in the program.
  The @racket[runtime*] field is a list of running times for the configuration.

  For example, a fully-typed Typed Racket program with @racket[_N] modules
   can be gradually typed in @racket[(expt 2 _N)] different ways by removing
   some of the @racket[_N] type annotations.
  The @racketmodname[gtp-plot/typed-racket-info] module represents configurations
   with "bitstrings" --- strings of @racket[#\0] and @racket[#\1] characters ---
   based on the alphabetical ordering of the programs' modules.
  With this in mind, here are some configurations for a Typed Racket program
   with four modules:

  @racketblock[
    #s(configuration-info "0000" 0 (4133 4074 4163))
    #s(configuration-info "0001" 1 (6380 6189 6423))
    #s(configuration-info "0010" 1 (11075 10937 11863))
    #s(configuration-info "0011" 2 (9384 9740 9418))
  ]
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

  A @deftech{performance info} structure contains performance data for one
   gradually typed program.
  An @deftech{exhaustive performance info} structure contains data for
   all configurations in the program.
  An @math{(r,s)} simple-random-approximate @deftech[#:key "SRA performance info"]{(SRA) performance info}
   structure contains data for @math{r} samples of configurations where each
   sample contains data for @math{s} configurations chosen uniformly at random.
}

@defproc[(performance-info? [x any/c]) boolean?]{
  Predicate for @tech{performance info} structures.
}

@defproc[(make-performance-info [name symbol?]
                                [#:src src (or/c #f path-string?)]
                                [#:num-units num-units natural?]
                                [#:num-configurations num-configurations natural?]
                                [#:baseline-runtime* baseline-runtime* (listof nonnegative-real/c)]
                                [#:untyped-runtime* untyped-runtime* (listof nonnegative-real/c)]
                                [#:typed-runtime* typed-runtime* (listof nonnegative-real/c)]
                                [#:make-in-configurations make-in-configurations (-> performance-info? (sequence/c configuration-info?))])
         performance-info?]{
  Builds a @tech{performance info} structure for the data for a gradually-typed program.

  @itemlist[
  @item{
    @racket[name] is the name of the program;
  }
  @item{
    @racket[src] is a performance data for the program
     (or @racket[#f] for datasets that do not live in a file),
     this file can be in any format that @racket[make-in-configurations] understands;
  }
  @item{
    @racket[num-units] is the number of @tech{typeable components} in the program;
  }
  @item{
    @racket[num-configurations] is the number of @tech{configurations} in the program;
  }
  @item{
    @racket[baseline-runtime*] is the performance of the given program without any gradual typing;
  }
  @item{
    @racket[untyped-runtime*] is the performance of the program with no type annotations,
     this may be the same as the baseline runtime;
  }
  @item{
    @racket[typed-runtime*] is the performance of the program when fully-typed;
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
  @defproc[(performance-info->src [pi performance-info?]) (or/c #f path-string?)]
  @defproc[(performance-info->num-units [pi performance-info?]) natural?]
  @defproc[(performance-info->baseline-runtime* [pi performance-info?]) (listof nonnegative-real/c)]
  @defproc[(performance-info->untyped-runtime* [pi performance-info?]) (listof nonnegative-real/c)]
  @defproc[(performance-info->typed-runtime* [pi performance-info?]) (listof nonnegative-real/c)]
  @defproc[(performance-info->baseline-runtime [pi performance-info?]) nonnegative-real/c]
  @defproc[(performance-info->untyped-runtime [pi performance-info?]) nonnegative-real/c]
  @defproc[(performance-info->typed-runtime [pi performance-info?]) nonnegative-real/c]
)]{
  Getter functions.
}

@deftogether[(
  @defproc[(performance-update-name [pi performance-info?]) performance-info?]
  @defproc[(performance-update-src [pi performance-info?]) performance-info?]
)]{
  Copy-and-update functions.
}

@defproc[(performance-info->num-configurations [pi performance-info?]) natural?]{
  Return the number of configurations in the program.
  Same as @racket[(expt 2 (performance-info->num-units _pi))].
}

@defproc[(filter-performance-info [pi performance-info?] [keep-cfg? (-> configuration-info? any/c)]) performance-info?]{
  Returns a @racket[performance-info] structure with all configurations @racket[_cfg] in @racket[pi]
   such that @racket[(keep-cfg? _cfg)] is not @racket[#false].
  Additionally the new performance info will have a different name and source
   than @racket[pi].

  @codeblock{
    #lang racket/base
    (require
      gtp-plot/configuration-info
      gtp-plot/performance-info)
    ;; Keep fully-typed configurations, and 'fully-typed but for 1-2 units'
    (define (keep-nearly-typed-configs pi)
      (define max-types (performance-info->num-units pi))
      (define min-types (max (- max-types 2) 0))
      (define (nearly-typed? cfg)
        (<= min-types (configuration-info->num-types cfg) max-types))
      (filter-performance-info pi nearly-typed?))
  }
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

@defproc[(count-configurations [pi performance-info?] [f (-> configuration-info? any)]) natural?]{
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
  @defproc[(max-overhead-configuration [pi performance-info?]) configuration-info?]
  @defproc[(min-overhead-configuration [pi performance-info?]) configuration-info?]
)]{
  Return a configuration with max. or min. overhead.
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
  Predicate for @tech{SRA performance info} structures.
}

@defproc[(make-sample-info [pi performance-info?] [samples (listof (listof configuration-info?))]) sample-info?]{
  Make an @tech{SRA performance info} structure from a @tech{performance info} structure
   and sampled configurations.
  Logs an @racket['error]-level message to @racket[gtp-plot-logger] if
   the sampled datasets do not all contain the same number of configurations.
}

@defproc[(sample-info->sample-size [si sample-info?]) natural?]{
  Count the number of configurations in each sample in the given @tech{SRA performance info}
   structure.
}

@defproc[(sample-info->num-samples [si sample-info?]) natural?]{
  Count the number of samples in the given structure.
}

@defproc[(sample-info->performance-info* [si sample-info?]) (listof performance-info?)]{
  Expand a @tech{SRA performance info} structure to a list of @tech{performance info} structures.
  Each structure represents one sample of configurations.
}
