#lang scribble/manual

@require[
  gtp-plot/reticulated-info
  gtp-plot/typed-racket-info
  (for-label
    gtp-plot/configuration-info
    gtp-plot/performance-info
    gtp-plot/reticulated-info
    gtp-plot/sample-info
    gtp-plot/typed-racket-info
    gtp-plot/util
    racket/base
    racket/contract
    (only-in racket/math natural?))]

@; -----------------------------------------------------------------------------
@title[#:tag "data-formats"]{Data Formats}

@section[#:tag "gtp-typed-racket-info"]{Typed Racket Info}
@defmodule[gtp-plot/typed-racket-info]

@defproc[(typed-racket-data? [ps path-string?]) boolean?]{
  A Typed Racket dataset:
  @itemlist[
  @item{
    lives in a file named @filepath{NAME-vX-OTHER.rktd},
     where @filepath{NAME} is the name of the program
     and @filepath{vX} is a Racket version number,
     and @filepath{OTHER} is any string (used to distinguish this data from other data with the same prefix).

    @bold{or} lives in a file with any name where the first non-whitespace
     characters on the first non-blank-line, non-comment
     line are the @litchar{#(} characters.

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


@section[#:tag "gtp-reticulated-info"]{Reticulated Info}
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
    @tt{ID} is a hyphen-separated sequence of digits.
    These represent a configuration as a @emph{mixed-radix number};
     the radix of position @math{k} is the number of @tech{typeable components}
     in module @math{k} of the program.
    @margin-note{Wikipedia: @hyperlink["https://en.wikipedia.org/wiki/Mixed_radix"]{mixed-radix number}}
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
  Build a @tech{performance info} structure from a Reticulated dataset.
}

@defproc[(reticulated-info? [x any/c]) boolean?]{
  Predicate for Reticulated datasets.
}

