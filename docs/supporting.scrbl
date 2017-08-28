#lang scribble/manual

@require[
  (for-label
    gtp-plot/system
    gtp-plot/util
    racket/base
    racket/contract
    (only-in racket/math natural?)
    (only-in openssl/md5 md5))]

@; -----------------------------------------------------------------------------
@title[#:tag "gtp-support"]{Support}

@section[#:tag "gtp-system"]{System API}
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


@section[#:tag "gtp-util"]{Other Helper Functions}
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

