#lang scribble/manual
@require[
  (for-label
    racket/base
    racket/contract
    (only-in openssl/md5 md5))]

@title{GTP plot}
@author{Ben Greenman}

@;@defmodule[gtp-plot/plot]{
@;  
@;}

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
