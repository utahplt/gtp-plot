gtp-plot
===
[![Build Status](https://travis-ci.org/bennn/gtp-plot.svg)](https://travis-ci.org/bennn/gtp-plot)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/gtp-plot/index.html)

For visualizing gradual typing performance.

![tetris-6.9-performance](examples/tetris-6.9.png)


### Motivation

1. If you have a gradual typing system
2. and a program
3. and multiple, gradually-typed versions of that program (configurations)
4. and have measured the performance of these configurations
5. then this library should help you plot the results.


### Install

```
$ raco pkg install gtp-plot
```

or

```
$ git clone https://github.com/bennn/gtp-plot
$ raco pkg install ./gtp-plot
```

### History

Influenced by:
- <https://github.com/nuprl/retic_performance>
- <https://github.com/nuprl/gradual-typing-performance>

but those repos are tailored to specific measuring projects.

Thank you Neil Toronto for the Racket [plot](https://github.com/racket/plot) library.
