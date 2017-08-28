#lang racket/base

;; About the performance of one program

(require racket/contract)
(provide
  performance-info

  (contract-out
   [make-performance-info
    (-> symbol?
        #:src path-string?
        #:num-units natural?
        #:baseline-runtime nonnegative-real/c
        #:untyped-runtime nonnegative-real/c
        #:typed-runtime nonnegative-real/c
        #:make-in-configurations (-> performance-info? sequence?)
        performance-info?)]

   [performance-info?
    (-> any/c boolean?)]
   ;; Predicate for instances of the `performance-info` struct

   [performance-info->name
    (-> performance-info? symbol?)]

   [performance-info->src
    (-> performance-info? path-string?)]

   [performance-info->num-units
    (-> performance-info? natural?)]
   ;; Count the number of annotatable-positions in the benchmark (for our experiment)
   ;; A benchmark with `F` functions and `C` classes with fields and `M` methods
   ;;  across all the classes has `F + C + M` types.

   [performance-info->num-configurations
    (-> performance-info? natural?)]
   ;; Count the number of configurations in a benchmark

   [performance-info->baseline-runtime
    (-> performance-info? nonnegative-real/c)]

   [performance-info->untyped-runtime
    (-> performance-info? nonnegative-real/c)]

   [performance-info->typed-runtime
    (-> performance-info? nonnegative-real/c)]

   [performance-info->make-in-configurations
    (-> performance-info? any)]

   [performance-info-update-src
    (-> performance-info? path-string? performance-info?)]

   [in-configurations
    (-> performance-info? (sequence/c configuration-info?))]

   [count-configurations
    (-> performance-info? (-> configuration-info? any) natural?)]
   ;; Count the number of configurations
   ;;  (encapsulted by the given `performance-info` struct)
   ;;  that satisfy the given predicate.

   [deliverable
    (-> real? (-> performance-info? natural?))]
   ;; `((deliverable D) p)` returns the number of configurations in `p`
   ;; that have overhead at most `D` relative to the baseline configuration of `p`

   [fold/mean-runtime
    (-> performance-info?
        (-> any/c nonnegative-real/c any)
        #:init (or/c #f (-> real? any))
        any)]

   [filter-configurations
    (-> performance-info? (-> real? any) (listof configuration-info?))]
   ;; Return the configurations whose mean running time satisfies the given predicate.

   [overhead
    (case->
     (-> performance-info? real? real?)
     (-> performance-info? (-> real? real?)))]
   ;; `(overhead p v)` returns the overhead of the running time `v` relative
   ;;  to the baseline runtime of `p`

   [make-D-deliverable?
    (-> real? performance-info? (-> (or/c configuration-info? real?) boolean?))]
   ;; Return a function that decides whether a given running time is
   ;;  D-deliverable.

   [max-overhead
    (-> performance-info? real?)]
   ;; Returns the maximum observed overhead of any configuration in `p`

   [mean-overhead
    (-> performance-info? real?)]
   ;; Returns the average overhead across all configurations in `p`

   [min-overhead
    (-> performance-info? real?)]
   ;; Returns the lowest observed overhead of any configuration in `p`

   [typed/baseline-ratio
    (-> performance-info? real?)]
   ;; Returns the overhead of the fully-typed configuration in `p`
   ;;  relative to the baseline performance

   [typed/untyped-ratio
    (-> performance-info? real?)]
   ;; Returns the overhead of the fully-typed configuration in `p`
   ;;  relative to the untyped configuration

   [untyped/baseline-ratio
    (-> performance-info? real?)]
   ;; Returns the overhead of the untyped configuration in `p`
   ;;  relative to the baseline configuration
))

(require
  gtp-plot/private/configuration-info
  gtp-plot/private/util
  (only-in racket/sequence
    sequence/c)
  (only-in racket/math
    natural?))

;; =============================================================================

(struct performance-info (
  name
  src
  num-units
  baseline-runtime
  untyped-runtime
  typed-runtime
  make-in-configurations
) #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<performance-info:~a>" (performance-info-name v)))])

(define (make-performance-info name #:src k
                                    #:num-units num-units
                                    #:baseline-runtime baseline
                                    #:untyped-runtime untyped
                                    #:typed-runtime typed
                                    #:make-in-configurations mic)
  (performance-info name k num-units baseline untyped typed mic))

(define performance-info->name
  performance-info-name)

(define performance-info->src
  performance-info-src)

(define performance-info->num-units
  performance-info-num-units)

(define (performance-info->num-configurations pi)
  (define units (performance-info->num-units pi))
  (expt 2 units))

(define performance-info->baseline-runtime
  performance-info-baseline-runtime)

(define performance-info->untyped-runtime
  performance-info-untyped-runtime)

(define performance-info->typed-runtime
  performance-info-typed-runtime)

(define performance-info->make-in-configurations
  performance-info-make-in-configurations)

(define (performance-info-update-src pi new-src)
  (performance-info (performance-info->name pi)
                    new-src
                    (performance-info->num-units pi)
                    (performance-info->baseline-runtime pi)
                    (performance-info->untyped-runtime pi)
                    (performance-info->typed-runtime pi)
                    (performance-info-make-in-configurations pi)))

(define (in-configurations pi) ;; awkward implementation, but ok API
  ((performance-info-make-in-configurations pi) pi))

(define overhead
  (case-lambda
   [(pi v)
    (/ v (performance-info->baseline-runtime pi))]
   [(pi)
    (let ([baseline (performance-info->baseline-runtime pi)])
      (λ (v) (/ v baseline)))]))

;; fold-configurations : (All (A) performance-info? (-> A B A) #:init (U #f (-> B A)) #:transform (U #f (-> Configuration B) -> A)
(define (fold-configurations pi f #:init [init-f #f] #:transform [trans-f #f])
  (define gen (in-configurations pi))
  (define init
    (for/first ([cfg gen])
      (if trans-f
        (trans-f cfg)
        cfg)))
  (for/fold ([acc (if init-f (init-f init) init)])
            ([cfg gen])
    (f acc (if trans-f (trans-f cfg) cfg))))

(define (fold/mean-runtime pi f #:init [init-f #f])
  (fold-configurations pi f #:init init-f #:transform configuration-info->mean-runtime))

(define (count-configurations pi good?)
  (define (add-good? count cfg)
    (if (good? cfg) (+ count 1) count))
  (fold-configurations pi add-good? #:init (λ (cfg) (add-good? 0 cfg))))

(define ((deliverable D) pi)
  (count-configurations pi (make-D-deliverable? D pi)))

(define (filter-configurations pi keep?)
  (define (keep-it acc cfg)
    (define t (configuration-info->mean-runtime cfg))
    (if (keep? t) (cons cfg acc) acc))
  (fold-configurations pi keep-it #:init (λ (cfg) (keep-it '() cfg))))

(define (make-D-deliverable? D pi)
  (define overhead/pi (overhead pi))
  (lambda (c/r)
    (define t (if (configuration-info? c/r) (configuration-info->mean-runtime c/r) c/r))
    (<= (overhead/pi t) D)))

(define (max-overhead pi)
  (overhead pi (fold/mean-runtime pi max)))

(define (mean-overhead pi)
  (define 1/N (/ 1 (performance-info->num-configurations pi)))
  (define (avg acc v)
    (+ acc (* 1/N v)))
  (overhead pi (fold/mean-runtime pi avg #:init (λ (v) (* 1/N v)))))

(define (min-overhead pi)
  (overhead pi (fold/mean-runtime pi min)))

(define (typed/baseline-ratio pi)
  (/ (performance-info->typed-runtime pi)
     (performance-info->baseline-runtime pi)))

(define (typed/untyped-ratio pi)
  (/ (performance-info->typed-runtime pi)
     (performance-info->untyped-runtime pi)))

(define (untyped/baseline-ratio pi)
  (/ (performance-info->untyped-runtime pi)
     (performance-info->baseline-runtime pi)))

(define (quick-performance-info pi)
  (define nc (performance-info->num-configurations pi))
  (printf "~a~n" (performance-info->name pi))
  (printf "- num configs  : ~a~n" nc)
  (printf "- Python time : ~a~n" (performance-info->baseline-runtime pi))
  (printf "- untyped time : ~a~n" (performance-info->untyped-runtime pi))
  (printf "- typed time   : ~a~n" (performance-info->typed-runtime pi))
  (printf "- untyped/baseline : ~a~n" (untyped/baseline-ratio pi))
  (printf "- typed/untyped : ~a~n" (typed/untyped-ratio pi))
  (printf "- typed/baseline : ~a~n" (typed/baseline-ratio pi))
  (printf "- min overhead : ~a~n" (min-overhead pi))
  (printf "- max overhead : ~a~n" (max-overhead pi))
  (printf "- avg overhead : ~a~n" (mean-overhead pi))
  (let ([d2 ((deliverable 2) pi)])
    (printf "- 2 deliv.     : ~a (~a%)~n" d2 (rnd (pct d2 nc))))
  (let ([d5 ((deliverable 5) pi)])
    (printf "- 5 deliv.     : ~a (~a%)~n" d5 (rnd (pct d5 nc))))
  (void))

;; =============================================================================

(module+ test
  (require rackunit racket/list)

  (test-case "benchmark->performance-info:example-data"
    (let* ([t* '(2 1 4 2)]
           [pi (make-performance-info 'example
                 #:src "EXAMPLE"
                 #:num-units (log2 (length t*))
                 #:baseline-runtime (first t*)
                 #:untyped-runtime (first t*)
                 #:typed-runtime (last t*)
                 #:make-in-configurations (λ (pi) (for/list ([t (in-list t*)] [i (in-naturals)]) (configuration-info t i (list t)))))])
      (check-equal? (performance-info->num-configurations pi) 4)
      (check-equal? (min-overhead pi) 1/2)
      (check-equal? (max-overhead pi) 2)
      (check-equal? (mean-overhead pi) 11/8)
      (check-equal? (typed/untyped-ratio pi) 1)
      (check-equal? ((deliverable 1.8) pi) 4)
      (void)))
)
