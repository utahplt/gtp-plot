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
    (-> performance-info? (-> real? any) natural?)]
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
    (-> real? performance-info? (-> real? boolean?))]
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
  (define (add-good? count t)
    (if (good? t) (+ count 1) count))
  (fold/mean-runtime pi add-good? #:init (λ (t0) (add-good? 0 t0))))

(define ((deliverable D) pi)
  (count-configurations pi (make-D-deliverable? D pi)))

(define (filter-configurations pi keep?)
  (define (keep-it acc cfg)
    (define t (configuration-info->mean-runtime cfg))
    (if (keep? t) (cons cfg acc) acc))
  (fold-configurations pi keep-it #:init (λ (cfg) (keep-it '() cfg))))

(define (make-D-deliverable? D pi)
  (define overhead/pf (overhead pi))
  (lambda (t)
    (<= (overhead/pf t) D)))

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

#;(module+ test
  (require rackunit racket/runtime-path rackunit-abbrevs)

  (define-runtime-path karst-example "./test/karst-example_tab.gz")

  (test-case "benchmark->performance-info:example-data"
    (define karst-example-gunzip (gunzip/cd karst-example))
    (define-values [num-configs configs/module* base-retic typed-retic]
      (scan-karst-file karst-example-gunzip))
    (check-equal? num-configs 4)
    (check-equal? configs/module* '(2 2))
    (check-equal? base-retic 10)
    (check-equal? typed-retic 20)

    (let ([pi (make-performance-info 'example
                #:src karst-example-gunzip
                #:num-configurations num-configs
                #:python-runtime base-retic
                #:untyped-retic-runtime base-retic
                #:typed-retic-runtime typed-retic)])
      (check-equal? (num-configurations pi) 4)
      (check-equal? (min-overhead pi) 1/2)
      (check-equal? (max-overhead pi) 10)
      (check-equal? (mean-overhead pi) 27/8)
      (check-equal? (typed/retic-ratio pi) 2)
      (check-equal? ((deliverable 2) pi) 3)
      (check-equal? ((deliverable 10) pi) 4)
      (let () ;; filter-time* tests
        (check-equal? (filter-time* pi (λ (t) (= t 100))) (list 100))
        (check-equal? (filter-time* pi (λ (t) (= t 5))) (list 5))
        (check-equal? (filter-time* pi (λ (t) (< t 20))) (list 10 5))
        (void))
      (void)))

  (test-case "benchmark->performance-info:no-data"
    (check-pred performance-info?
      (benchmark->performance-info (->benchmark-info 'stats))))

  ;; general correctness/sanity for a real program
  (let* ([bm (->benchmark-info 'Espionage)]
         [pf (benchmark->performance-info bm)])
    (test-case "performance-info:spot-check"
      (check-true (performance-info? pi))
      (check <= (performance-info-python-runtime pi) (performance-info-untyped-runtime pi))
      (let* ([lo (min-overhead pi)]
             [hi (max-overhead pi)]
             [avg (mean-overhead pi)]
             [nc (num-configurations pi)]
             [d2 ((deliverable 2) pi)]
             [d3 ((deliverable 3) pi)]
             [dhi ((deliverable hi) pi)])
        (check <= lo hi)
        (check <= lo avg)
        (check <= avg hi)
        (check <= d2 nc)
        (check <= d2 d3)
        (check-equal? dhi nc)
        (void)))

    (test-case "quick-stats:spot-check"
      (define quick-stats-str
        (let ([sp (open-output-string)])
          (parameterize ([current-output-port sp])
            (quick-performance-info 'Espionage))
          (begin0 (get-output-string sp) (close-output-port sp))))
      (define m (regexp-match #rx"avg overhead : ([.0-9]+)\n" quick-stats-str))
      (check-true (pair? m))
      (check-equal? (string->number (cadr m)) (mean-overhead pi))
      (void))
  )

  (test-case "typed-configuration?"
    (check-true (typed-configuration? '(0 0 0)))
    (check-true (typed-configuration? '()))
    (check-false (typed-configuration? '(1 0)))
    (check-false (typed-configuration? '(9 8 7 7 9))))

  (test-case "parse-line"
    (check-equal?
      (parse-line "0-0	4	[1, 2, 2, 3]")
      (list "0-0" "4" "[1, 2, 2, 3]"))
    (check-exn exn:fail:contract?
      (λ () (parse-line ""))))

  (test-case "string->num-types"
    (check-equal?
      (string->num-types "8")
      8)
    (check-exn exn:fail:contract?
      (λ () (string->num-types "0.3")))
    (check-exn exn:fail:contract?
      (λ () (string->num-types "#f"))))

  (test-case "string->time*"
    (check-equal?
      (string->time* "[1, 2, 3]")
      '(1 2 3))
    (check-equal?
      (string->time* "[1.23, 4.554]")
      '(1.23 4.554))
    (check-exn exn:fail:contract?
      (λ () (string->time* "[]")))
    (check-exn exn:fail:contract?
      (λ () (string->time* "[1, -2]"))))

  (test-case "samples"
    (define (check-sample* bm-name)
      (define pi (benchmark->performance-info (->benchmark-info bm-name)))
      (define n+s* (performance-info->sample* pi))
      (define num-configs (car n+s*))
      (define s* (cdr n+s*))
      (check-true (< 0 (length s*)) "positive number of sample files")
      (define count* (map count-karst-lines s*))
      (check-true (apply = num-configs count*))
      (void))

    (check-sample* 'Espionage))

  (test-case "count-types"
    (check-apply* count-types
     ['(0) '(2)
      ==> 1]
     ['(1) '(2)
      ==> 0]
     ['(0 9) '(128 32)
      ==> 10]
     ['(2 16) '(128 32)
      ==> 10]
     ['(5 9) '(128 32)
      ==> 8]
     ['(0) '(1)
      ==> 0]
     ['(6 18) '(128 32)
      ==> 8]
     ['(13 18) '(128 32)
      ==> 7]
     ['(14 17 30 2) '(16 64 32 4)
      ==> 7]
     ['(15 55 31 3) '(16 64 32 4)
      ==> 1]
     ['(3 12 21 0) '(16 64 32 4)
      ==> 10]
     ['(0 6 24 1) '(16 64 32 4)
      ==> 12]
     ['(0 6 8 1) '(16 64 32 4)
      ==> 13]
     ['(0 6 8 0) '(16 64 32 4)
      ==> 14]))

  (test-case "successors"
    (check-apply* successors
     ['(1) '(32)
      ==> '((0))]
     ['(2) '(32)
      ==> '((0))]
     ['(16) '(32)
      ==> '((0))]
     ['(3) '(32)
      ==> '((1) (2))]))

  (test-case "string-set"
    (check-apply* string-set
     ["hello" 0 #\H
      ==> "Hello"]
     ["hello" 4 #\H
      ==> "hellH"]))

  (let ()
    (define futen (->benchmark-info 'futen))
    (define spectralnorm (->benchmark-info 'spectralnorm))
    (define call_method (->benchmark-info 'call_method))
    (define fannkuch (->benchmark-info 'fannkuch))

    (test-case "better-with-types"
      (check-apply* count-better-with-types
       [(list fannkuch) ==> 0]
       [(list spectralnorm) ==> 13]
       [(list call_method) ==> 66]
       [(list futen spectralnorm fannkuch) ==> 23496]))

    (test-case "find-speedy-types"
      (check-apply* find-speedy-types
       [(list fannkuch)
        ==> (make-immutable-hash '((fannkuch . ())))]
       [(list fannkuch spectralnorm)
        ==> (make-immutable-hash
              '((fannkuch . ())
                (spectralnorm . (("10" . "2") ("23" . "21") ("26" . "10")
                                 ("26" . "18") ("28" . "12") ("28" . "20")
                                 ("30" . "14") ("30" . "22") ("24" . "8")
                                 ("24" . "16") ("16" . "0") ("18" . "2")
                                 ("20" . "4") ("12" . "4") ("8" . "0")
                                 ("14" . "6") ("22" . "6")))))])))

  (test-case "has-karst-data"
    (check-true (performance-info-has-karst-data? (->performance-info 'call_method)))
    (check-false (performance-info-has-karst-data? (->performance-info 'Evolution))))

  (test-case "ratio-for-samples"
    (define (check-t/p-ratio bm-name)
      (define pi (->performance-info bm-name))
      (void ;; assert that `pi` has ONLY sample data
        (when (performance-info-has-karst-data? pi)
          (raise-user-error 'check-t/p-ratio "benchmark '~a' has more than just sample data" bm-name))
        (performance-info->sample* pi))
      (check-pred typed/python-ratio pi)
      (void))
    (check-t/p-ratio 'Evolution))

  (test-case "in-configurations/karst"
    (define pi (->performance-info 'fannkuch))
    (define x
      (for/list ([(a b c) (in-configurations pi)])
        (list a b c)))
    (check-equal? (length x) 2)
    (check-equal? (caar x) '(0))
    (check-equal? (caadr x) '(1)))

  (test-case "in-configurations/sample-data"
    (define pi (->performance-info 'sample_fsm))
    (define x
      (for/list ([(a b c) (in-configurations pi)])
        (list a b c)))
    (check-equal? (length x) 1900)
    (check-equal? (caar x) '(13 0 2 7 7)))

  (test-case "parse-typed-racket-filename"
    (check-equal? (parse-typed-racket-filename "nepls-2017/src/tr-data/mbta-v6.4-2016-07-25T06:46:31.rktd")
                  "mbta"))
)

;; -----------------------------------------------------------------------------

#;(module+ main
  (require racket/cmdline)
  (define *fix-num-types?* (make-parameter #f))
  (command-line
   #:program "perf-info"
   #:once-each
   [("--fix-num-types") "Reset the type counts in the given files" (*fix-num-types?* #t)]
   #:args benchmark-name*
   (cond
    [(null? benchmark-name*)
     (printf "usage: rp:perf-info <benchmark-name> ...~n")]
    [(*fix-num-types?*)
     (void (map fix-num-types benchmark-name*))]
    [(null? (cdr benchmark-name*))
     (quick-performance-info (car benchmark-name*))]
    [else
     (for ([n (in-list benchmark-name*)])
       (with-handlers ([exn:fail:contract? (λ (e) (printf "WARNING: failure processing '~a'~n" n))])
         (quick-performance-info n)))])))

