#lang racket/base

;; About the performance of one program

(require racket/contract)
(provide
  performance-info
  performance-info-make-in-configurations

  (contract-out
   [make-performance-info
    (-> symbol?
        #:src (or/c #f path-string?)
        #:num-units natural?
        #:num-configurations natural?
        #:baseline-runtime* (listof nonnegative-real/c)
        #:untyped-runtime* (listof nonnegative-real/c)
        #:typed-runtime* (listof nonnegative-real/c)
        #:make-in-configurations (-> performance-info? sequence?)
        performance-info?)]

   [performance-info?
    (-> any/c boolean?)]
   ;; Predicate for instances of the `performance-info` struct

   [performance-info->name
    (-> performance-info? symbol?)]

   [performance-info->src
    (-> performance-info? (or/c #f path-string?))]

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

   [performance-info->baseline-runtime*
    (-> performance-info? (listof nonnegative-real/c))]

   [performance-info->untyped-runtime*
    (-> performance-info? (listof nonnegative-real/c))]

   [performance-info->typed-runtime*
    (-> performance-info? (listof nonnegative-real/c))]

   [performance-info->make-in-configurations
    (-> performance-info? any)]

   [performance-info-update-name
    (-> performance-info? symbol? performance-info?)]

   [performance-info-update-src
    (-> performance-info? path-string? performance-info?)]

   [performance-info-update-in-configurations
    (-> performance-info? any/c performance-info?)]

   [performance-info*->unique-name*
    (-> (listof performance-info?) (unique-listof string?))]

   [in-configurations
    (-> performance-info? (sequence/c configuration-info?))]

   [count-configurations
    (-> performance-info? (-> configuration-info? any) natural?)]
   ;; Count the number of configurations
   ;;  (encapsulted by the given `performance-info` struct)
   ;;  that satisfy the given predicate.

   [deliverable
    (-> nonnegative-real/c (-> performance-info? natural?))]
   ;; `((deliverable D) p)` returns the number of configurations in `p`
   ;; that have overhead at most `D` relative to the baseline configuration of `p`

   [fold/mean-runtime
    (-> performance-info?
        (-> any/c nonnegative-real/c any)
        #:init (or/c #f (-> nonnegative-real/c any))
        any)]

   [filter-configurations
    (-> performance-info? (-> nonnegative-real/c any) (listof configuration-info?))]
   ;; Return the configurations whose mean running time satisfies the given predicate.

   [overhead
    (case->
     (-> performance-info? nonnegative-real/c nonnegative-real/c)
     (-> performance-info? (-> nonnegative-real/c nonnegative-real/c)))]
   ;; `(overhead p v)` returns the overhead of the running time `v` relative
   ;;  to the baseline runtime of `p`

   [make-D-deliverable?
    (-> nonnegative-real/c performance-info? (-> (or/c configuration-info? nonnegative-real/c) boolean?))]
   ;; Return a function that decides whether a given running time is
   ;;  D-deliverable.

   [max-overhead
    (-> performance-info? nonnegative-real/c)]
   ;; Returns the maximum observed overhead of any configuration in `p`

   [max-overhead-configuration
    (-> performance-info? configuration-info?)]
   ;; Returns the configuration with max. overhead

   [mean-overhead
    (-> performance-info? nonnegative-real/c)]
   ;; Returns the average overhead across all configurations in `p`

   [min-overhead
    (-> performance-info? nonnegative-real/c)]
   ;; Returns the lowest observed overhead of any configuration in `p`

   [min-overhead-configuration
    (-> performance-info? configuration-info?)]
   ;; Returns the configuration with min. overhead

   [typed/baseline-ratio
    (-> performance-info? nonnegative-real/c)]
   ;; Returns the overhead of the fully-typed configuration in `p`
   ;;  relative to the baseline performance

   [typed/untyped-ratio
    (-> performance-info? nonnegative-real/c)]
   ;; Returns the overhead of the fully-typed configuration in `p`
   ;;  relative to the untyped configuration

   [untyped/baseline-ratio
    (-> performance-info? nonnegative-real/c)]
   ;; Returns the overhead of the untyped configuration in `p`
   ;;  relative to the baseline configuration

   [filter-performance-info
    (-> performance-info? (-> configuration-info? any/c) performance-info?)]

   [performance-info%best-typed-path
     (-> performance-info? natural? (-> configuration-info? configuration-info? (or/c #f exact-integer?)) performance-info?)]
))

(require
  gtp-plot/configuration-info
  gtp-plot/util
  (only-in racket/list
    group-by
    remove-duplicates)
  (only-in racket/sequence
    sequence/c)
  (only-in math/statistics
    mean)
  (only-in racket/math
    natural?))

(define (unique-listof ctc)
  (and/c (listof ctc)
         no-duplicates?))

(define (no-duplicates? x*)
  (= (length x*)
     (length (remove-duplicates x*))))

;; =============================================================================

(struct performance-info (
  name
  src
  num-units
  num-configurations
  baseline-runtime*
  untyped-runtime*
  typed-runtime*
  make-in-configurations
) #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<performance-info:~a>" (performance-info-name v)))])

(define (make-performance-info name #:src k
                                    #:num-units num-units
                                    #:num-configurations num-configurations
                                    #:baseline-runtime* baseline
                                    #:untyped-runtime* untyped
                                    #:typed-runtime* typed
                                    #:make-in-configurations mic)
  (performance-info name k num-units num-configurations baseline untyped typed mic))

(define performance-info->name
  performance-info-name)

(define performance-info->src
  performance-info-src)

(define performance-info->num-units
  performance-info-num-units)

(define performance-info->num-configurations
  performance-info-num-configurations)

(define performance-info->baseline-runtime*
  performance-info-baseline-runtime*)

(define performance-info->untyped-runtime*
  performance-info-untyped-runtime*)

(define performance-info->typed-runtime*
  performance-info-typed-runtime*)

(define (performance-info->baseline-runtime pi)
  (mean (performance-info-baseline-runtime* pi)))

(define (performance-info->untyped-runtime pi)
  (mean (performance-info-untyped-runtime* pi)))

(define (performance-info->typed-runtime pi)
  (mean (performance-info-typed-runtime* pi)))

(define performance-info->make-in-configurations
  performance-info-make-in-configurations)

(define (performance-info-update-name pi new-name)
  (performance-info new-name
                    (performance-info->src pi)
                    (performance-info->num-units pi)
                    (performance-info->num-configurations pi)
                    (performance-info->baseline-runtime* pi)
                    (performance-info->untyped-runtime* pi)
                    (performance-info->typed-runtime* pi)
                    (performance-info-make-in-configurations pi)))

(define (performance-info-update-src pi new-src)
  (performance-info (performance-info->name pi)
                    new-src
                    (performance-info->num-units pi)
                    (performance-info->num-configurations pi)
                    (performance-info->baseline-runtime* pi)
                    (performance-info->untyped-runtime* pi)
                    (performance-info->typed-runtime* pi)
                    (performance-info-make-in-configurations pi)))

(define (performance-info-update-in-configurations pi new-ic)
  (performance-info (performance-info->name pi)
                    (performance-info->src pi)
                    (performance-info->num-units pi)
                    (performance-info->num-configurations pi)
                    (performance-info->baseline-runtime* pi)
                    (performance-info->untyped-runtime* pi)
                    (performance-info->typed-runtime* pi)
                    new-ic))

(define (performance-info*->unique-name* pi*)
  (define base-name* (map performance-info->name pi*))
  (if (no-duplicates? base-name*)
    (map symbol->string base-name*)
    (for/list ([bn (in-list base-name*)]
               [i (in-naturals)])
      (format "~a (~a)" bn i))))

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
(define (fold-configurations pi f #:init [pre-init-f #f] #:transform [pre-trans-f #f])
  (define init-f (or pre-init-f values))
  (define trans-f (or pre-trans-f values))
  (for/fold ([acc (void)])
            ([pre-cfg (in-configurations pi)]
             [first? (in-sequences (in-value #t) (in-cycle (in-value #f)))])
    (define cfg (trans-f pre-cfg))
    (if first?
      (init-f cfg)
      (f acc cfg))))

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

(define (max-overhead-configuration pi)
  (define (keep-max acc cfg)
    (if (>= (configuration-info->mean-runtime acc) (configuration-info->mean-runtime cfg))
      acc
      cfg))
  (fold-configurations pi keep-max))

(define (min-overhead-configuration pi)
  (define (keep-min acc cfg)
    (if (<= (configuration-info->mean-runtime acc) (configuration-info->mean-runtime cfg))
      acc
      cfg))
  (fold-configurations pi keep-min))

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

(define (filter-performance-info pi keep-cfg?)
  (define old-name (performance-info-name pi))
  ;; old -> new
  (define new-name (string->symbol (format "~a%~a" old-name (object-name keep-cfg?))))
  (define-values [new-num-configurations in-filtered-configurations]
    (let ([cfg* (for/list ([cfg (in-configurations pi)] #:when (keep-cfg? cfg)) cfg)])
      (values (length cfg*)
              (lambda (_pi) cfg*))))
  (make-performance-info new-name
                         #:src #false
                         #:num-units (performance-info-num-units pi)
                         #:num-configurations new-num-configurations
                         #:baseline-runtime* (performance-info-baseline-runtime* pi)
                         #:untyped-runtime* (performance-info-untyped-runtime* pi)
                         #:typed-runtime* (performance-info-typed-runtime* pi)
                         #:make-in-configurations in-filtered-configurations))

(define (performance-info%best-typed-path pi max-dist cfg-dist)
  (define new-name
    (let ((old-name (performance-info-name pi)))
      (string->symbol (format "~a+~a type~a" old-name max-dist (if (= max-dist 1) "" "s")))))
  (define num-configs (performance-info-num-configurations pi))
  (define new-untyped-t* (box #f)) ;; gotta infer the untyped configuration, we don't know the order
  (define num-<=? (box 0))
  (define cfg*
    (for/list ((orig-cfg (in-configurations pi)))
      (define orig-id (configuration-info->id orig-cfg))
      (define orig-num-types (configuration-info->num-types orig-cfg))
      (define orig-t* (configuration-info->runtime* orig-cfg))
      (define best-t*
        (begin ;; look at all configurations, pick best time out of all in range
          (set-box! num-<=? 0)
          (for/fold ((acc-t* orig-t*)
                     (acc-mean (mean orig-t*))
                     #:result acc-t*)
                    ((cfg (in-configurations pi)))
            (define new-t*
              (let ((dist (cfg-dist orig-cfg cfg)))
                (and dist ;; #f means unrelated
                     (<= 0 dist)
                     (set-box! num-<=? (+ 1 (unbox num-<=?)))
                     (<= dist max-dist)
                     (configuration-info->runtime* cfg))))
            (define new-mean
              (and new-t* (mean new-t*)))
            (if (and new-mean (< new-mean acc-mean))
              (values new-t* new-mean)
              (values acc-t* acc-mean)))))
      (when (= (unbox num-<=?) num-configs)
        (set-box! new-untyped-t* best-t*))
      (configuration-info orig-id orig-num-types best-t*)))
  (make-performance-info new-name
                         #:src #false
                         #:num-units (performance-info-num-units pi)
                         #:num-configurations num-configs
                         #:baseline-runtime* (performance-info-baseline-runtime* pi)
                         #:untyped-runtime* (unbox new-untyped-t*)
                         #:typed-runtime* (performance-info-typed-runtime* pi)
                         #:make-in-configurations (lambda (_pi) cfg*)))

;; =============================================================================

(module+ test
  (require rackunit racket/list)

  (test-case "benchmark->performance-info:example-data"
    (let* ([t* '(2 1 4 2)]
           [nc (length t*)]
           [make-list-seq
             (λ (pi) (for/list ([t (in-list t*)] [i (in-naturals)]) (configuration-info t i (list t))))]
           [make-fn-seq
             (λ (pi)
               (define cfg* (make-list-seq pi))
               (define *i (box 0))
               (define (gen)
                 (define i (unbox *i))
                 (begin0
                   (and (< i nc) (list-ref cfg* i))
                   (set-box! *i (+ i 1))))
               (in-producer gen #f))]
           [pi/list
             ;; configurations in a list
             (make-performance-info 'example
               #:src "EXAMPLE"
               #:num-units (log2 nc)
               #:num-configurations nc
               #:baseline-runtime* (list (first t*))
               #:untyped-runtime* (list (first t*))
               #:typed-runtime* (list (last t*))
               #:make-in-configurations make-list-seq)]
           [pi/fn
             ;; configurations in a stateful producer
             (make-performance-info 'example/state
                 #:src "EXAMPLE/STATE"
                 #:num-units (log2 nc)
                 #:num-configurations nc
                 #:baseline-runtime* (list (first t*))
                 #:untyped-runtime* (list (first t*))
                 #:typed-runtime* (list (last t*))
                 #:make-in-configurations make-fn-seq)])
      (define (run-tests pi)
        (check-equal? (performance-info->num-configurations pi) 4)
        (check-equal? (min-overhead pi) 1/2)
        (check-equal? (max-overhead pi) 2)
        (check-equal? (configuration-info->id (min-overhead-configuration pi)) 1)
        (check-equal? (configuration-info->id (max-overhead-configuration pi)) 4)
        (check-equal? (mean-overhead pi) (mean (map (let ((baseline (car t*))) (lambda (i) (/ i baseline))) t*)))
        (check-equal? (typed/untyped-ratio pi) 1)
        (check-equal? ((deliverable 1.8) pi) 3)
        (let ([pi%0 (filter-performance-info pi (lambda (cfg) (zero? (configuration-info->num-types cfg))))])
          (check-not-equal? (performance-info->name pi) (performance-info->name pi%0))
          (check-false (performance-info->src pi%0))
          (check-equal? (list (configuration-info (car t*) 0 (list (car t*))))
                        (for/list ([cfg (in-configurations pi%0)]) cfg)))
        (check-not-exn
          (lambda () (performance-info-update-name pi 'yo)))
        (check-not-exn
          (lambda () (performance-info-update-src pi #false))))
      (run-tests pi/list)
      (run-tests pi/fn)
      (void)))

  (test-case "performance-info%best-typed-path"
    (let* ([t* '(1 2.5 5 3.3 1.5 1.7 1.7 0.5)]
           [nc (length t*)]
           [make-list-seq
             (λ (_pi) (for/list ([t (in-list t*)] [i (in-naturals)])
                        (configuration-info i i (list t))))]
           [cfg-pos-dist
             (lambda (c0 c1)
               (define id0 (configuration-info->id c0))
               (define id1 (configuration-info->id c1))
               (and (<= id0 id1)
                    (integer-count-bits (- id1 id0))))]
           [pi
             ;; configurations in a list
             (make-performance-info 'example
               #:src "EXAMPLE"
               #:num-units (log2 nc)
               #:num-configurations nc
               #:baseline-runtime* (list (first t*))
               #:untyped-runtime* (list (first t*))
               #:typed-runtime* (list (last t*))
               #:make-in-configurations make-list-seq)]
           [pi%1
             (performance-info%best-typed-path pi 1 cfg-pos-dist)]
           [pi%2
             (performance-info%best-typed-path pi 2 cfg-pos-dist)])
      (check-equal? ((deliverable 2) pi) 5)
      (check-equal? ((deliverable 2) pi%1) 8)
      (check-equal? ((deliverable 2) pi%2) 8)
      (check-equal? ((deliverable 0.6) pi) 1)
      (check-equal? ((deliverable 0.6) pi%1) 4)
      (check-equal? ((deliverable 0.6) pi%2) 7)))
)
