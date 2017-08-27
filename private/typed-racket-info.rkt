#lang racket/base

(require racket/contract/base)
(provide
  typed-racket-data?
  typed-racket-info?
  (contract-out
   [make-typed-racket-info
    (-> typed-racket-data? performance-info?)]))

(require
  gtp-plot/private/configuration-info
  gtp-plot/private/performance-info
  gtp-plot/private/util
  (only-in math/statistics
    mean)
  (only-in racket/file
    file->value))

;; =============================================================================

(struct typed-racket-info performance-info ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<typed-racket-info:~a>" (performance-info->name v)))])

(define (typed-racket-data? path)
  (and (path-string? path) (parse-typed-racket-filename path)))

(define (make-typed-racket-info path)
  (define bm-name (parse-typed-racket-filename path))
  (define v (file->value path))
  (define nc (vector-length v))
  (define nu (log2 nc))
  (define rr (mean (vector-ref v 0)))
  (define tr (mean (vector-ref v (- nc 1))))
  (typed-racket-info
    (string->symbol bm-name)
    path
    nu
    rr
    rr
    tr
    in-typed-racket-configurations))

(define (parse-typed-racket-filename ps)
  ;; expecting TR filenames to start with 'name-vXX' where XX is a Racket version
  (define-values [_base name mbd?] (split-path ps))
  (and
    (path-string? name)
    (let ([m (regexp-match #rx"^([^-]*)-v.*rktd$" (path-string->string name))])
      (and m (cadr m)))))

(define (in-typed-racket-configurations pi)
  (define go
    (let* ([v (file->value (performance-info->src pi))]
           [num-configs (vector-length v)]
           [num-units (log2 num-configs)]
           [count-hi-bits (λ (cfg)
                            (for/sum ([c (in-string cfg)]
                                      #:when (eq? c #\1))
                              1))]
           [curr (box 0)])
      (λ ()
        (define i (unbox curr))
        (cond
         [(= i num-configs)
          #f]
         [else
          (set-box! curr (+ i 1))
          (define cfg (natural->bitstring i #:pad num-units))
          (define num-types (count-hi-bits cfg))
          (define t* (vector-ref v i))
          (configuration-info cfg num-types t*)]))))
  (in-producer go #f))

(define (stop? a b c)
  (and (eq? #f a) (eq? a b) (eq? b c)))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs racket/runtime-path)

  (define-runtime-path morsecode-file "./test/morsecode-v6.4.rktd")
  (define-runtime-path gregor-file "./test/gregor-v6.4.rktd")

  (test-case "typed-racket-data?"
    (check-pred typed-racket-data? morsecode-file)
    (check-pred typed-racket-data? gregor-file)

    (check-false (typed-racket-data? "README.md"))
    (check-false (typed-racket-data? 'x))
    (check-false (typed-racket-data? 42)))

  (test-case "morsecode"
    (define M (make-typed-racket-info morsecode-file))

    (check-equal?
      (performance-info->name M)
      'morsecode)
    (check-equal?
      (performance-info->src M)
      morsecode-file)
    (check-equal?
      (performance-info->num-units M)
      4)
    (check-equal?
      (performance-info->num-configurations M)
      16)
    (check-equal?
      (performance-info->baseline-runtime M)
      (performance-info->untyped-runtime M))
    (check-equal?
      (performance-info->baseline-runtime M)
      (mean '(1328 1341 1321 1351 1326 1319 1348 1278 1319 1338)))
    (check-equal?
      (performance-info->typed-runtime M)
      (mean '(1265 1258 1285 1261 1265 1250 1238 1260 1279 1271)))
    (check-equal?
      (performance-info->src (performance-info-update-src M gregor-file))
      gregor-file)
    (check-equal?
      (for/list ([cfg (in-configurations M)]) (configuration-info->id cfg))
      '("0000" "0001" "0010" "0011" "0100" "0101" "0110" "0111"
        "1000" "1001" "1010" "1011" "1100" "1101" "1110" "1111"))
    (check-equal?
      (count-configurations M (λ (cfg) #true))
      16)
    (check-equal?
      (count-configurations M (let ([done (box #f)])
                                 (λ (cfg)
                                   (if (unbox done)
                                     #f
                                     (set-box! done #true)))))
      1)
    (check-equal?
      ((deliverable 1.0) M)
      4)
    (check-equal?
      ((deliverable 1.8) M)
      14)
    (check-equal?
      ((deliverable 5) M)
      16)
    (check-equal?
      (length (filter-configurations M (λ (r) #true)))
      16)
    (check-equal?
      (overhead M 1000)
      10000/13269)
    (check-equal?
      (overhead M 9000)
      30000/4423)
    (check-equal?
      (max-overhead M)
      8671/4423)
    (check-equal?
      (mean-overhead M)
      70873/53076)
    (check-equal?
      (min-overhead M)
      11875/13269)
    (check-equal?
      (typed/baseline-ratio M)
      (typed/untyped-ratio M))
    (check-equal?
      (typed/untyped-ratio M)
      12632/13269)
    (check-equal?
      (untyped/baseline-ratio M)
      1))

  (test-case "gregor"
    (define G (make-typed-racket-info gregor-file))

    (check-equal?
      (performance-info->name G)
      'gregor)
    (check-equal?
      (performance-info->src G)
      gregor-file)
    (check-equal?
      (performance-info->num-units G)
      13)
    (check-equal?
      (performance-info->num-configurations G)
      (expt 2 13))
    (check-equal?
      (performance-info->baseline-runtime G)
      (performance-info->untyped-runtime G))
    (check-equal?
      (performance-info->baseline-runtime G)
      19326/11)
    (check-equal?
      (performance-info->typed-runtime G)
      20657/11)
    (check-equal?
      (count-configurations G (λ (cfg) #true))
      8192)
    (check-equal?
      (count-configurations G (let ([done (box #f)])
                                 (λ (cfg)
                                   (if (unbox done)
                                     #f
                                     (set-box! done #true)))))
      1)
    (check-equal?
      ((deliverable 1.0) G)
      2)
    (check-equal?
      ((deliverable 1.8) G)
      4806)
    (check-equal?
      ((deliverable 5) G)
      8192)
    (check-equal?
      (length (filter-configurations G (λ (r) #true)))
      8192)
    (check-equal?
      (overhead G 1000)
      5500/9663)
    (check-equal?
      (overhead G 9000)
      16500/3221)
    (check-equal?
      (max-overhead G)
      21628/9663)
    (check-equal?
      (mean-overhead G)
      91811223/52772864)
    (check-equal?
      (min-overhead G)
      6393/6442)
    (check-equal?
      (typed/baseline-ratio G)
      (typed/untyped-ratio G))
    (check-equal?
      (typed/untyped-ratio G)
      20657/19326)
    (check-equal?
      (untyped/baseline-ratio G)
      1))

)
