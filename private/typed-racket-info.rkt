#lang racket/base

(require racket/contract/base)
(provide
  (contract-out
   [typed-racket-data?
    (-> any/c any)]
   [typed-racket-info?
    (-> any/c any)]
   [make-typed-racket-info
    (->* [typed-racket-data?] [#:name (or/c #f symbol?)] performance-info?)]
   [make-typed-racket-sample-info
    (-> (listof (listof configuration-info?))
        #:name symbol?
        #:typed-configuration typed-configuration-info?
        #:untyped-configuration untyped-configuration-info?
        sample-info?)]
   [make-typed-racket-configuration-info
     (-> typed-racket-id? (listof nonnegative-real/c) typed-racket-configuration-info?)]
   [typed-racket-configuration-info?
     (-> any/c boolean?)]
   [typed-configuration-info?
     (-> any/c boolean?)]
   [untyped-configuration-info?
     (-> any/c boolean?)]))

(require
  gtp-plot/configuration-info
  gtp-plot/performance-info
  gtp-plot/sample-info
  gtp-util
  (only-in gtp-plot/util
    path->name)
  (only-in racket/string
    string-prefix?)
  (only-in racket/file
    file->value))

;; =============================================================================

(struct typed-racket-info performance-info ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<typed-racket-info:~a>" (performance-info->name v)))])

(define (typed-racket-data? path)
  (and (path-string? path)
       (looks-like-typed-racket-data path)))

(define NON-SPACE-NON-RPAREN #rx"[^ \t\n\r)]")

(define (looks-like-typed-racket-data path)
  (with-input-from-file path
    (lambda ()
      (define first-data-line
        (for/first ((ln (in-lines))
                    #:when (and (not (whitespace-string? ln))
                                (not (simple-comment-string? ln))))
          (and (string-prefix? ln "#(") ln)))
      (and first-data-line
           (or (regexp-match? NON-SPACE-NON-RPAREN (substring first-data-line 2))
               (regexp-match? NON-SPACE-NON-RPAREN (current-input-port)))))))

(define (make-typed-racket-info path #:name [name #f])
  (define bm-name (->bm-name name path))
  (define v (file->value path))
  (define nc (vector-length v))
  (define nu (log2 nc))
  (define rr (vector-ref v 0))
  (define tr (vector-ref v (- nc 1)))
  (typed-racket-info
    bm-name
    path
    nu
    nc
    rr
    rr
    tr
    in-typed-racket-configurations))

(define (make-typed-racket-sample-info cfg**
                                       #:name name
                                       #:typed-configuration typed-config
                                       #:untyped-configuration untyped-config)
  (define num-units (typed-racket-id->num-units (configuration-info->id typed-config)))
  (define tr (configuration-info->runtime* typed-config))
  (define rr (configuration-info->runtime* untyped-config))
  (define empty-pi
    (typed-racket-info name "dummy" num-units (expt 2 num-units) rr rr tr in-typed-racket-configurations))
  (make-sample-info empty-pi cfg**))

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
           [num-units (performance-info->num-units pi)]
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
          (define cfg (natural->bitstring i #:bits num-units))
          (define num-types (count-hi-bits cfg))
          (define t* (vector-ref v i))
          (configuration-info cfg num-types t*)]))))
  (in-producer go #f))

(define (stop? a b c)
  (and (eq? #f a) (eq? a b) (eq? b c)))

;; -----------------------------------------------------------------------------

(struct typed-racket-configuration-info configuration-info () #:transparent)

(define (make-typed-racket-configuration-info id t*)
  (typed-racket-configuration-info id (typed-racket-id->num-types id) t*))

(define (typed-racket-id? str)
  (and (string? str)
       (for/and ([c (in-string str)])
         (memq c '(#\0 #\1)))))

(define (typed-racket-id->num-units str)
  (string-length str))

(define (typed-racket-id->num-types str)
  (for/sum ([c (in-string str)])
    (if (eq? c #\1) 1 0)))

(define (typed-configuration-info? tc)
  (and (typed-racket-configuration-info? tc)
       (let ([tid (configuration-info->id tc)])
         (= (configuration-info->num-types tc)
            (typed-racket-id->num-types tid)
            (typed-racket-id->num-units tid)))))

(define (untyped-configuration-info? tc)
  (and (typed-racket-configuration-info? tc)
       (= 0
          (configuration-info->num-types tc)
          (typed-racket-id->num-types (configuration-info->id tc)))))

(define (->bm-name name path)
  (or name
      (let ((n (parse-typed-racket-filename path)))
        (and n (string->symbol n)))
      (string->symbol (path->name path))))

;; =============================================================================

(module+ test
  (require
    rackunit rackunit-abbrevs racket/runtime-path
    (only-in math/statistics mean))

  (define CI? (and (getenv "CI") #true))

  (define-runtime-path morsecode-file "./test/morsecode-v6.4.rktd")
  (define-runtime-path gregor-file "./test/gregor-v6.4.rktd")

  (test-case "typed-racket-data?"
    (check-pred typed-racket-data? morsecode-file)
    (check-pred typed-racket-data? gregor-file)

    (check-false (typed-racket-data? "README.md"))
    (check-false (typed-racket-data? 'x))
    (check-false (typed-racket-data? 42)))

  (test-case "morsecode"
    (unless CI?
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
        1)
      (let* ([n-1? (lambda (cfg) (= (configuration-info->num-types cfg) (sub1 (performance-info->num-units M))))]
             [M% (filter-performance-info M n-1?)]
             [cfg* (for/list ((x (in-configurations M%))) x)])
        (check-equal? (length cfg*) (performance-info->num-units M)))))

  (test-case "gregor"
    (unless CI?
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
        1)))

  (test-case "typed-racket-id?"
    (check-pred typed-racket-id? "000")
    (check-pred typed-racket-id? "011")
    (check-false (typed-racket-id? "211"))
    (check-false (typed-racket-id? "a11"))
    (check-false (typed-racket-id? "0o0")))

  (test-case "typed-racket-id->num-units"
    (check-equal? (typed-racket-id->num-units "001")
                  3)
    (check-equal? (typed-racket-id->num-units "00101011")
                  8))

  (test-case "typed-racket-id->num-types"
    (check-equal? (typed-racket-id->num-types "001")
                  1)
    (check-equal? (typed-racket-id->num-types "1111")
                  4)
    (check-equal? (typed-racket-id->num-types "1")
                  1))

  (test-case "typed-configuration-info?"
    (check-true (typed-configuration-info? (make-typed-racket-configuration-info "1" '())))
    (check-true (typed-configuration-info? (make-typed-racket-configuration-info "1111" '())))
    (check-false (typed-configuration-info? (make-typed-racket-configuration-info "0" '())))
    (check-false (typed-configuration-info? (make-typed-racket-configuration-info "1101" '()))))

  (test-case "untyped-configuration-info?"
    (check-false (untyped-configuration-info? (make-typed-racket-configuration-info "010" '(2 2 2))))
    (check-true (untyped-configuration-info? (make-typed-racket-configuration-info "000" '(2 2 2)))))

  (test-case "make-typed-racket-sample-info"
    (define-values [mbta-si cfg**]
      (let* ([cfg0 (make-typed-racket-configuration-info "0111" '(1464 1532 1488 1520 1496 1472))]
             [cfg1 (make-typed-racket-configuration-info "0110" '(2876 2872 2964 2916 2900 2872))]
             [cfg2 (make-typed-racket-configuration-info "1100" '(1472 1544 1516 1540 1508 1540))]
             [cfg3 (make-typed-racket-configuration-info "1001" '(1540 1496 1528 1560 1552 1552))]
             [cfg** (list (list cfg0 cfg1) (list cfg2 cfg3))]
             [uc (make-typed-racket-configuration-info "0000" '(1464 1532 1488 1520 1496 1472))]
             [tc (make-typed-racket-configuration-info "1111" '(2816 2804 2940 2872 2932 2904))])
        (values (make-typed-racket-sample-info cfg** #:name 'mbta #:typed-configuration tc #:untyped-configuration uc)
                cfg**)))
    (check-true (sample-info? mbta-si))
    (check-equal?
      (for*/list ([pi (in-list (sample-info->performance-info* mbta-si))]
                  [cfg (in-configurations pi)])
        (configuration-info->id cfg))
      (for*/list ([cfg* (in-list cfg**)]
                  [cfg (in-list cfg*)])
        (configuration-info->id cfg)))
    (void))

  (test-case "->bm-name"
    (check-equal? (->bm-name 'A #f) 'A)
    (check-equal? (->bm-name #f "foo/fsm-v6.4.rktd") 'fsm)
    (check-equal? (->bm-name #f "hello-x.rktd") 'hello-x))
)
