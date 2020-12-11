#lang racket/base

(require racket/contract/base)
(provide
  (contract-out
   [typed-racket-data?
    (-> any/c any)]
   [typed-racket-info?
    (-> any/c any)]
   [typed-racket-id?
    (-> any/c any)]
   [typed-racket-id<=?
    (-> any/c any/c any)]
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
   [typed-racket-info%best-typed-path
     (-> typed-racket-info? natural? typed-racket-info?)]
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
  (only-in racket/math
    natural?)
  (only-in racket/path
    file-name-from-path)
  (only-in racket/string
    string-prefix?
    string-contains?
    non-empty-string?)
  (only-in racket/file
    file->value))

;; =============================================================================

(struct typed-racket-info performance-info ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<typed-racket-info:~a>" (performance-info->name v)))])

(define (typed-racket-data? v)
  (cond
    [(and (path-string? v) (file-exists? v))
     (looks-like-typed-racket-data v)]
    [(vector? v)
     (for/and ((elem (in-vector v)))
       (listof-nonnegative-real? elem))]
    [else
     #f]))

(define listof-nonnegative-real? (listof nonnegative-real/c))

(define NON-SPACE-NON-RPAREN #rx"[^ \t\n\r)]")

(define (looks-like-typed-racket-data path)
  (or
    (gtp-measure-typed-untyped-lang-file? path)
    (with-input-from-file path
      (lambda ()
        (define first-data-line
          (for/first ((ln (in-lines))
                      #:when (and (not (whitespace-string? ln))
                                  (not (simple-comment-string? ln))))
            (and (string-prefix? ln "#(") ln)))
        (and first-data-line
             (or (regexp-match? NON-SPACE-NON-RPAREN (substring first-data-line 2))
                 (regexp-match? NON-SPACE-NON-RPAREN (current-input-port))))))))

(define (gtp-measure-typed-untyped-lang-file? path)
  ;; 2020-08-06 : previously used lang-file package, but need gtp-measure installed
  (with-input-from-file path
    (lambda ()
      (for/first ((line (in-lines))
                  #:when (regexp-match? #rx"#lang" line))
        (regexp-match? #rx"gtp-measure/output/typed-untyped" line)))))

(define (make-typed-racket-info path #:name [name #f])
  (define vector-data? (vector? path))
  (define gtp-measure-file? (if vector-data? #f (gtp-measure-typed-untyped-lang-file? path)))
  (define bm-name
    (if vector-data?
      (or name 'vector-data)
      (->bm-name name path)))
  (define v
    (cond
      [vector-data?
        path]
      [gtp-measure-file?
       (parse-gtp-measure-data path)]
      [else
       (file->value path)]))
  (define nc (vector-length v))
  (define nu (log2 nc))
  (define rr (vector-ref v 0))
  (define tr (vector-ref v (- nc 1)))
  (if vector-data?
    (typed-racket-info
      bm-name
      #f
      nu
      nc
      rr
      rr
      tr
      (let ((cfg* (vector->configurations v nu)))
        (lambda (_pi) v)))
    (typed-racket-info
      bm-name
      (cond
        [gtp-measure-file? (make-tmp path v)]
        [else path])
      nu
      nc
      rr
      rr
      tr
      in-typed-racket-configurations)))

(define (make-tmp base-path v)
  (define t-dir (find-system-path 'temp-dir))
  (define base-name (path-string->string (file-name-from-path base-path)))
  (define t-name
    (let loop ([suffix #f])
      (define p (build-path t-dir (format "~a~a" base-name (or suffix ""))))
      (if (or (directory-exists? p) (file-exists? p))
        (loop (if suffix (+ suffix 1) 0))
        p)))
  (void
    (with-output-to-file t-name #:exists 'error
      (lambda ()
        (writeln v))))
  t-name)

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

(define (parse-typed-racket-data path)
  (if (gtp-measure-typed-untyped-lang-file? path)
    (parse-gtp-measure-data path)
    (file->value path)))

(define (parse-gtp-measure-data path)
  ;; TODO error-checking, flexible input, watch out for sampled data
  (with-input-from-file path
    (lambda ()
      (void (read-line)) ;; drop #lang
      (for/vector ([ln (in-lines)]
                   #:when (non-empty-string? ln))
        (map time-string->cpu-time (cadr (string->value ln)))))))

(define (in-typed-racket-configurations pi)
  (vector->configurations (file->value (performance-info->src pi))
                          (performance-info->num-units pi)))

(define (vector->configurations v num-units)
  (define go
    (let* ([num-configs (vector-length v)]
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

(define (typed-racket-info%best-typed-path pi n)
  (performance-info->typed-racket-info
    (performance-info%best-typed-path pi n typed-racket-configuration-dist)))

(define (performance-info->typed-racket-info pi)
  (typed-racket-info
    (performance-info->name pi)
    (performance-info->src pi)
    (performance-info->num-units pi)
    (performance-info->num-configurations pi)
    (performance-info->baseline-runtime* pi)
    (performance-info->untyped-runtime* pi)
    (performance-info->typed-runtime* pi)
    (performance-info-make-in-configurations pi)))

;; -----------------------------------------------------------------------------

(struct typed-racket-configuration-info configuration-info () #:transparent)

(define (make-typed-racket-configuration-info id t*)
  (typed-racket-configuration-info id (typed-racket-id->num-types id) t*))

(define (typed-racket-id? str)
  (and (string? str)
       (for/and ([c (in-string str)])
         (memq c '(#\0 #\1)))))

(define (typed-racket-id->int* str)
  (for/list ((c (in-string str)))
    (case c
      ((#\0) 0)
      ((#\1) 1)
      (else (raise-argument-error 'typed-racket-id->int* "typed-racket-id?" str)))))

(define (typed-racket-id<=? id0 id1)
  (define i*0 (typed-racket-id->int* id0))
  (define i*1 (typed-racket-id->int* id1))
  (unless (= (length i*0) (length i*1))
    (raise-arguments-error 'typed-racket-id<=? "ids must be for the same benchmark" "id0" id0 "id1" id1))
  (bit*<? i*0 i*1))

(define (typed-racket-configuration-dist c0 c1)
  (typed-racket-id-dist (configuration-info->id c0) (configuration-info->id c1)))

(define (typed-racket-id-dist id0 id1)
  (define i*0 (typed-racket-id->int* id0))
  (define i*1 (typed-racket-id->int* id1))
  (cond
    [(bit*<? i*0 i*1)
     (bit*-dist i*0 i*1)]
    [(bit*<? i*1 i*0)
     (- (bit*-dist i*1 i*0))]
    [else
     #f]))

(define (bit*<? i*0 i*1)
  (for/and ((i0 (in-list i*0))
            (i1 (in-list i*1)))
    (<= i0 i1)))

(define (bit*-dist i*0 i*1)
  (for/sum ((i0 (in-list i*0))
            (i1 (in-list i*1))
            #:when (< i0 i1))
    1))

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
  (define-runtime-path lnm-file "./test/lnm-gtp-measure.rktd")

  (test-case "typed-racket-data?"
    (check-pred typed-racket-data? morsecode-file)
    (check-pred typed-racket-data? gregor-file)
    (check-pred typed-racket-data? lnm-file)
    (check-pred typed-racket-data? '#((20) (50) (100) (200/3) (30) (100/3) (100/3) (10)))

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

  (test-case "lnm"
    (unless CI?
      (define L (make-typed-racket-info lnm-file))

      (check-equal?
        (performance-info->name L)
        'lnm-gtp-measure)
      (check-not-equal?
        (performance-info->src L)
        lnm-file)
      (check-pred
        (let ((expected (path-string->string (file-name-from-path lnm-file) )))
          (lambda (fn) (string-contains? (path-string->string fn) expected)))
        (performance-info->src L))
      (check-equal?
        (performance-info->num-units L)
        6)
      (check-equal?
        (performance-info->num-configurations L)
        (expt 2 6))
      (check-equal?
        (performance-info->baseline-runtime L)
        (performance-info->untyped-runtime L))
      (check-equal?
        (performance-info->baseline-runtime L)
        525)
      (check-equal?
        (performance-info->typed-runtime L)
        5467/2)
      (check-equal?
        (count-configurations L (λ (cfg) #true))
        64)
      (check-equal?
        (count-configurations L (let ([done (box #f)])
                                   (λ (cfg)
                                     (if (unbox done)
                                       #f
                                       (set-box! done #true)))))
        1)
      (check-equal?
        ((deliverable 1.0) L)
        6)
      (check-equal?
        ((deliverable 1.8) L)
        16)
      (check-equal?
        ((deliverable 5) L)
        48)
      (check-equal?
        (length (filter-configurations L (λ (r) #true)))
        64)
      (check-equal?
        (overhead L 1000)
        40/21)
      (check-equal?
        (overhead L 9000)
        120/7)
      (check-equal?
        (max-overhead L)
        781/150)
      (check-equal?
        (mean-overhead L)
        829393/268800)
      (check-equal?
        (min-overhead L)
        4141/4200)
      (check-equal?
        (typed/baseline-ratio L)
        (typed/untyped-ratio L))
      (check-equal?
        (typed/untyped-ratio L)
        781/150)
      (check-equal?
        (untyped/baseline-ratio L)
        1)))

  (test-case "typed-racket-id?"
    (check-pred typed-racket-id? "000")
    (check-pred typed-racket-id? "011")
    (check-false (typed-racket-id? "211"))
    (check-false (typed-racket-id? "a11"))
    (check-false (typed-racket-id? "0o0")))

  (test-case "typed-racket-id<=?"
    (check-true (typed-racket-id<=? "0" "0"))
    (check-true (typed-racket-id<=? "0" "1"))
    (check-true (typed-racket-id<=? "00" "11"))
    (check-true (typed-racket-id<=? "0000" "1111"))
    (check-false (typed-racket-id<=? "1111" "0000"))
    (check-true (typed-racket-id<=? "0110" "1110"))
    (check-false (typed-racket-id<=? "0110" "1011")))

  (test-case "typed-racket-id-dist"
    (check-equal? (typed-racket-id-dist "0" "0") 0)
    (check-equal? (typed-racket-id-dist "0" "1") 1)
    (check-equal? (typed-racket-id-dist "00" "11") 2)
    (check-equal? (typed-racket-id-dist "0000" "1111") 4)
    (check-equal? (typed-racket-id-dist "1111" "0000") -4)
    (check-equal? (typed-racket-id-dist "0110" "1110") 1)
    (check-equal? (typed-racket-id-dist "0110" "1011") #f))

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
