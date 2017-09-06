#lang racket/base

(require racket/contract)
(provide
  reticulated-sample-directory?
  (contract-out
   [reticulated-data?
    (-> any/c any)]
   [reticulated-info?
    (-> any/c any)]
   [make-reticulated-info
    (-> reticulated-data? performance-info?)]))

(require
  gtp-plot/configuration-info
  gtp-plot/sample-info
  gtp-plot/system
  gtp-plot/performance-info
  gtp-plot/util
  (only-in file/glob
    glob)
  (only-in file/gunzip
    gunzip)
  (only-in math/statistics
    mean)
  (only-in racket/file
    file->value)
  (only-in racket/math
    natural?)
  (only-in racket/string
    string-split
    string-replace))

;; =============================================================================

(define DATA-SUFFIX ".tab")
(define PYTHON-SUFFIX (string-append "python" DATA-SUFFIX))
(define TYPED-SUFFIX (string-append "retic-typed" DATA-SUFFIX))
(define UNTYPED-SUFFIX (string-append "retic-untyped" DATA-SUFFIX))
(define META-SUFFIX (string-append "meta.rktd"))

(define SAMPLE-GLOB (string-append "sample-*" DATA-SUFFIX))

(define METADATA-KEY* '(num-units))

(define UNDEFINED-SRC "AMBIGUOUS")

(struct reticulated-info performance-info ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<reticulated-info:~a>" (performance-info->name v)))])

(define (reticulated-data? path)
  (and (path-string? path)
       (directory-exists? path)
       (or (reticulated-exhaustive-directory? path)
           (reticulated-sample-directory? path))))

(define (make-reticulated-info path)
  (if (reticulated-sample-directory? path)
    (make-reticulated-sample-info path)
    (make-reticulated-exhaustive-info path)))

(define (make-reticulated-sample-info path)
  (define name (parse-reticulated-directory-name path))
  (define num-units
    (reticulated-meta->num-units (file->reticulated-meta (build-path path (format "~a-~a" name META-SUFFIX)))))
  (define baseline-runtime
    (mean (file->runtime* (build-path path (format "~a-~a" name PYTHON-SUFFIX)))))
  (define untyped-runtime
    (mean (file->runtime* (build-path path (format "~a-~a" name UNTYPED-SUFFIX)))))
  (define typed-runtime
    (mean (file->runtime* (build-path path (format "~a-~a" name TYPED-SUFFIX)))))
  (define ri
    (reticulated-info
      (string->symbol name)
      UNDEFINED-SRC
      num-units
      baseline-runtime
      untyped-runtime
      typed-runtime
      in-reticulated-configurations))
  (define samples
    (glob (build-path path SAMPLE-GLOB)))
  (make-sample-info ri samples))

(define (make-reticulated-exhaustive-info path)
  (define name (parse-reticulated-directory-name path))
  (define data-path
    (let* ([data-name (format "~a~a" name DATA-SUFFIX)]
           [data-path (build-path path data-name)]
           [zip-path (build-path path (format "~a~a.gz" name DATA-SUFFIX))])
      (if (file-exists? data-path)
        data-path
        (unpack-data zip-path data-name))))
  (define-values [num-configs untyped-runtime typed-runtime]
    (scan-karst-file data-path))
  (define num-units (log2 num-configs))
  (define baseline-runtime
    (mean (file->runtime* (build-path path (format "~a-~a" name PYTHON-SUFFIX)))))
  (reticulated-info
    (string->symbol name)
    data-path
    num-units
    baseline-runtime
    untyped-runtime
    typed-runtime
    in-reticulated-configurations))

(define (in-reticulated-configurations pi)
  (define src (performance-info->src pi))
  (when (equal? src UNDEFINED-SRC)
    (raise-argument-error 'in-configurations "reticulated-info? with exhaustive data" pi))
  (define go
    (let ([p (open-input-file src)]
          [line-number (box 0)])
      (λ ()
        (define ln (read-line p))
        (cond
         [(eof-object? ln)
          (close-input-port p)
          #f]
         [else
          (set-box! line-number (+ 1 (unbox line-number)))
          (parse-configuration ln (unbox line-number))]))))
  (in-producer go #f))

;; -----------------------------------------------------------------------------

(define (reticulated-exhaustive-directory? path)
  (define name (parse-reticulated-directory-name path))
  (define python-path
    (build-path path (format "~a-~a" name PYTHON-SUFFIX)))
  (define data-path
    (build-path path (format "~a~a" name DATA-SUFFIX)))
  (define data-path-zip
    (build-path path (format "~a~a.gz" name DATA-SUFFIX)))
  (and (file-exists? python-path)
       (or (file-exists? data-path)
           (file-exists? data-path-zip))))

(define (reticulated-sample-directory? path)
  (define name (parse-reticulated-directory-name path))
  (define python-path
    (build-path path (format "~a-~a" name PYTHON-SUFFIX)))
  (define untyped-path
    (build-path path (format "~a-~a" name UNTYPED-SUFFIX)))
  (define typed-path
    (build-path path (format "~a-~a" name TYPED-SUFFIX)))
  (define meta-path
    (build-path path (format "~a-~a" name META-SUFFIX)))
  (define samples
    (glob (build-path path SAMPLE-GLOB)))
  (and (file-exists? python-path)
       (file-exists? untyped-path)
       (file-exists? typed-path)
       (file-exists? meta-path)
       (not (null? samples))))

(define (parse-reticulated-directory-name path)
  (define-values [_base pre-name _mbd?] (split-path path))
  (unless (path-string? pre-name)
    (raise-argument-error 'parse-reticulated-directory-name "simple path-string?" path))
  (path-string->string pre-name))

(define (file->runtime* path)
  (with-input-from-file path
    (λ ()
      (for/fold ([acc '()])
                ([ln (in-lines)]
                 [i (in-naturals 1)])
        (define n (string->number ln))
        (if (nonnegative-real/c n)
          (cons n acc)
          (begin
            (log-gtp-plot-error "expected nonnegative real, got ~a in line ~a of file ~a" ln i path)
            acc))))))

(define (reticulated-meta? x)
  (and (hash? x)
       (for/and ([k (in-list METADATA-KEY*)])
         (hash-has-key? x k))))

(define/contract (file->reticulated-meta path)
  (-> path-string? reticulated-meta?)
  (file->value path))

(define (reticulated-meta->num-units rm)
  (or (hash-ref rm 'num-units #f)
      (raise-argument-error 'reticulated-meta->num-units "reticulated-meta?" rm)))

(define (unpack-data src-path dst-name)
  (define tmp (find-system-path 'temp-dir))
  (define src-md5 (md5sum src-path))
  (define unzip-dir
    (build-path tmp (format "gtp-plot-~a" src-md5)))
  (ensure-directory unzip-dir)
  (define unzip-file
    (build-path unzip-dir dst-name))
  (unless (file-exists? unzip-file)
    (gunzip src-path (λ (_f _a) unzip-file))
    (void))
  unzip-file)

;; scan-karst-file : Path-String -> (Values Natural (Listof Natural) Natural Natural)
;; Take a "first glance" pass over a data file
;; Return
;; - the number of configurations
;; - the max. number of types per module (in alphabetical order)
(define (scan-karst-file k)
  (define-values [num-configs num-configs++]
    (let ([nc (box 0)])
      (values nc
              (λ ()
                (set-box! num-configs (+ 1 (unbox num-configs)))))))
  (define-values [configs/module* update-configs/module*]
    (make-configuration-counter))
  (define-values [base-retic typed-retic update-base-retic update-typed-retic]
    (let ([ur (box #f)]
          [tr (box #f)])
      (values ur
              tr
              (λ (times-str)
                (set-box! ur
                  (mean (string->time* times-str))))
              (λ (times-str)
                (when (not (unbox tr))
                  (set-box! tr
                    (mean (string->time* times-str))))))))
  (with-input-from-file k
    (λ ()
      (for ([ln (in-lines)])
        (define-values [cfg-str times-str]
          (let ([ln-info (parse-line ln)])
            (values (car ln-info) (caddr ln-info))))
        (num-configs++)
        (define cfg/mod* (string->configuration cfg-str))
        (define prev-cfg* (unbox configs/module*))
        (update-configs/module* cfg/mod*)
        (when (typed-configuration? cfg/mod*)
          (update-typed-retic times-str))
        (unless (equal? prev-cfg* (unbox configs/module*))
          (update-base-retic times-str))
        (void))))
  (values (unbox num-configs)
          (unbox base-retic)
          (unbox typed-retic)))

(define (make-configuration-counter)
  (let ([cm (box #f)])
    (values cm
            (λ (cfg/mod*)
              (set-box! cm
                (if (and (unbox cm)
                         (for/and ([v-old (in-list (unbox cm))]
                                   [v-new (in-list cfg/mod*)])
                           (>= v-old v-new)))
                  (unbox cm)
                  cfg/mod*))))))

(define (typed-configuration? cfg/mod*)
  (andmap zero? cfg/mod*))

(define (parse-configuration ln line-number)
  (with-handlers ([exn:fail:read? (λ (e) (log-gtp-plot-error "PARSE ERROR on line ~a" line-number) (raise e))])
    (define str* (parse-line ln))
    (define cfg (string->configuration (car str*)))
    (define nt (string->num-types (cadr str*)))
    (define t* (string->time* (caddr str*)))
    (configuration-info cfg nt t*)))

(define/contract (parse-line str)
  (-> string? (list/c string? string? string?))
  (tab-split str))

(define/contract (string->num-types t-str)
  (-> string? natural?)
  (string->number t-str))

(define/contract (string->time* times-str)
  (-> string? (non-empty-listof (and/c real? (>=/c 0))))
  (let ([sp (open-input-string (string-replace times-str "," ""))])
    (begin0 (read sp) (close-input-port sp))))

(define (string->configuration cfg-str)
  (define (arg-error)
    (raise-argument-error 'string->configuration "string of hyphen-separated natural numbers" cfg-str))
  (define cfg* (string-split cfg-str "-"))
  (if (null? cfg*)
    (arg-error)
    (for/list ([str (in-list cfg*)])
      (define n (string->number str))
      (if (exact-nonnegative-integer? n)
        n
        (arg-error)))))

;; =============================================================================

(module+ test
  (require rackunit rackunit-abbrevs racket/runtime-path racket/list)

  (define-runtime-path fannkuch-path "./test/fannkuch")
  (define-runtime-path espionage-path "./test/espionage")
  (define-runtime-path sample_fsm-path "./test/sample_fsm")

  #;(test-case "fannkuch"
    (define F (make-reticulated-info fannkuch-path))

    (check-true reticulated-info? F)
    (check-equal?
      (performance-info->name F)
      'fannkuch)
    (check-equal?
      (performance-info->src F)
      (build-path fannkuch-path "fannkuch.tab"))
    (check-equal?
      (performance-info->num-units F)
      1)
    (check-equal?
      (performance-info->num-configurations F)
      2)
    (check-equal?
      (rnd (performance-info->baseline-runtime F))
      "12.23")
    (check-equal?
      (rnd (performance-info->untyped-runtime F))
      "13.94")
    (check-equal?
      (rnd (performance-info->typed-runtime F))
      "14.06")
    (check-equal?
      (for/list ([cfg (in-configurations F)]) (configuration-info->id cfg))
      '((0) (1)))
    (check-equal?
      (count-configurations F (λ (cfg) #true))
      2)
    (check-equal?
      ((deliverable 0.5) F)
      0)
    (check-equal?
      ((deliverable 1.4) F)
      2)
    (check-equal?
      (length (filter-configurations F (λ (r) #true)))
      2)
    (check-equal?
      (rnd (max-overhead F))
      "1.15")
    (check-equal?
      (rnd (mean-overhead F))
      "1.15")
    (check-equal?
      (rnd (min-overhead F))
      "1.14")
    (check-equal?
      (rnd (typed/baseline-ratio F))
      "1.15")
    (check-equal?
      (rnd (typed/untyped-ratio F))
      "1.01")
    (check-equal?
      (rnd (untyped/baseline-ratio F))
      "1.14"))

  #;(test-case "espionage"
    (define E (make-reticulated-info espionage-path))

    (check-equal?
      (performance-info->name E)
      'espionage)
    (check-not-equal?
      (performance-info->src E)
      (build-path espionage-path "espionage.tab"))
    (check-equal?
      (last (string-split (path->string (performance-info->src E)) "."))
      "tab")
    (check-equal?
      (performance-info->num-units E)
      12)
    (check-equal?
      (performance-info->num-configurations E)
      4096)
    (check-equal?
      (rnd (performance-info->baseline-runtime E))
      "1.60")
    (check-equal?
      (rnd (performance-info->untyped-runtime E))
      "4.60")
    (check-equal?
      (rnd (performance-info->typed-runtime E))
      "8.23")
    (let ([id* (for/list ([cfg (in-configurations E)]) (configuration-info->id cfg))])
      (check-equal? (length id*) 4096)
      (check-true (for/and ([id (in-list id*)]) (= 2 (length id)))))
    (check-equal?
      (count-configurations E (λ (cfg) (= 0 (car (configuration-info->id cfg)))))
      32)
    (check-equal?
      ((deliverable 4.0) E)
      2048)
    (check-equal?
      (rnd (max-overhead E))
      "5.39")
    (check-equal?
      (rnd (typed/baseline-ratio E))
      "5.14")
    (check-equal?
      (rnd (typed/untyped-ratio E))
      "1.79")
    (check-equal?
      (rnd (untyped/baseline-ratio E))
      "2.87"))

  #;(test-case "sample_fsm"
    (define S (make-reticulated-info sample_fsm-path))

    (check-pred sample-info? S)
    (check-equal?
      (performance-info->name S)
      'sample_fsm)
    (check-not-equal?
      (performance-info->src S)
      (build-path sample_fsm-path "sample_fsm.tab"))
    (check-equal?
      (performance-info->num-units S)
      19)
    (check-equal?
      (performance-info->num-configurations S)
      524288)
    (check-equal?
      (rnd (performance-info->baseline-runtime S))
      "0.45")
    (check-equal?
      (rnd (performance-info->untyped-runtime S))
      "1.26")
    (check-equal?
      (rnd (performance-info->typed-runtime S))
      "2.72")
    (check-exn exn:fail:contract?
      (λ () (in-configurations S)))
    (let ([pi* (sample-info->performance-info* S)])
      (check-equal?
        (map (deliverable 4) pi*)
        '(137 139 136 145 146 143 143 142 150 148)))
    (check-equal?
      (rnd (typed/baseline-ratio S))
      "6.07")
    (check-equal?
      (rnd (typed/untyped-ratio S))
      "2.16")
    (check-equal?
      (rnd (untyped/baseline-ratio S))
      "2.80"))

  (test-case "reticulated-exhaustive-directory?"
    (check-true (reticulated-exhaustive-directory? fannkuch-path))
    (check-false (reticulated-exhaustive-directory? sample_fsm-path)))

  (test-case "sample-directory"
    (check-false (reticulated-sample-directory? fannkuch-path))
    (check-true (reticulated-sample-directory? sample_fsm-path)))

  (test-case "parse-directory-name"
    (check-equal? (parse-reticulated-directory-name fannkuch-path) "fannkuch")
    (check-equal? (parse-reticulated-directory-name sample_fsm-path) "sample_fsm"))

  (test-case "file->runtime*"
    (let ([t* (file->runtime* (build-path espionage-path "espionage-python.tab"))])
      (check-equal? (length t*) 80)
      (check-equal? (rnd (mean t*)) "1.60")))

  (test-case "reticulated-meta?"
    (check-true (reticulated-meta? (make-immutable-hash '((num-units . 4096)))))
    (check-false (reticulated-meta? #f))
    (check-false (reticulated-meta? (make-immutable-hash '()))))

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
)

