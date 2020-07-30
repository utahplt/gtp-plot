#lang racket/base

(require racket/contract)
(provide
  reticulated-sample-directory?
  (contract-out
   [reticulated-data?
    (-> any/c any)]
   [reticulated-info?
    (-> any/c any)]
   [reticulated-id?
     (-> any/c any)]
   [reticulated-id<=?
     (-> any/c any/c any)]
   [make-reticulated-info
    (->* [reticulated-data?] [(or/c 'exhaustive 'approximate #f)] performance-info?)]
   [reticulated-info%best-typed-path
     (-> reticulated-info? natural? reticulated-info?)]))

(require
  gtp-plot/configuration-info
  gtp-plot/sample-info
  gtp-plot/system
  gtp-plot/performance-info
  gtp-plot/util
  file/glob
  (only-in file/gunzip
    gunzip)
  (only-in racket/file
    file->value)
  (only-in racket/math
    natural?)
  (only-in racket/sequence
    sequence->list)
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

(define (mk-exists?/log f str)
  (lambda (fn)
    (if (f fn)
      #t
      (begin
        (log-gtp-plot-warning "~a '~a' does not exist" str fn)
        #f))))

(define file-exists?/log (mk-exists?/log file-exists? "file"))
(define directory-exists?/log (mk-exists?/log directory-exists? "directory"))

(struct reticulated-info performance-info ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<reticulated-info:~a>" (performance-info->name v)))])

(define (reticulated-data? path)
  (and (path-string? path)
       (directory-exists?/log path)
       (or (reticulated-exhaustive-directory? path)
           (reticulated-sample-directory? path))))

(define (make-reticulated-info path [kind #f])
  (define mk
    (case kind
      ((exhaustive)
       make-reticulated-exhaustive-info)
      ((approximate)
       make-reticulated-sample-info)
      (else
       (if (reticulated-sample-directory? path)
         make-reticulated-sample-info
         make-reticulated-exhaustive-info))))
  (mk path))

;; (-> (or/c reticulated-sample-directory? (and/c reticulated-exhaustive-directory? has-samples?))
;;     sample-info?)
(define (make-reticulated-sample-info path)
  (define name (parse-reticulated-directory-name path))
  (define-values [num-units untyped-runtime* typed-runtime*]
    (let ((data-path (directory->data-path path name)))
      (if data-path
        (let-values (((num-configs untyped-runtime* typed-runtime*) (scan-karst-file data-path)))
          (values (log2 num-configs) untyped-runtime* typed-runtime*))
        (values
          (reticulated-meta->num-units (file->reticulated-meta (build-path path (format "~a-~a" name META-SUFFIX))))
          (file->runtime* (build-path path (format "~a-~a" name UNTYPED-SUFFIX)))
          (file->runtime* (build-path path (format "~a-~a" name TYPED-SUFFIX)))))))
  (define baseline-runtime*
    (file->runtime* (build-path path (format "~a-~a" name PYTHON-SUFFIX))))
  (define ri
    (reticulated-info
      (string->symbol name)
      UNDEFINED-SRC
      num-units
      (expt num-units 2)
      baseline-runtime*
      untyped-runtime*
      typed-runtime*
      in-reticulated-configurations))
  (define cfg**
    (for/list ([sample (in-glob (build-path path SAMPLE-GLOB))])
      (sequence->list (in-reticulated-configurations/src sample))))
  (make-sample-info ri cfg**))

(define (make-reticulated-exhaustive-info path)
  (define name (parse-reticulated-directory-name path))
  (define data-path (directory->data-path path name))
  (unless data-path
    (raise-argument-error 'make-reticulated-exhaustive-info "reticulated-exhaustive-directory?" path))
  (define-values [num-configs untyped-runtime typed-runtime]
    (scan-karst-file data-path))
  (define num-units (log2 num-configs))
  (define baseline-runtime
    (file->runtime* (build-path path (format "~a-~a" name PYTHON-SUFFIX))))
  (reticulated-info
    (string->symbol name)
    data-path
    num-units
    num-configs
    baseline-runtime
    untyped-runtime
    typed-runtime
    in-reticulated-configurations))

(define (in-reticulated-configurations pi)
  (define src (performance-info->src pi))
  (when (equal? src UNDEFINED-SRC)
    (raise-argument-error 'in-configurations "reticulated-info? with exhaustive data" pi))
  (in-reticulated-configurations/src src))

(define (in-reticulated-configurations/src src)
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

(define (reticulated-info%best-typed-path pi n)
  (performance-info->reticulated-info
    (performance-info%best-typed-path pi n reticulated-configuration-dist)))

(define (performance-info->reticulated-info pi)
  (reticulated-info
    (performance-info->name pi)
    (performance-info->src pi)
    (performance-info->num-units pi)
    (performance-info->num-configurations pi)
    (performance-info->baseline-runtime* pi)
    (performance-info->untyped-runtime* pi)
    (performance-info->typed-runtime* pi)
    (performance-info-make-in-configurations pi)))

;; -----------------------------------------------------------------------------

(define (reticulated-exhaustive-directory? path)
  (define name (parse-reticulated-directory-name path))
  (define python-path
    (build-path path (format "~a-~a" name PYTHON-SUFFIX)))
  (define data-path
    (build-path path (format "~a~a" name DATA-SUFFIX)))
  (define data-path-zip
    (build-path path (format "~a~a.gz" name DATA-SUFFIX)))
  (and (file-exists?/log python-path)
       (or (file-exists?/log data-path)
           (file-exists?/log data-path-zip))))

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
  (and (file-exists?/log python-path)
       (file-exists?/log untyped-path)
       (file-exists?/log typed-path)
       (file-exists?/log meta-path)
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

(define (directory->data-path path name)
  (let* ([data-name (format "~a~a" name DATA-SUFFIX)]
         [data-path (build-path path data-name)]
         [zip-path (build-path path (format "~a~a.gz" name DATA-SUFFIX))])
    (cond
      [(file-exists? data-path)
       data-path]
      [(file-exists? zip-path)
       (unpack-data zip-path data-name)]
      [else
        #f])))

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
                  (string->time* times-str)))
              (λ (times-str)
                (when (not (unbox tr))
                  (set-box! tr
                    (string->time* times-str)))))))
  (with-input-from-file k
    (λ ()
      (for ([ln (in-lines)])
        (define-values [cfg-str times-str]
          (let ([ln-info (parse-line ln)])
            (values (car ln-info) (caddr ln-info))))
        (num-configs++)
        (define cfg/mod* (string->reticulated-id cfg-str))
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
    (define cfg (string->reticulated-id (car str*)))
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

(define (string->reticulated-id cfg-str [who 'string->reticulated-id])
  (define (arg-error)
    (raise-argument-error who "string of hyphen-separated natural numbers" cfg-str))
  (define cfg* (string-split cfg-str "-"))
  (if (null? cfg*)
    (arg-error)
    (for/list ([str (in-list cfg*)])
      (define n (string->number str))
      (if (exact-nonnegative-integer? n)
        n
        (arg-error)))))

(define (normalize-reticulated-id cfg [who 'normalize-reticulated-id])
  (cond
    [(reticulated-id-list? cfg)
     cfg]
    [(string? cfg)
     (string->reticulated-id cfg who)]
    [else
      (raise-argument-error who "(or/c (listof natural?) string?)" cfg)]))

(define reticulated-id-regexp
  #rx"^[0-9]+(-[0-9]+)*$")

(define (reticulated-id-list? x)
  (and (list? x) (andmap natural? x)))

(define (reticulated-id-string? str)
  (regexp-match? reticulated-id-regexp str))

(define reticulated-id?
  (or/c reticulated-id-list?
        (and/c string? reticulated-id-string?)))

(define (reticulated-id<=? pre-id0 pre-id1)
  (define id0 (normalize-reticulated-id pre-id0 'reticulated-id<=?))
  (define id1 (normalize-reticulated-id pre-id1 'reticulated-id<=?))
  (unless (= (length id0) (length id1))
    (raise-arguments-error 'reticulated-id<=? "ids must be for the same benchmark" "id0" id0 "id1" id1))
  (reticulated-id-list<=? id0 id1))

(define (reticulated-configuration-dist c0 c1)
  (reticulated-id-dist (configuration-info->id c0) (configuration-info->id c1)))

(define (reticulated-id-dist pre-id0 pre-id1)
  (define id0 (normalize-reticulated-id pre-id0 'reticulated-id<=?))
  (define id1 (normalize-reticulated-id pre-id1 'reticulated-id<=?))
  (cond
    [(reticulated-id-list<=? id0 id1)
     (reticulated-id-list-dist id0 id1)]
    [(reticulated-id-list<=? id1 id0)
     (- (reticulated-id-list-dist id1 id0))]
    [else
      #false]))

(define (reticulated-id-list<=? id0 id1)
  (for/and ((n0 (in-list id0))
            (n1 (in-list id1)))
    #;(ditto = n1 (bitwise-and n0 n1))
    (= n0 (bitwise-ior n0 n1))))

(define (reticulated-id-list-dist id0 id1)
  (for/sum ((n0 (in-list id0))
            (n1 (in-list id1)))
    (integer-count-bits (- n0 n1))))

;; =============================================================================

(module+ test
  (require
    rackunit rackunit-abbrevs racket/runtime-path racket/list
    (only-in math/statistics
      mean))

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

  (test-case "reticulated-id?"
    (check-true (reticulated-id? "0"))
    (check-true (reticulated-id? "0-0-0"))
    (check-true (reticulated-id? "2001-14-3"))
    (check-true (reticulated-id? '(2001 14 3)))
    (check-false (reticulated-id? "0-"))
    (check-false (reticulated-id? "A-A"))
    (check-false (reticulated-id? "0--5-0"))
    (check-false (reticulated-id? '(-5 3))))

  (test-case "reticulated-id<=?"
    (check-true (reticulated-id<=? "0000" "0000"))
    (check-false (reticulated-id<=? "0000" "1111"))
    (check-true (reticulated-id<=? "1111" "0000"))
    (check-true (reticulated-id<=? "7-6" "5-6"))
    (check-false (reticulated-id<=? "6-6" "5-6")))

  (test-case "reticulated-id-dist"
    (check-equal? (reticulated-id-dist "9-9-9" "9-9-9") 0)
    (check-equal? (reticulated-id-dist "1111" "0000") 6) ;; "1111" ~> 10001010111
    (check-equal? (reticulated-id-dist "7-6" "5-6") 1)
    (check-equal? (reticulated-id-dist "5-6" "7-6") -1)
    (check-equal? (reticulated-id-dist "6-6" "5-6") #f))
)

