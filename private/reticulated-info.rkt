#lang racket/base

(require racket/contract)
(provide
  reticulated-data?
  (contract-out
   [make-reticulated-info
    (-> reticulated-data? performance-info?)]
   [reticulated-info->sample-info
    (-> performance-info? sample-info?)]))

(require
  gtp-plot/configuration-info
  gtp-plot/sample-info
  gtp-plot/system
  gtp-plot/private/performance-info
  gtp-plot/private/util
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

(define SAMPLE-GLOB (string-append "sample-*~a" DATA-SUFFIX))

(define METADATA-KEY* '(num-units))

(struct reticulated-info performance-info ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc v port mode)
     (fprintf port "#<reticulated-info:~a>" (performance-info->name v)))])

(define (reticulated-data? path)
  (and (path-string?)
       (directory-exists? path)
       (or (reticulated-exhaustive-directory? path)
           (reticulated-sample-directory? path))))

(define (reticulated-info->sample-info ri)
  (define src (performance-info->src ri))
  (make-sample-info ri (glob (build-path src SAMPLE-GLOB))))

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
      "AMBIGUOUS"
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
  (define go
    (let ([p (open-input-file (performance-info->src pi))]
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

(define/contract (file->reticulated-meta path)
  (-> path-string? reticulated-meta?)
  (file->value path))

(define (reticulated-meta? x)
  (and (hash? x)
       (for/and ([k (in-list METADATA-KEY*)])
         (hash-has-key? x k))))

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
  (require rackunit rackunit-abbrevs racket/runtime-path)

  (define-runtime-path fannkuch-path "./test/fannkuch")
  (define-runtime-path espionage-path "./test/espionage")
  (define-runtime-path sample_fsm-path "./test/sample_fsm")


)

