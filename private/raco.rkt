#lang racket/base

(require
  racket/contract/base
  racket/cmdline
  gtp-plot/typed-racket-info
  gtp-plot/reticulated-info
  gtp-plot/plot
  gtp-plot/util
  pict
  (only-in racket/port with-input-from-string)
  (only-in racket/math natural?)
  (only-in plot/utils plot-color/c))

;; =============================================================================

(define GTP-PLOT 'gtp-plot)

(define CLOUD 'cloud)
(define EXACT 'exact)
(define OVERHEAD 'overhead)
(define RECTANGLE 'rectangle)
(define SAMPLE 'sample)
(define VALIDATE 'validate)

(define *plot-type* (make-parameter OVERHEAD))
(define *single-plot?* (make-parameter #f))
(define *output* (make-parameter "gtp-plot.png"))

(define (read-string str [ok? #f])
  (define v (with-input-from-string str read))
  (if (or (not ok?) (ok? v))
    v
    (raise-argument-error GTP-PLOT (format "~a" (contract-name ok?)) str)))

(define (->performance-info x)
  (cond
   [(typed-racket-data? x)
    (make-typed-racket-info x)]
   [(reticulated-data? x)
    (make-reticulated-info x)]
   [else
    (raise-user-error GTP-PLOT "failed to derive performance-info from input ~a" x)]))

;; =============================================================================

(module+ main
  (*FONT-SIZE* 14)
  (command-line
   #:program (symbol->string GTP-PLOT)
   #:once-any
   [("-c" "--cloud") "Make cloud plot" (*plot-type* CLOUD)]
   [("-e" "--exact") "Plot exact running times" (*plot-type* EXACT)]
   [("-o" "--overhead") "Make overhead plot" (*plot-type* OVERHEAD)]
   [("-s" "--sample") "Plot samples" (*plot-type* SAMPLE)]
   [("-r" "--rectangle") "Make a rectangle" (*plot-type* RECTANGLE)]
   [("-v" "--validate") "Validate samples" (*plot-type* VALIDATE)]
   #:once-each
   [("-A" "--axis") major-axis "Set major axis" (*MAJOR-AXIS* (read-string major-axis axis/c))]
   [("-W" "--width") plot-width "Set plot width" (*OVERHEAD-PLOT-WIDTH* (read-string plot-width positive?))]
   [("-H" "--height") plot-height "Set plot height" (*OVERHEAD-PLOT-HEIGHT* (read-string plot-height positive?))]
   [("-D") D "Overhead value for comparison" (*STANDARD-D* (read-string D positive?))]
   [("--single") "All plots on same axis (maybe nonsense)" (*single-plot?* #t)]
   [("--colors") colors "Color palette for cloud plots" (*CLOUD-COLOR-WHEEL* (read-string colors (listof plot-color/c)))]
   [("--output") out-file "Save plot to" (*output* out-file)]
   [("--font-size") fs "Change font size" (*FONT-SIZE* (read-string fs natural?))]
   ;;[("--cache") "Cache plots" (*CACHE?* #true)]
   #:args path-string*
   (cond
    [(null? path-string*)
     (printf "usage: raco ~a <data-file> ...~n" GTP-PLOT)]
    [else
     (define pi*
       (if (null? (cdr path-string*))
         (list (->performance-info (car path-string*)))
         (filter values
           (for/list ([n (in-list path-string*)])
             (with-handlers ([exn:fail?
                              (λ (e)
                                (define errmsg (format "Error processing '~a', run 'raco ~a ~a' to debug." n GTP-PLOT n))
                                (if (*single-plot?*)
                                  (raise-user-error 'gtp-plot errmsg)
                                  (begin (displayln errmsg) #f)))])
               (->performance-info n))))))
     (define render-one
       (case (*plot-type*)
        [(cloud) cloud-plot]
        [(overhead) overhead-plot]
        [(exact) exact-runtime-plot]
        [(rectangle) rectangle-plot]
        [(sample) samples-plot]
        [(validate) validate-samples-plot]
        [else (raise-user-error GTP-PLOT "unknown plot type '~a'" (*plot-type*))]))
     (define p*
       (if (*single-plot?*)
         (list (render-one pi*))
         (map render-one pi*)))
     (define OUT-FILE (*output*))
     (and (save-pict OUT-FILE (apply vl-append 50 p*))
          (printf "Saved output to '~a'~n" OUT-FILE))])))

;; =============================================================================
