#lang racket/base

;; For plotting data on one set of axis

(require racket/contract)
(define (non-empty-treeof x)
  (and/c (not/c null?) (treeof x)))
(provide
  axis/c
  (contract-out
    [cloud-plot
     (-> performance-info? pict?)]

    [overhead-plot
     (-> (non-empty-treeof performance-info?) pict?)]

    [discrete-overhead-plot
     (-> performance-info? (listof real?) pict?)]

    [samples-plot
     (-> (non-empty-treeof performance-info?) pict?)]

    #;[discrete-samples-plot
     (-> performance-info? (listof real?) pict?)]

    [exact-runtime-plot
     (-> (non-empty-treeof performance-info?) pict?)]

    [relative-overhead-cdf
      (-> performance-info? performance-info? pict?)]

    [rectangle-plot
     (-> performance-info? pict?)]

    [validate-samples-plot
     (-> performance-info? pict?)]

    [grid-plot
     (parametric->/c [X]
       (-> (-> X pict?)
           (listof X)
           pict?))]
))

(require
  gtp-plot/configuration-info
  gtp-plot/performance-info
  gtp-plot/sample-info
  gtp-plot/util
  pict
  plot/no-gui
  plot/utils
  racket/sequence
  (only-in math/number-theory
    binomial)
  (only-in math/base
    euler.0)
  (only-in math/statistics
    mean)
  (only-in racket/draw
    make-color)
  (only-in racket/format
    ~r)
  (only-in racket/list
    flatten
    range)
  (only-in racket/math
    exact-ceiling
    exact-floor)
  (only-in scribble-abbrevs
    add-commas))

;; =============================================================================
;; Parameters, constants

(define TICK-SIZE 4)
(define TITLE-FACE "Liberation Serif")

(define axis/c
  (flat-named-contract 'axis/c (or/c 'X 'Y)))

(define-syntax-rule (defparam id val type)
  (begin (define id (make-parameter val))
         (provide id)))

(defparam *BAR-WIDTH* 0.1 Real)
(defparam *CACHE-SIZE* (expt 2 16) Natural) ;; max num. configs to store in memory
(defparam *CONFIDENCE-LEVEL* 95 Percent)
(defparam *CONFIGURATION-X-JITTER* 0.4 Real)
(defparam *CLOUD-COLOR-WHEEL* '("skyblue" "slategray" "darkslateblue" "black") (listof plot-color/c))
(defparam *CLOUD-MIN-ALIGN* 'center anchor/c)
(defparam *CLOUD-MAX-ALIGN* 'center anchor/c)
(defparam *BRUSH-COLOR-CONVERTER* values (-> Natural Plot-Color))
(defparam *PEN-COLOR-CONVERTER* values (-> Natural Plot-Color))
(defparam *FONT-SIZE* 10 Natural)
(defparam *INTERVAL-ALPHA* 1 Nonnegative-Real)
(defparam *SAMPLE-INTERVAL-ALPHA* 0.4 Nonnegative-Real)
(defparam *MULTI-INTERVAL-ALPHA* 0.6 Nonnegative-Real)
(defparam *INTERVAL-STYLE* 'solid plot-brush-style/c)
(defparam *SAMPLE-INTERVAL-STYLE* 'solid plot-brush-style/c)
(defparam *MULTI-INTERVAL-STYLE* 'solid plot-brush-style/c)
(defparam *LEGEND-HSPACE* 20 Pict-Units)
(defparam *LEGEND-VSPACE* 10 Pict-Units)
(defparam *LEGEND?* #true Boolean)
(defparam *GRID-X* 600 Natural)
(defparam *GRID-Y* 1300 Natural)
(defparam *GRID-X-SKIP* 30 Natural)
(defparam *GRID-Y-SKIP* 6 Natural)
(defparam *GRID-NUM-COLUMNS* 3 Positive-Integer)
(defparam *MAJOR-AXIS* 'X axis/c)
(defparam *OVERHEAD-FONT-FACE* "bold" Font-Face)
(defparam *OVERHEAD-FREEZE-BODY* #f boolean?)
(defparam *OVERHEAD-LABEL?* #f boolean?)
(defparam *OVERHEAD-LINE-COLOR* 3 plot-color/c)
(defparam *OVERHEAD-LINE-STYLE* 'solid plot-pen-style/c)
(defparam *OVERHEAD-LINE-WIDTH* 1 Nonnegative-Real)
(defparam *OVERHEAD-MAX* 22 Natural)
(defparam *OVERHEAD-PLOT-HEIGHT* 300 Pict-Units)
(defparam *OVERHEAD-PLOT-WIDTH* 600 Pict-Units)
(defparam *OVERHEAD-SAMPLES* 20 Natural)
(defparam *OVERHEAD-SHOW-RATIO* #t (U Symbol Boolean))
(defparam *POINT-ALPHA* 0.4 Nonnegative-Real)
(defparam *POINT-COLOR* 2 plot-color/c)
(defparam *POINT-SIZE* 3 Positive-Index)
(defparam *POINT-SYMBOL* 'fullcircle point-sym/c)
(defparam *RATIO-DOT-COLOR* "firebrick" Color)
(defparam *RATIO-DOT-SIZE* 8 Natural)
(defparam *RATIO-DOT-SYM* 'plus point-sym/c)
(defparam *RECTANGLE-BORDER-COLOR* "black" Color)
(defparam *SAMPLE-COLOR* "chocolate" Color)
(defparam *STANDARD-D* #f (or/c #f positive?))
(defparam *TICK-GRID?* #true boolean?)
(defparam *TYPED/UNTYPED-RATIO-XTICK?* #f Boolean)

;; -----------------------------------------------------------------------------

(define (exact-runtime-plot pre-pi*)
  (log-gtp-plot-info "rendering exact-runtime-plot for ~a" pre-pi*)
  ;; TODO use standard-D
  (define multi? (pair? pre-pi*))
  (define pi* (if multi? (flatten pre-pi*) (list pre-pi*)))
  (define nt (or (check= (map performance-info->num-units pi*))
                 (raise-arguments-error 'exact-runtime-plot "incompatible performance-info structures" "pi*" pi*)))
  (define size/num-units (num-units->point-size nt))
  (define alpha/num-units (num-units->point-alpha nt))
  (define max-runtime (box 0))
  (define num-points (box 0))
  (define color0 (*POINT-COLOR*))
  (define elem*
    (list
      (make-vrule* nt)
      (for/list ([pi (in-list pi*)]
                 [color (in-naturals color0)])
        (parameterize ([*POINT-COLOR* color]
                       [*POINT-SIZE* size/num-units]
                       [*POINT-ALPHA* alpha/num-units])
          (for/fold ([acc '()])
                    ([cfg (in-configurations pi)])
            (define num-types (configuration-info->num-types cfg))
            (define t* (configuration-info->runtime* cfg))
            (cons
              (configuration-points
                (for/list ([t (in-list t*)]
                           [x (in-list (linear-seq (- num-types (*CONFIGURATION-X-JITTER*)) (+ num-types (*CONFIGURATION-X-JITTER*)) (length t*)))])
                  (set-box! max-runtime (max (unbox max-runtime) t))
                  (set-box! num-points (+ (unbox num-points) 1))
                  (list x t)))
              acc))))))
  (define y-max (exact-ceiling (unbox max-runtime)))
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-exact-runtime-xticks nt)]
                   [plot-y-ticks (make-exact-runtime-yticks y-max)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)])
      (plot-pict elem*
        #:x-min (- 0 0.5)
        #:x-max (+ nt 0.5)
        #:y-min 0
        #:y-max y-max
        #:x-label (and (*OVERHEAD-LABEL?*) "Num Type Ann.")
        #:y-label (and (*OVERHEAD-LABEL?*) "Time (ms)")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (define base-pict
    (exact-add-legend (performance-info->name (car pi*)) (unbox num-points) body))
  (begin0
    (if (and multi? (*LEGEND?*))
      (add-color-legend base-pict (make-color-legend pi* color0))
      base-pict)
    (log-gtp-plot-info "rendering complete")))

(define (overhead-plot pre-pi*)
  (log-gtp-plot-info "rendering overhead-plot for ~a" pre-pi*)
  ;; TODO use standard-D
  (define multi? (pair? pre-pi*))
  (define pi* (if multi? (flatten pre-pi*) (list pre-pi*)))
  (define color0 (*OVERHEAD-LINE-COLOR*))
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-overhead-x-ticks)]
                   [plot-x-transform log-transform]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)]
                   [*INTERVAL-ALPHA* (if multi? (*MULTI-INTERVAL-ALPHA*) (*INTERVAL-ALPHA*))]
                   [*INTERVAL-STYLE* (if multi? (*MULTI-INTERVAL-STYLE*) (*INTERVAL-STYLE*))])
      (plot-pict
        (list
          (if (length=2 pi*)
            (make-overlapping-intervals (car pi*) (cadr pi*))
            (for/list ([pi (in-list pi*)]
                       [i (in-naturals color0)])
              (parameterize ([*OVERHEAD-LINE-COLOR* i])
                (make-count-configurations-function pi))))
          #;(if #f ;(*OVERHEAD-SHOW-RATIO*)
            (make-dot pi typed/baseline-ratio)
            '())
          (tick-grid))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (define base-pict
    ;; TODO don't just use car
    (overhead-add-legend (car pi*) body))
  (begin0
    (if (and multi? (*LEGEND?*))
      (add-color-legend base-pict (make-color-legend pi* color0))
      base-pict)
    (log-gtp-plot-info "rendering finished")))

(define (discrete-overhead-plot pi D*)
  (log-gtp-plot-info "rendering discrete-overhead-plot for ~a at ~a" pi D*)
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-overhead-x-ticks)]
                   [plot-x-transform log-transform]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)])
      (plot-pict
        (list-if
          (let ([get-Y (make-simple-deliverable-counter pi)])
            (for/list ([D (in-list D*)])
              (make-overhead-bar D (get-Y D))))
          (and (*TICK-GRID?*) (tick-grid)))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (define base-pict
    (overhead-add-legend pi body))
  (begin0
    (if (*LEGEND?*)
      (add-color-legend base-pict (make-color-legend (list pi) (*OVERHEAD-LINE-COLOR*)))
      base-pict)
    (log-gtp-plot-info "rendering finished")))

(define (make-dot pi get-x)
  (define x-posn (get-x pi))
  (define y-posn
    (let ([num-configs (performance-info->num-configurations pi)]
          [num-deliv ((deliverable x-posn) pi)])
      (pct num-deliv num-configs)))
  (points (list (vector x-posn y-posn))
    #:color (*RATIO-DOT-COLOR*)
    #:sym (*RATIO-DOT-SYM*)
    #:size (*RATIO-DOT-SIZE*)))

(define (samples-plot pre-si*)
  (log-gtp-plot-info "rendering samples-plot for ~a" pre-si*)
  ;; TODO use standard-D
  (define multi? (pair? pre-si*))
  (define si* (if multi? (flatten pre-si*) (list pre-si*)))
  (define sample-size (sample-info->sample-size (car si*)))
  (define num-samples (sample-info->num-samples (car si*)))
  (define pi** (map sample-info->performance-info* si*))
  (define color0 (*SAMPLE-COLOR*))
  (define mean-overhead*
    (for/list ([pi* (in-list pi**)])
      (let ([dc* (map make-simple-deliverable-counter pi*)])
        (λ (r)
          (mean (for/list ([dc (in-list dc*)]) (dc r)))))))
  (define exact-ticks
    (if (*TYPED/UNTYPED-RATIO-XTICK?*)
      (error 'not-implemented-xtick) ;;(list (typed/baseline-ratio (car si*)))
      '()))
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-overhead-x-ticks exact-ticks)]
                   [plot-x-transform log-transform]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)]
                   [*INTERVAL-ALPHA* (*SAMPLE-INTERVAL-ALPHA*)]
                   [*INTERVAL-STYLE* (*SAMPLE-INTERVAL-STYLE*)])
      (plot-pict
        (list
          (if (length=2 pi**)
            (parameterize ([*OVERHEAD-LINE-COLOR* color0])
              (make-overlapping-sample-intervals (car pi**) (cadr pi**) (car mean-overhead*) (cadr mean-overhead*)))
            (for/list ([pi* (in-list pi**)]
                       [mean-overhead (in-list mean-overhead*)]
                       [i (in-colors color0)])
              (parameterize ([*OVERHEAD-LINE-COLOR* i])
                (list
                  (make-count-configurations-function mean-overhead)
                  (make-sample-function-interval pi*)))))
          (tick-grid))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (define base-pict
    (samples-add-legend (car si*) sample-size num-samples body))
  (begin0
    (if (and (*LEGEND?*) multi?)
      (add-color-legend base-pict (make-color-legend si* color0))
      base-pict)
    (log-gtp-plot-info "rendering finished")))

(define (discrete-samples-plot pi D*)
  (discrete-overhead-plot pi D*))

(define (validate-samples-plot si)
  (log-gtp-plot-info "rendering validate-samples-plot for ~a" si)
  ;; TODO use standard-D
  (define sample-size (sample-info->sample-size si))
  (define pi* (sample-info->performance-info* si))
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-overhead-x-ticks)]
                   [plot-x-transform log-transform]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)]
                   [*OVERHEAD-LINE-WIDTH* 0.5])
      (plot-pict
        (list
          (make-count-configurations-function si)
          (parameterize ([*OVERHEAD-LINE-COLOR* (*SAMPLE-COLOR*)])
            (make-sample-function-interval pi*))
          (tick-grid)
          (make-count-configurations-function si #:interval? #f))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (begin0
    (samples-add-legend (performance-info->name si) sample-size (length pi*) body)
    (log-gtp-plot-info "rendering finished")))

(define (relative-overhead-cdf pi-0 pi-1)
  (define bm
    (let ([name-0 (performance-info->name pi-0)]
          [name-1 (performance-info->name pi-1)])
      (if (eq? name-0 name-1)
        name-0
        (raise-arguments-error 'relative-overhead-cdf "expected performance-info for the same benchmark" "pi-0" pi-0 "pi-1" pi-1))))
  (define config->t0+t1
    #;(for/list ([cfg0 (in-configurations pi-0)]
               [cfg1 (in-configurations pi-1)])
      (define id0 (configuration-info->id cfg0))
      (define id1 (configuration-info->id cfg1))
      (unless (equal? id0 id1)
        (raise-arguments-error 'relative-overhead-cfg "mis-matched configurations" "id0" id0 "id1" id1))
      (list id0 (configuration-info->mean-runtime cfg0) (configuration-info->mean-runtime cfg1)))
    (let* ([H (make-hash)]
           [make-updater
             (lambda (i t)
               (lambda (m*)
                 (if (< (length m*) i)
                   (cons t m*)
                   (cons (mean (list t (car m*))) (cdr m*)))))]
           [_
            (for ((pi (in-list (list pi-1 pi-0)))
                  (i (in-naturals 1)))
              (for ((cfg (in-configurations pi)))
                (define id (configuration-info->id cfg))
                (define t (configuration-info->mean-runtime cfg))
                (hash-update! H id (make-updater i t) '())))])
      H))
  (void
    (for (((k v) (in-hash config->t0+t1)))
      (unless (= 2 (length v))
        (printf "ERROR wrong number of values ~a ~a ~a ~n"  bm k v))))
  (define dataset-num-configs (hash-count config->t0+t1))
  (define actual-num-configs
    (let ([nc-0 (performance-info->num-configurations pi-0)]
          [nc-1 (performance-info->num-configurations pi-1)])
      (if (= nc-0 nc-1)
        nc-0
        (raise-arguments-error 'relative-overhead-cfg "expected performance-info for the same benchmark, but num. configs is different" "pi-0" pi-0 "num-configs-0" nc-0 "pi-1" pi-1 "num-configs-0" nc-0))))
  (define overhead->num-configs
    (let* ([H (make-hash)]
           [relative-overhead (lambda (v) (/ (cadr v) (car v)))]
           [_
            (for (((k v) (in-hash config->t0+t1)))
              ;(define k (car kv))
              ;(define v (cdr kv))
              (unless (= 2 (length v))
                (raise-user-error 'fail "bad values ~a ~a" k v))
              (define ovr (relative-overhead v))
              (hash-update! H (truncate (* ovr 100)) add1 0))])
      H))
  (define (f-lo r) 0)
  (define (f-hi r)
    (define num-good-configs
      (for/sum (((k v) (in-hash overhead->num-configs))
                #:when (<= k (* 100 r)))
        v))
    (pct num-good-configs dataset-num-configs))
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-overhead-x-ticks)]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)])
      (plot-pict
        (list #;(discrete-histogram
                (for/list (((k v) (in-hash f-hi)))
                  (vector k (pct v dataset-num-configs))))
              (make-overhead-function-interval f-lo f-hi)
              (tick-grid))
        #:x-min 0
        #:x-max #;(apply max (hash-keys f-hi)) (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (define base-pict
    (add-legend (render-benchmark-name bm) body
                (render-count dataset-num-configs
                              (if (< dataset-num-configs actual-num-configs) "unique configs." "configurations"))))
  (begin0
    base-pict
    (log-gtp-plot-info "rendering finished")))


(define (cloud-plot pi)
  (log-gtp-plot-info "rendering cloud-plot for ~a" pi)
  (begin ;; TODO remove these checks
    (unless (eq? (*MAJOR-AXIS*) 'X)
      (raise-argument-error 'cloud-plot "(*MAJOR-AXIS*) = 'X" (*MAJOR-AXIS*)))
    (unless (eq? (*CLOUD-MIN-ALIGN*) 'center)
      (raise-argument-error 'cloud-plot "(*CLOUD-MIN-ALIGN*) = 'center" (*CLOUD-MIN-ALIGN*)))
    (unless (eq? (*CLOUD-MAX-ALIGN*) 'center)
      (raise-argument-error 'cloud-plot "(*CLOUD-MAX-ALIGN*) = 'center" (*CLOUD-MAX-ALIGN*)))
    (void))
  (define cloud-builder
    (make-cloud-builder pi))
  (define elem*
    (parameterize ([*POINT-ALPHA* 0.8])
      (for/list ([cfg (in-configurations pi)])
        (cloud-builder (configuration-info->num-types cfg) (configuration-info->runtime* cfg)))))
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks no-ticks]
                   [plot-y-ticks no-ticks]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size 0]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)])
      (plot-pict elem*
        ;; #:x-min 0
        ;; #:x-max (+ nt 1)
        ;; #:y-min 0
        ;; #:y-max ;; OBVIOUS
        #:x-label #f #;(and (*OVERHEAD-LABEL?*) "Num Type Ann.")
        #:y-label #f #;(and (*OVERHEAD-LABEL?*) "Time (ms)")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (begin0
    (cloud-add-legend (performance-info->name pi) body)
    (log-gtp-plot-info "rendering finished")))

(define (rectangle-plot pi)
  (log-gtp-plot-info "rendering rectangle-plot for ~a" pi)
  (define W (*OVERHEAD-PLOT-WIDTH*))
  (define H (*OVERHEAD-PLOT-HEIGHT*))
  (define RADIUS 5)
  (define LINE-WIDTH 1)
  (define COLOR "black")
  (define num-D
    ((deliverable (or (*STANDARD-D*) 10)) pi))
  (define nc
    (performance-info->num-configurations pi))
  (define outer
    (filled-rounded-rectangle
      W H RADIUS #:color "white" #:border-color COLOR #:border-width LINE-WIDTH))
  (define inner
    (filled-rounded-rectangle
      W (* (/ num-D nc) H) RADIUS #:color COLOR #:border-color COLOR #:border-width LINE-WIDTH))
  (define shim (rectangle W (* 2 LINE-WIDTH) #:border-color COLOR #:border-width LINE-WIDTH))
  (begin0
    (lb-superimpose outer (lt-superimpose inner shim))
    (log-gtp-plot-info "rendering finished")))

(define (grid-plot make-plot pi**)
  (define num-plots (length pi**))
  (log-gtp-plot-info "rendering grid-plot for ~a items" num-plots)
  (define GRID-X (*GRID-X*))
  (define GRID-Y (*GRID-Y*))
  (define GRID-X-SKIP (*GRID-X-SKIP*))
  (define GRID-Y-SKIP (*GRID-Y-SKIP*))
  (define GRID-NUM-COLUMNS (*GRID-NUM-COLUMNS*))
  (define plot-width
    (if GRID-X
      (/ (- GRID-X GRID-X-SKIP) GRID-NUM-COLUMNS)
      (*OVERHEAD-PLOT-WIDTH*)))
  (define plot-height
    (if GRID-Y
      (exact-floor (/ (+ GRID-Y GRID-Y-SKIP) num-plots))
      (*OVERHEAD-PLOT-HEIGHT*)))
  (define plot*
    (parameterize ([*OVERHEAD-PLOT-HEIGHT* plot-height]
                   [*OVERHEAD-PLOT-WIDTH* plot-width])
      (map make-plot pi**)))
  (define col*
    (for/list ([p* (in-list (columnize plot* GRID-NUM-COLUMNS))])
      (apply vl-append GRID-Y-SKIP p*)))
  (begin0
    (apply ht-append GRID-X-SKIP col*)
    (log-gtp-plot-info "rendering finished")))

;; -----------------------------------------------------------------------------

(define maybe-freeze
  (let ([SCALE-FACTOR 4])
    (λ (p)
      (if (*OVERHEAD-FREEZE-BODY*)
        (scale (freeze (scale p SCALE-FACTOR)) (/ 1 SCALE-FACTOR))
        p))))

(define (configuration-points p**)
  (points p**
    #:color ((*BRUSH-COLOR-CONVERTER*) (*POINT-COLOR*))
    #:alpha (*POINT-ALPHA*)
    #:sym (*POINT-SYMBOL*)
    #:size (*POINT-SIZE*)))

(define (make-vrule* count)
  (for/list ([i (in-range (+ 1 count))])
    (vrule (- i 0.5) #:width 0.2 #:color 0)))

(define (make-cloud-builder pi)
  (define runtime->color ; real? -> plot-color/c
    (let ()
      (define color-ref
        (let* ([colors (*CLOUD-COLOR-WHEEL*)]
               [N (length colors)])
          (λ (i) (list-ref colors (min i (- N 1))))))
      (define runtime->natural
        (let ([D (*STANDARD-D*)])
          (if D
            (let ([ok? (make-D-deliverable? D pi)])
              (λ (r) (if (ok? r) 0 1)))
            (let ([O (overhead pi)])
              (λ (r) (order-of-magnitude (O r)))))))
      (λ (r)
        (color-ref (runtime->natural r)))))
  (define (num-types->x-posn x)
    x)
  (define num-types->y-posn ; natural? -> real?
    (let* ([nt (performance-info->num-units pi)]
           [H (make-hash)]
           [_ (for/fold ([acc 0])
                        ([k (in-range (+ nt 1))])
                (define-values [max-configs-per-type rising?]
                  (let ([num-configs (binomial nt k)])
                    (if (< num-configs acc)
                      (values acc #f)
                      (values num-configs #t))))
                (define-values [init step]
                  (if rising?
                    (values 0 +1)
                    (values max-configs-per-type -1)))
                (hash-set! H k (cons init step))
                max-configs-per-type)])
      (λ (x-posn)
        (define-values [curr step]
          (let ([y (hash-ref H x-posn)])
            (values (car y) (cdr y))))
        (hash-set! H x-posn (cons (+ curr step) step))
        curr)))
  (λ (n-types t*)
    (define x (num-types->x-posn n-types))
    (define y (num-types->y-posn n-types))
    (define c (runtime->color (mean t*)))
    (rectangles
      (list (vector (ivl x (+ x 1)) (ivl y (+ y 1))))
      #:line-width 0
      #:line-color c
      #:color c
      #:alpha (*POINT-ALPHA*))))

(define (make-overlapping-intervals pi0 pi1)
  (define color0 (*OVERHEAD-LINE-COLOR*))
  (define color1 (+ color0 1))
  (define f0 (cached (->deliverable-counter pi0)))
  (define f1 (cached (->deliverable-counter pi1)))
  (define (lo0 n) 0)
  (define (lo1 n) 0)
  (list
    (parameterize ([*OVERHEAD-LINE-COLOR* color1])
      (make-overhead-function-interval lo1 f1))
    (parameterize ([*OVERHEAD-LINE-COLOR* color0])
      (make-overhead-function-interval lo0 f0))
    (parameterize ([*OVERHEAD-LINE-COLOR* color1])
      (make-count-configurations-function f1 #:interval? #f))))

(define (make-overlapping-sample-intervals pi*0 pi*1 m0 m1)
  (define color0 (*OVERHEAD-LINE-COLOR*))
  (define color1 (+ color0 1))
  (define f0 (cached (->deliverable-counter m0)))
  (define f1 (cached (->deliverable-counter m1)))
  (define (lo0 n) 0)
  (define (lo1 n) 0)
  (list
    (parameterize ([*OVERHEAD-LINE-COLOR* color1])
      (make-overhead-function-interval lo1 f1))
    (parameterize ([*OVERHEAD-LINE-COLOR* color0])
      (list (make-overhead-function-interval lo0 f0)
            (make-sample-function-interval pi*0)))
    (parameterize ([*OVERHEAD-LINE-COLOR* color1])
      (make-sample-function-interval pi*1))))

(define (make-count-configurations-function pi #:interval? [ivl #t])
  (define f (->deliverable-counter pi))
  (if ivl
    (make-overhead-function-interval (λ (r) 0) f)
    (function
      f
      0 (*OVERHEAD-MAX*)
      #:color ((*PEN-COLOR-CONVERTER*) (*OVERHEAD-LINE-COLOR*))
      #:width (*OVERHEAD-LINE-WIDTH*)
      #:samples (*OVERHEAD-SAMPLES*)
      #:style (*OVERHEAD-LINE-STYLE*))))

(define (make-overhead-function-interval lo hi)
  (function-interval lo hi
    0 (*OVERHEAD-MAX*)
    #:alpha (*INTERVAL-ALPHA*)
    #:color ((*BRUSH-COLOR-CONVERTER*) (*OVERHEAD-LINE-COLOR*))
    #:style (*INTERVAL-STYLE*)
    #:line1-style 'transparent
    #:line2-color ((*PEN-COLOR-CONVERTER*) (*OVERHEAD-LINE-COLOR*))
    #:line2-width (*OVERHEAD-LINE-WIDTH*)
    #:line2-style (*OVERHEAD-LINE-STYLE*)
    #:samples (*OVERHEAD-SAMPLES*)))

;; make-simple-deliverable-counter : (-> performance-info? (-> real? natural?))
;; Specification for `make-deliverable-counter`
(define (make-simple-deliverable-counter pi)
  (define nc (count-configurations pi (λ (_) #true)))
  (λ (D)
    (define c (count-configurations pi (make-D-deliverable? D pi)))
    (pct c nc)))

;; make-deliverable-counter : (-> performance-info? (-> real? natural?))
;; Same behavior as `make-deliverable-counter`, but `(make-deliverable-counter p)`
;;  has two side-effects for efficiency:
;; - if called with `i` such that `(= 100 ((make-deliverable-counter p) i))`
;;   then future calls to the function return 100 immediately
;; - if called with `i` such that `(= N ((make-deliverable-counter p) i))`
;;   and `(<= (* (- 100 N) (performance-info->num-configurations pi)) (*CACHE-SIZE*))`,
;;   then saves `N` and future calls only check whether the remaining configurations
;;   are now deliverable
;; Both side-effects assume monotonic calling contexts.
;; If `plot` made calls in a non-monotonic order, these would be WRONG!
(define (make-deliverable-counter pi)
  (define nc (performance-info->num-configurations pi))
  (define all-good? (box #f))
  (define cache (box #f)) ;; (U #f (Pairof Natural (Listof Real)))
  (λ (D)
    (if (unbox all-good?)
      100
      (let* ([good? (make-D-deliverable? D pi)]
             [num-good (if (unbox cache)
                         (+ (car (unbox cache))
                            (for/sum ([t (in-list (cdr (unbox cache)))] #:when (good? t)) 1))
                         (count-configurations pi good?))]
             [n (pct num-good nc)])
        (when (= n 100)
          (set-box! all-good? #t))
        (unless (or (unbox cache) (unbox all-good?))
          (define num-configs-left (- nc num-good))
          (when (<= num-configs-left (*CACHE-SIZE*))
            (set-box! cache (cons num-good (map configuration-info->mean-runtime (filter-configurations pi (λ (t) (not (good? t)))))))))
        n))))

(define (make-sample-function-interval pi*)
  (define (make-get-percents)
    (let ([ctr* (map make-simple-deliverable-counter pi*)])
      (λ (r)
        (for/list ([ctr (in-list ctr*)])
          (ctr r)))))
  (function-interval
    (λ (r) (lower-confidence ((make-get-percents) r)))
    (λ (r) (upper-confidence ((make-get-percents) r)))
    0 (*OVERHEAD-MAX*)
    #:alpha (*INTERVAL-ALPHA*)
    #:color (*OVERHEAD-LINE-COLOR*)
    #:line1-style 'transparent
    #:line2-color (*OVERHEAD-LINE-COLOR*)
    #:line2-width (*OVERHEAD-LINE-WIDTH*)
    #:line2-style 'solid
    #:samples (*OVERHEAD-SAMPLES*)
    #:label #f))

(define (make-overhead-bar x-mid y-hi)
  (define y-lo 0)
  (define-values [x-lo x-hi] (get-log-scaled-bounds x-mid (*BAR-WIDTH*)))
  (rectangles (list (vector (ivl x-lo x-hi) (ivl y-lo y-hi)))))

(define (get-log-scaled-bounds x-mid epsilon)
  (define l2 (log x-mid))
  (values (expt euler.0 (- l2 epsilon))
          (expt euler.0 (+ l2 epsilon))))

(define (lower-confidence n*)
  (- (mean n*) (error-bound n*)))

(define (upper-confidence n*)
  (+ (mean n*) (error-bound n*)))

(define (error-bound n*)
  (define cv
    (case (*CONFIDENCE-LEVEL*)
     [(95) 1.96]
     [(98) 2.326]
     [else (error 'error-bounds "Unknown confidence level '~a'" (*CONFIDENCE-LEVEL*))]))
  (confidence-offset n* #:cv cv))

(define (cached f)
  (define cache (make-hasheqv))
  (λ (n)
    (or (hash-ref cache n #f)
        (let ([fn (f n)])
          (hash-set! cache n fn)
          fn))))

;; ??? idk but using this 3 times
(define (->deliverable-counter x)
  (if (performance-info? x)
    (make-deliverable-counter x)
    x))

(define (make-overhead-x-ticks [extra-xticks '()])
  (define MAJOR-TICKS
    (sort (append extra-xticks (list 1 2 (*OVERHEAD-MAX*))) <))
  (define MINOR-TICKS
    (append (for/list ([i (in-range 12 20 2)]) (/ i 10))
            (for/list ([i (in-range 4 20 2)]) i)))
  (define m-ticks
    (ticks (real*->ticks-layout MAJOR-TICKS)
           (ticks-format/units "x")))
  (ticks-add m-ticks MINOR-TICKS #f))

(define (make-overhead-y-ticks)
  (define NUM-TICKS 3)
  (define UNITS "%")
  (ticks (λ (ax-min ax-max)
           (for/list ([y (in-list (linear-seq ax-min ax-max NUM-TICKS #:end? #t))])
             (pre-tick (exact-floor y) #t)))
         (ticks-format/units UNITS)))

(define (make-exact-runtime-xticks num-types)
  (define x*
    (if (<= num-types 6)
      (range (+ 1 num-types))
      (map exact-floor (linear-seq 0 num-types 5 #:start? #t #:end? #t))))
  (ticks (real*->ticks-layout x*)
         (λ (ax-min ax-max pre-ticks)
           (for/list ([pt (in-list pre-ticks)])
             (rnd+ (pre-tick-value pt))))))

(define (make-exact-runtime-yticks max-runtime)
  (define x* (list 0 (exact->inexact (/ max-runtime 2)) max-runtime))
  (ticks (real*->ticks-layout x*)
         (λ (ax-min ax-max pre-ticks)
           (for/list ([pt (in-list pre-ticks)])
             (define v (pre-tick-value pt))
             (cond
              [(= v max-runtime)
               (format "~as" v)]
              [else
               (~r v #:precision 1)])))))

(define ((real*->ticks-layout x*) ax-min ax-max)
  (for/list ([x (in-list x*)])
    (pre-tick x #t)))

(define ((ticks-format/units units) ax-min ax-max pre-ticks)
  (for/list ([pt (in-list pre-ticks)])
    (define v (pre-tick-value pt))
    (if (= v ax-max)
      (format "~a~a" (rnd+ v) units)
      (rnd+ v))))

(define (overhead-add-legend pi pict)
  (define name (render-benchmark-name (performance-info->name pi)))
  (define tp-ratio
    (if (*OVERHEAD-SHOW-RATIO*)
      (render-typed/baseline-ratio (typed/baseline-ratio pi))
      (blank 0 0)))
  (define nc (render-count (performance-info->num-configurations pi) "configurations"))
  (add-legend (hb-append (*LEGEND-HSPACE*) name tp-ratio)
              pict
              nc))

(define (exact-add-legend bm-name num-points pict)
  (define name (render-benchmark-name bm-name))
  (define np
    (if (*OVERHEAD-SHOW-RATIO*)
      (render-count num-points "points")
      (blank 0 0)))
  (add-legend name pict np))

(define (cloud-add-legend bm-name pict)
  (define name (render-benchmark-name bm-name))
  (add-legend name pict (blank 0 0)))

(define (samples-add-legend pi sample-size num-samples pict)
  (define-values [name tp-ratio]
    (cond
     [(performance-info? pi)
      (values (performance-info->name pi)
              (if (*OVERHEAD-SHOW-RATIO*)
                (render-typed/baseline-ratio (typed/baseline-ratio pi))
                (blank 0 0)))]
     [else
      (values pi (blank 0 0))]))
  (define s-info (render-count num-samples (format "samples of ~a configurations" (add-commas sample-size))))
  (add-legend (hb-append (*LEGEND-HSPACE*) (render-benchmark-name name) tp-ratio)
              pict s-info))

(define (add-legend top-left body top-right)
  (rt-superimpose
    (vl-append (*LEGEND-VSPACE*) top-left body)
    top-right))

(define (add-color-legend pict legend)
  (vr-append (*LEGEND-VSPACE*) pict legend))

(define (make-color-legend pi* first-color)
  (define HSPACE (*LEGEND-HSPACE*))
  (define swatch-width (- (*FONT-SIZE*) 2))
  (define swatch-height (- (*FONT-SIZE*) 2))
  (define name* (performance-info*->unique-name* pi*))
  (for/fold ([acc (blank 0 0)])
            ([name (in-list name*)]
             [c (in-naturals first-color)])
    (define lbl (text name '() (*FONT-SIZE*)))
    (define color (filled-rectangle swatch-width swatch-height #:color (apply make-color (->pen-color c))))
    (hc-append HSPACE
      acc
      (hc-append 2 color lbl))))

(define (title-text str [angle 0])
  (text str (cons 'bold TITLE-FACE) (*FONT-SIZE*) angle))

(define (render-benchmark-name sym)
  (title-text (symbol->string sym)))

(define (render-typed/baseline-ratio r)
  (define text
    (case (*OVERHEAD-SHOW-RATIO*)
     ['short (format "(~ax)" (rnd+ r))]
     [else (format "typed/baseline ratio: ~ax" (rnd+ r))]))
  (parameterize ([*FONT-SIZE* (sub1 (*FONT-SIZE*))])
    (title-text text)))

(define (rnd+ n)
  (if (exact-integer? n)
    (number->string n)
    (rnd n)))

(define (render-count n descr)
  (title-text (format "~a ~a" (add-commas n) descr)))

(define (check= v*)
  (let loop ([v* v*])
    (cond
     [(null? v*)
      (raise-argument-error 'check= "non-empty-list" v*)]
     [(null? (cdr v*))
      (car v*)]
     [(equal? (car v*) (cadr v*))
      (loop (cdr v*))]
     [else
      #f])))

(define (num-units->point-size nu)
  (cond
   [(< nu 10)
    6]
   [else
    (*POINT-SIZE*)]))

(define (num-units->point-alpha nu)
  (cond
   [(< nu 10)
    0.8]
   [else
    (*POINT-ALPHA*)]))

(define (length=2 x*)
  (and (not (null? x*))
       (not (null? (cdr x*)))
       (null? (cddr x*))))

(define (list-if . v*)
  (filter values v*))

(define (in-colors c)
  (cond
    [(exact-nonnegative-integer? c)
     (in-naturals c)]
    [(string? c)
     (in-sequences (in-value c) (in-naturals 4))]
    [else
     (raise-argument-error 'in-colors "color?" c)]))

;; =============================================================================

(module+ test
  (require
    rackunit
    rackunit-abbrevs
    gtp-plot/reticulated-info
    racket/runtime-path)

  (define-runtime-path espionage-data "./test/espionage/")
  (define espionage (make-reticulated-info espionage-data))

  (define-runtime-path fsm-data "./test/sample_fsm/")
  (define fsm (make-reticulated-info fsm-data))

  (define (sequence-take s n)
    (for/list ([x s]
               [_i (in-range n)])
      x))

  (test-case "deliverable-counter"
    (define (check-deliverable-counter/cache pi)
      (define f0 (make-simple-deliverable-counter pi))
      (define f1 (make-deliverable-counter pi))
      (define seq (linear-seq 1 (*OVERHEAD-MAX*) (*OVERHEAD-SAMPLES*)))
      (define-values [v*0 t0]
        (force/cpu-time (λ () (map f0 seq))))
      (define-values [v*1 t1]
        (force/cpu-time (λ () (map f1 seq))))
      (check-equal? v*0 v*1)
      (check < t1 t0)
      (void))

    (check-deliverable-counter/cache espionage)
  )

  (test-case "overhead-plot"
    (check-true (pict? (overhead-plot espionage))))

  (test-case "exact-runtime-plot"
    (check-true (pict? (exact-runtime-plot espionage))))

  (test-case "rectangle-plot"
    (check-true (pict? (rectangle-plot espionage))))

  (test-case "cloud-plot"
    (check-true (pict? (cloud-plot espionage))))

  (test-case "samples-plot"
    (check-true (pict? (samples-plot fsm))))

  (test-case "relative-overhead-cdf"
    (check-true (pict? (relative-overhead-cdf fsm fsm))))

  #;(test-case "validate-samples-plot"
    (check-true (pict? (validate-samples-plot espionage))))

  (test-case "length=2"
    (check-true (length=2 '(A B)))
    (check-false (length=2 '()))
    (check-false (length=2 '(A)))
    (check-false (length=2 '(A A A))))

  (test-case "in-colors"
    (check-equal? (sequence-take (in-colors 5) 3)
                  '(5 6 7))
    (check-equal? (sequence-take (in-colors "green") 3)
                  '("green" 4 5)))
)

