#lang racket/base

;; For plotting data on one set of axis

(require racket/contract)
(provide
  axis/c
  (contract-out
    [cloud-plot
     (-> performance-info? pict?)]

    [overhead-plot
     (-> (treeof performance-info?) pict?)]

    [samples-plot
     (-> performance-info? pict?)]

    [exact-runtime-plot
     (-> performance-info? pict?)]

    [rectangle-plot
     (-> performance-info? pict?)]

    [validate-samples-plot
     (-> performance-info? pict?)]
))

(require
  gtp-plot/configuration-info
  gtp-plot/performance-info
  gtp-plot/sample-info
  gtp-plot/util
  pict
  plot/no-gui
  plot/utils
  (only-in math/number-theory
    binomial)
  (only-in math/statistics
    mean)
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

(defparam *CACHE-SIZE* (expt 2 16) Natural) ;; max num. configs to store in memory
(defparam *CONFIDENCE-LEVEL* 95 Percent)
(defparam *CONFIGURATION-X-JITTER* 0.4 Real)
(defparam *CLOUD-COLOR-WHEEL* '("skyblue" "slategray" "darkslateblue" "black") (listof plot-color/c))
(defparam *CLOUD-MIN-ALIGN* 'center anchor/c)
(defparam *CLOUD-MAX-ALIGN* 'center anchor/c)
(defparam *FONT-SIZE* 10 Natural)
(defparam *INTERVAL-ALPHA* 1 Nonnegative-Real)
(defparam *LEGEND-HSPACE* 20 Pict-Units)
(defparam *LEGEND-VSPACE* 10 Pict-Units)
(defparam *MAJOR-AXIS* 'X axis/c)
(defparam *OVERHEAD-FONT-FACE* "bold" Font-Face)
(defparam *OVERHEAD-FONT-SCALE* 0.03 Nonnegative-Real)
(defparam *OVERHEAD-FREEZE-BODY* #f boolean?)
(defparam *OVERHEAD-LABEL?* #f boolean?)
(defparam *OVERHEAD-LINE-COLOR* 3 plot-color/c)
(defparam *OVERHEAD-LINE-STYLE* 'solid plot-pen-style/c)
(defparam *OVERHEAD-LINE-WIDTH* 1 Nonnegative-Real)
(defparam *OVERHEAD-MAX* 10 Natural)
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
(defparam *TYPED/UNTYPED-RATIO-XTICK?* #f Boolean)

;; -----------------------------------------------------------------------------

(define (exact-runtime-plot pi)
  ;; TODO use standard-D
  (define nt (performance-info->num-units pi))
  (define max-runtime (box 0))
  (define num-points (box 0))
  (define elem*
    (parameterize ([*POINT-COLOR* 2]
                   [*POINT-SYMBOL* 'fullcircle])
      (list
        (make-vrule* nt)
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
            acc)))))
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
  (exact-add-legend (performance-info->name pi) (unbox num-points) body))

(define (overhead-plot pre-pi*)
  (define multi? (pair? pre-pi*))
  (define pi* (if multi? (flatten pre-pi*) (list pre-pi*)))
  ;; TODO use standard-D
  (define body (maybe-freeze
    (parameterize ([plot-x-ticks (make-overhead-x-ticks)]
                   [plot-x-transform log-transform]
                   [plot-y-ticks (make-overhead-y-ticks)]
                   [plot-x-far-ticks no-ticks]
                   [plot-y-far-ticks no-ticks]
                   [plot-tick-size TICK-SIZE]
                   [plot-font-face (*OVERHEAD-FONT-FACE*)]
                   [plot-font-size (*FONT-SIZE*)]
                   [*INTERVAL-ALPHA* (if multi? 0.6 (*INTERVAL-ALPHA*))])
      (plot-pict
        (list
          (for/list ([pi (in-list pi*)]
                     [i (in-naturals 3)])
            (parameterize ([*OVERHEAD-LINE-COLOR* i])
              (make-count-configurations-function pi)))
          #;(if #f ;(*OVERHEAD-SHOW-RATIO*)
            (make-dot pi typed/baseline-ratio)
            '())
          (tick-grid))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead (vs. retic-untyped)")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  ;; TODO don't just use car
  (overhead-add-legend (car pi*) body))

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

(define (samples-plot si)
  ;; TODO use standard-D
  (define sample-size (sample-info->sample-size si))
  (define pi* (sample-info->performance-info* si))
  (define mean-overhead
    (let ([dc* (map make-simple-deliverable-counter pi*)])
      (λ (r)
        (mean (for/list ([dc (in-list dc*)]) (dc r))))))
  (define exact-ticks
    (if (*TYPED/UNTYPED-RATIO-XTICK?*)
      (list (typed/baseline-ratio si))
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
                   [*OVERHEAD-LINE-COLOR* (*SAMPLE-COLOR*)])
      (plot-pict
        (list
          (parameterize ([*INTERVAL-ALPHA* 0.4])
            (make-count-configurations-function mean-overhead))
          (make-sample-function-interval pi*)
          (tick-grid))
        #:x-min 1
        #:x-max (*OVERHEAD-MAX*)
        #:y-min 0
        #:y-max 100
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead (vs. retic-untyped)")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (samples-add-legend si sample-size (length pi*) body))

(define (validate-samples-plot si)
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
        #:x-label (and (*OVERHEAD-LABEL?*) "Overhead (vs. retic-untyped)")
        #:y-label (and (*OVERHEAD-LABEL?*) "% Configs.")
        #:width (*OVERHEAD-PLOT-WIDTH*)
        #:height (*OVERHEAD-PLOT-HEIGHT*)))))
  (samples-add-legend (performance-info->name si) sample-size (length pi*) body))

(define (cloud-plot pi)
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
  (cloud-add-legend (performance-info->name pi) body))

(define (rectangle-plot pi)
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
  (lb-superimpose outer (lt-superimpose inner shim)))

;; -----------------------------------------------------------------------------

(define maybe-freeze
  (let ([SCALE-FACTOR 4])
    (λ (p)
      (if (*OVERHEAD-FREEZE-BODY*)
        (scale (freeze (scale p SCALE-FACTOR)) (/ 1 SCALE-FACTOR))
        p))))

(define (configuration-points p**)
  (points p**
    #:color (*POINT-COLOR*)
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


(define (make-count-configurations-function pi #:interval? [ivl #t])
  (define f (if (performance-info? pi) (make-deliverable-counter pi) pi))
  (if ivl
    (function-interval
      (λ (r) 0)
      f
      0 (*OVERHEAD-MAX*)
      #:alpha (*INTERVAL-ALPHA*)
      #:color (*OVERHEAD-LINE-COLOR*)
      #:line1-color (*OVERHEAD-LINE-COLOR*)
      #:line2-color (*OVERHEAD-LINE-COLOR*)
      #:line1-width (*OVERHEAD-LINE-WIDTH*)
      #:line2-width (*OVERHEAD-LINE-WIDTH*)
      #:samples (*OVERHEAD-SAMPLES*)
      #:style (*OVERHEAD-LINE-STYLE*))
    (function
      f
      0 (*OVERHEAD-MAX*)
      #:color (*OVERHEAD-LINE-COLOR*)
      #:width (*OVERHEAD-LINE-WIDTH*)
      #:samples (*OVERHEAD-SAMPLES*)
      #:style (*OVERHEAD-LINE-STYLE*))))

;; make-simple-deliverable-counter : (-> performance-info? (-> real? natural?))
;; Specification for `make-deliverable-counter`
(define (make-simple-deliverable-counter pi)
  (define nc (performance-info->num-configurations pi))
  (λ (D)
    (pct (count-configurations pi (make-D-deliverable? D pi)) nc)))

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
    (let ([ctr* (map make-deliverable-counter pi*)])
      (λ (r)
        (for/list ([ctr (in-list ctr*)])
          (ctr r)))))
  (function-interval
    (λ (r) (lower-confidence ((make-get-percents) r)))
    (λ (r) (upper-confidence ((make-get-percents) r)))
    0 (*OVERHEAD-MAX*)
    #:alpha (*INTERVAL-ALPHA*)
    #:color (*OVERHEAD-LINE-COLOR*)
    #:line1-color (*OVERHEAD-LINE-COLOR*)
    #:line1-width (*OVERHEAD-LINE-WIDTH*)
    #:line2-color (*OVERHEAD-LINE-COLOR*)
    #:line2-width (*OVERHEAD-LINE-WIDTH*)
    #:samples (*OVERHEAD-SAMPLES*)
    #:style 'solid
    #:label #f))

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
  (define np (render-count num-points "points"))
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
  (define s-info (title-text (format "~a samples of ~a configurations" num-samples sample-size)))
  (add-legend (hb-append (*LEGEND-HSPACE*) (render-benchmark-name name) tp-ratio)
              pict s-info))

(define (add-legend top-left body top-right)
  (rt-superimpose
    (vl-append (*LEGEND-VSPACE*) top-left body)
    top-right))

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

;; =============================================================================


#;(module+ test
  (require rackunit rackunit-abbrevs)

  ;; TODO what to test?
  ;; - actually works, makes plots for various inputs
  ;; - same plots for "same" inputs
  ;; - count-configs fucntion? but it's a plot/function
  ;; - make ticks? also weird output format
  ;; - add legend? ditto idk what besides comparing argb pixels

  (test-case "deliverable-counter"
    (define (check-deliverable-counter/cache bm-name)
      (define pi (benchmark->performance-info (->benchmark-info bm-name)))
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

    ;; Maybe want to put a time limit on this. For me it's like 20 seconds, I don't mind --ben
    (check-deliverable-counter/cache 'PythonFlow)
    (check-deliverable-counter/cache 'call_simple)
  )

)
