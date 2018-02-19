#lang racket/base

;; Subtype of performance info,
;;  represents multiple sample datasets for a benchmark.

;; TODO document this better

(require racket/contract/base)
(provide
  sample-info?

  (contract-out
   [make-sample-info
    (-> performance-info? (listof path-string?) sample-info?)]

   [sample-info->sample-size
    (-> sample-info? natural?)]

   [sample-info->performance-info*
    (-> sample-info? (listof performance-info?))]))

(require
  gtp-plot/performance-info
  gtp-plot/util
  (only-in racket/math
    natural?))

;; =============================================================================

(struct sample-info performance-info (
  sample-size
  sample-src*
) #:transparent)

(define (make-sample-info pi src*)
  (define sample-size
    (infer-sample-size pi src*))
  (sample-info (performance-info->name pi)
               (performance-info->src pi)
               (performance-info->num-units pi)
               (performance-info->num-configurations pi)
               (performance-info->baseline-runtime pi)
               (performance-info->untyped-runtime pi)
               (performance-info->typed-runtime pi)
               (performance-info->make-in-configurations pi)
               sample-size
               src*))

(define (infer-sample-size pi src*)
  (for/fold ([ss #f])
            ([src (in-list src*)])
    (define pi%s (performance-info-update-src pi src))
    (define new-ss (count-configurations pi%s (λ (cfg) #true)))
    (cond
     [(not ss)
      new-ss]
     [(= new-ss ss)
      ss]
     [else
      (log-gtp-plot-error "~a: expected ~a samples, got ~a samples in file ~a" (performance-info->name pi) ss new-ss src)
      (min new-ss ss)])))

(define sample-info->sample-size
  sample-info-sample-size)

(define (sample-info->performance-info* si)
  (for/list ([src (in-list (sample-info-sample-src* si))])
    (performance-info-update-src si src)))

;; =============================================================================

(module+ test
  ;; TODO
)

