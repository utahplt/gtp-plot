#lang racket/base

;; Subtype of performance info,
;;  represents multiple sample datasets for a benchmark.

;; TODO document this better

(require racket/contract/base)
(provide
  sample-info?

  (contract-out
   [make-sample-info
    (-> performance-info? (listof (listof configuration-info?)) sample-info?)]

   [sample-info->sample-size
    (-> sample-info? natural?)]

   [sample-info->num-samples
    (-> sample-info? natural?)]

   [sample-info->performance-info*
    (-> sample-info? (listof performance-info?))]))

(require
  gtp-plot/performance-info
  gtp-plot/configuration-info
  gtp-plot/util
  (only-in racket/math
    natural?))

;; =============================================================================

(struct sample-info performance-info (
  sample-size
  sample-cfg**
) #:transparent)

(define (make-sample-info pi ci**)
  (define sample-size
    (infer-sample-size ci**))
  (sample-info (performance-info->name pi)
               (performance-info->src pi)
               (performance-info->num-units pi)
               (performance-info->num-configurations pi)
               (performance-info->baseline-runtime* pi)
               (performance-info->untyped-runtime* pi)
               (performance-info->typed-runtime* pi)
               in-sample-info-configurations
               sample-size
               ci**))

(define (in-sample-info-configurations si)
  (apply in-sequences (map in-list (sample-info-sample-cfg** si))))

(define (infer-sample-size ci**)
  (for/fold ([ss #f])
            ([ci* (in-list ci**)]
             [i (in-naturals)])
    (define new-ss (length ci*))
    (cond
     [(not ss)
      new-ss]
     [(= new-ss ss)
      ss]
     [else
      (log-gtp-plot-error "expected ~a samples, got ~a samples in sample ~a" ss new-ss i)
      (min new-ss ss)])))

(define sample-info->sample-size
  sample-info-sample-size)

(define (sample-info->num-samples si)
  (length (sample-info-sample-cfg** si)))

(define (sample-info->performance-info* si)
  (for/list ([cfg* (in-list (sample-info-sample-cfg** si))])
    (sample-info-update-sample-cfg** si (list cfg*))))

(define (sample-info-update-sample-cfg** si cfg**)
  (sample-info (performance-info->name si)
               (performance-info->src si)
               (performance-info->num-units si)
               (performance-info->num-configurations si)
               (performance-info->baseline-runtime* si)
               (performance-info->untyped-runtime* si)
               (performance-info->typed-runtime* si)
               (performance-info-make-in-configurations si)
               (sample-info-sample-size si)
               cfg**))

;; =============================================================================

(module+ test
  (require rackunit)

  (test-case "make-sample-info"
             (define cfg (configuration-info 'A 0 '(4 4)))
    (define si (make-sample-info (make-performance-info 'dummy #:src "dummy" #:num-units 2 #:num-configurations 2 #:baseline-runtime* '(10) #:untyped-runtime* '(10) #:typed-runtime* '(10) #:make-in-configurations (lambda (x) '(a b c)))
                                 (list (list cfg))))
    (check-true (sample-info? si))
    (check-true (performance-info? si))
    (check-equal? (for/list ((x (in-configurations si))) x)
                  (list cfg))
    (check-equal? (sample-info->sample-size si) 1)
    (let ([pi* (sample-info->performance-info* si)])
      (check-equal? (length pi*) 1)))

  (test-case "infer-sample-size"
    (check-equal? (infer-sample-size '()) #f)
    (check-equal? (infer-sample-size '((1) (1))) 1)
    (check-equal? (infer-sample-size '((1 1) (1 1) (1 1))) 2))
)

