#lang racket/base

(require racket/contract/base)
(provide
  (struct-out configuration-info)
  (contract-out
   [configuration-info->id
    (-> configuration-info? any)]
   [configuration-info->num-types
    (-> configuration-info? natural?)]
   [configuration-info->runtime*
    (-> configuration-info? (listof nonnegative-real/c))]
   [configuration-info->mean-runtime
    (-> configuration-info? nonnegative-real/c)]))

(require
  gtp-plot/util
  (only-in racket/math natural?)
  (only-in math/statistics mean))

;; =============================================================================

(struct configuration-info (
  id
  num-types
  runtime*
) #:prefab )

(define configuration-info->id
  configuration-info-id)

(define configuration-info->num-types
  configuration-info-num-types)

(define configuration-info->runtime*
  configuration-info-runtime*)

(define (configuration-info->mean-runtime cfg)
  (define t* (configuration-info->runtime* cfg))
  (mean t*))
