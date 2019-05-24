#lang racket

(require gsl-integration)

(define (f x)
  ( / (log x) (sqrt x)))

(define expected (- 4.0))

(define res  (qags f 0 1 #:epsrel 1e-7))

(if (= 0 (first res))
    (begin
      (displayln (string-append "result          = " (~a (second res))))
      (displayln (string-append "exact result    = " (~a expected)))
      (displayln (string-append "result          = " (~a (second res))))
      (displayln (string-append "estimated error = " (~a (third res))))
      (displayln (string-append "actual error    = " (~a (- (second res) expected)))))
    (error "symbol nr = " (first res)))

