#lang info

(define collection "gsl-integration")
(define deps '("base"
               "scribble-lib"
               "rackunit-lib"
               "racket-doc"))
(define scribblings '(("scribblings/gsl-integration.scrbl" ())))
(define pkg-desc "Binding to gnu gsl numerical integration")
(define pkg-authors '(Petter Pripp))
(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))
