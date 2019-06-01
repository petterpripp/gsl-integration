#lang info

(define collection "gsl-integration")
(define deps '("base" "rackunit-lib"))               
(define build-deps '("racket-doc" "scribble-lib" "scribble-math"))
(define scribblings '(("scribblings/gsl-integration.scrbl" ())))
(define pkg-desc "Binding to gnu gsl numerical integration")
(define pkg-authors '(Petter Pripp))
(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))
