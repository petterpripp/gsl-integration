#lang info
(define collection "gsl-integration")
(define version "1.0")
(define deps '("base"
               "scribble-lib"
               "rackunit-lib"
               "racket-doc"))
(define scribblings '(("scribblings/gsl-integration.scrbl" ())))
(define pkg-desc "Binding to gnu gsl numerical integration")
(define pkg-authors '(Petter Pripp))
;raco pkg install -n gsl-integration
;raco test -m test.rkt
