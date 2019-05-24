#lang scribble/manual
@(require (for-label racket))

@title{GNU GSL Numeric Integration }
by @author+email[ "Petter Pripp" "petter.pripp@yahoo.com"]
@defmodule[gsl-integration #:packages ["gsl-integration"]]

Racket library on top of GNU GSL Numeric Integration C functions.

Library hides memory allocation and other low level C stuff.

GNU GSL has to be installed separately.
Tested for version 2.5.

Naming of functions and keywords follow GNU GSL documentation
@url{https://www.gnu.org/software/gsl/doc/html/integration.html}

The source code is distributed under the GNU General Public License.

@section{Example}

@racketblock[             

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
    (error "status = " (~a (first res))))]


@#reader scribble/comment-reader
(racketblock
;result          = -4.000000000000085 
;exact result    = -4.0
;result          = -4.000000000000085
;estimated error = 1.354472090042691e-13
;actual error    = -8.526512829121202e-1
)

For more examples look at test.rkt source file.

Tip: When working with math formulas it is recommended to use a infix library, for better readability.
For example: @url{https://pkgs.racket-lang.org/package/infix}


@section{Reference}

The functions will always return a list.

First element is status code. Success when code = 0, otherwise error.

Success list: 0, result. Thereafter one or both (see GNU GSL documentation):  abserr , neveals.

Error list: codenr, gsl-symbol, message.

--------

QNG non-adaptive Gauss-Kronrod integration

@defproc[(qng (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              ) (or/c (list/c integer? real? real? integer?)
                      (list/c integer? symbol? string?))]

QAG adaptive integration
@defproc[(qag (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)
              (#:key    key    exact-positive-integer? 2)
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]

QAGS adaptive integration with singularities
@defproc[(qags (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]

QAGP adaptive integration with known singular points
@defproc[(qagp (f (-> flonum? flonum? ))
              (pts (listof real?))              
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]

QAGI adaptive integration on infinite intervals
@defproc[(qagi (f (-> flonum? flonum? ))
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]


@defproc[(qagiu (f (-> flonum? flonum? ))
                (a real?)              
                (#:epsabs epsabs real? 0)
                (#:epsrel epsrel real? 1e-8)
                (#:limit  limit  exact-positive-integer? 1000)              
                ) (or/c (list/c integer? real? real?) 
                        (list/c integer? symbol? string?))]

@defproc[(qagil (f (-> flonum? flonum? ))
                (b real?)              
                (#:epsabs epsabs real? 0)
                (#:epsrel epsrel real? 1e-8)
                (#:limit  limit  exact-positive-integer? 1000)              
                ) (or/c (list/c integer? real? real?) 
                        (list/c integer? symbol? string?))]

QAWC adaptive integration for Cauchy principal values
@defproc[(qawc (f (-> flonum? flonum? ))
              (a real?)
              (b real?)
              (c real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]

CQUAD doubly-adaptive integration
@defproc[(cquad (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real? integer?)
                      (list/c integer? symbol? string?))]

Romberg integration
@defproc[(romberg (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:n  n exact-positive-integer? 20)              
              ) (or/c (list/c integer? real? integer?)
                      (list/c integer? symbol? string?))]
