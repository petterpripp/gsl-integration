#lang scribble/manual
@; don't run this file for testing:
@(module test racket/base)
@(require (for-label racket))
@(require scribble-math)
@(use-mathjax)
@title{GNU GSL Numerical Integration }
by @author+email[ "Petter Pripp" "petter.pripp@yahoo.com"]
@defmodule[gsl-integration #:packages ["gsl-integration"]]

Interface to GNU GSL Numerical Integration.

Library hides memory allocation and other low level C stuff.

GNU GSL has to be installed separately. Development version of gsl is preferred.
In Ubuntu the package is: libgsl-dev.

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
;actual error    = -8.526512829121202e-14
)

For more examples look at test.rkt source file.


@section{Reference}

The functions will always return a list.

First element is status code. Success when code = 0, otherwise error.

@(bold "Success list:") 0, result. Thereafter one or both (see GNU GSL documentation):  abserr , neveal.

@(bold "Error list:") codenr, gsl-symbol, message.



@defproc[(qng (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              ) (or/c (list/c integer? real? real? integer?)
                      (list/c integer? symbol? string?))]{
 The QNG algorithm is a non-adaptive procedure which uses fixed
 Gauss-Kronrod-Patterson abscissae to sample the integrand at a maximum of 87 points.
 It is provided for fast integration of smooth functions.

 When success, returns: @racketblock[ (0 result abserr neval)]
 When error, returns: @racketblock[ (codenr gsl-symbol message)]}


@defproc[(qng-r (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              ) (list/c real? real? integer?)]{
 Same as qng, but raises an exception when error.
 
 When success, returns: @racketblock[ (result abserr neval)]
}

 

@defproc[(qag (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)
              (#:key    key    exact-positive-integer? 2)
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]{
 The QAG algorithm is a simple adaptive integration procedure.
 The integration region is divided into subintervals,
 and on each iteration the subinterval with the largest estimated error is bisected.
 This reduces the overall error rapidly, as the subintervals become concentrated around local difficulties in the integrand.

 When success, returns: @racketblock[ (0 result abserr)]
 When error, returns: @racketblock[ (codenr gsl-symbol message)]}



@defproc[(qag-r (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)
              (#:key    key    exact-positive-integer? 2)
              ) (list/c real? real?)]{
 Same as qag, but raises an exception when error.
 
 When success, returns: @racketblock[ (result abserr)]
}
                
                      



@defproc[(qags (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]{
QAGS adaptive integration with singularities

The presence of an integrable singularity in the integration region causes an adaptive routine to concentrate new subintervals around the singularity.
As the subintervals decrease in size the successive approximations to the integral converge in a limiting fashion.
This approach to the limit can be accelerated using an extrapolation procedure. The QAGS algorithm combines adaptive bisection with the Wynn epsilon-algorithm to speed up the integration of many types of integrable singularities.
}


@defproc[(qags-r (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (list/c real? real?)]{
 Same as qags, but raises an exception when error.
 
 When success, returns: @racketblock[ (result abserr)]
}


@defproc[(qagp (f (-> flonum? flonum? ))
              (pts (listof real?))              
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]{
 QAGP adaptive integration with known singular points

 Dublicates points will removed and the points will be sorted.                                                          
}


@defproc[(qagp-r (f (-> flonum? flonum? ))
              (pts (listof real?))              
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (list/c real? real?)]{
 Same as qagp, but raises an exception when error.
 
 When success, returns: @racketblock[ (result abserr)]
}


@defproc[(qagi (f (-> flonum? flonum? ))
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]{
 QAGI adaptive integration on infinite interval @($"(-\\infty,+\\infty)") }


@defproc[(qagi-r (f (-> flonum? flonum? ))
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (list/c real? real?) ]{                      
 Same as qagi, but raises an exception when error.
 
 When success, returns: @racketblock[ (result abserr)]}


@defproc[(qagiu (f (-> flonum? flonum? ))
                (a real?)              
                (#:epsabs epsabs real? 0)
                (#:epsrel epsrel real? 1e-8)
                (#:limit  limit  exact-positive-integer? 1000)              
                ) (or/c (list/c integer? real? real?) 
                        (list/c integer? symbol? string?))]{
 QAGIU adaptive integration on semi-infinite interval @($"(a,+\\infty)") }                                                     

@defproc[(qagil (f (-> flonum? flonum? ))
                (b real?)              
                (#:epsabs epsabs real? 0)
                (#:epsrel epsrel real? 1e-8)
                (#:limit  limit  exact-positive-integer? 1000)              
                ) (or/c (list/c integer? real? real?) 
                        (list/c integer? symbol? string?))]{
 QAGIL adaptive integration on semi-infinite interval @($"(-\\infty,b)") }                                                     


@defproc[(qawc (f (-> flonum? flonum? ))
              (a real?)
              (b real?)
              (c real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real?) 
                      (list/c integer? symbol? string?))]{QAWC adaptive integration for Cauchy principal values}


@defproc[(cquad (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:limit  limit  exact-positive-integer? 1000)              
              ) (or/c (list/c integer? real? real? integer?)
                      (list/c integer? symbol? string?))]{
 CQUAD is a doubly-adaptive general-purpose quadrature routine which can handle most types of singularities,
 non-numerical function values such as Inf or NaN, as well as some divergent integrals.
 It generally requires more function evaluations than the integration routines in QUADPACK,
 yet fails less often for difficult integrands.}


@defproc[(romberg (f (-> flonum? flonum? ))
              (a real?)
              (b real?) 
              (#:epsabs epsabs real? 0)
              (#:epsrel epsrel real? 1e-8)
              (#:n  n exact-positive-integer? 20)              
              ) (or/c (list/c integer? real? integer?)
                      (list/c integer? symbol? string?))]{Romberg integration}

@section{Troubleshooting}

@subsection{Romberg}
If you get an error that it can not find Romberg, but not error on the other functions: You have and older version of GNU GSL on our system.
Romberg was added at version 2.5

@subsection{ffi-lib: couldn't open ...}
If you get error:  ffi-lib: couldn't open "libgslcblas.so" (libgslcblas.so: cannot open shared object file: No such file or directory):



Solution 1: Install development version of gsl. In Ubuntu the package is: libgsl-dev.

Solution 2: Modify source code in wrap.rkt with version number.
This apply if you have installed package with version number. Example: libgsl27

@racketblock[ 
(define-ffi-definer gslcblas (ffi-lib "libgslcblas" #:global? #t))
(define-ffi-definer gsl (ffi-lib "libgsl"  #:global? #t))
]

Modify this to

@racketblock[ 
(define-ffi-definer gslcblas (ffi-lib "libgslcblas" '("0" #f) #:global? #t))
(define-ffi-definer gsl (ffi-lib "libgsl" '("27" #f)  #:global? #t))
]


Solution 3: Specify absolute path in wrap.rkt without .so extension (package location may differ).

@racketblock[ 
 (define-ffi-definer gsl (ffi-lib "/usr/lib/x86_64-linux-gnu/libgslcblas"  #:global? #t))
 (define-ffi-definer gslcblas (ffi-lib "/usr/lib/x86_64-linux-gnu/libgsl" #:global? #t))
]


@subsection{DrRacket freeze/crash}
The function f should not raise error. C-library do not work with Racket exception's.
Try run at command line to see what exception is raised.
