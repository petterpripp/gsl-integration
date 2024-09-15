#lang racket/base

(require ffi/unsafe 
         ffi/unsafe/define
         "pointer.rkt"
         racket/list
         racket/format)


(define-ffi-definer gslcblas (ffi-lib "libgslcblas" '("0" #f) #:global? #t))
(define-ffi-definer gsl (ffi-lib "libgsl" '("27" #f)  #:global? #t))
;(define-ffi-definer gslwrap (ffi-lib "./wrap/libgslwrap" #:global? #t))


; Turns off default gsl error handler, preventing unwanted abort of program and freeze of DrRacket.
(define _gsl_error_handler_t-pointer (_cpointer/null 'gsl_error_handler_t))
;(c2gsl "gsl_error_handler_t * gsl_set_error_handler_off();")
(gsl gsl_set_error_handler_off (_fun -> _gsl_error_handler_t-pointer))
(define previous_handler (gsl_set_error_handler_off))


(define gsl_errno
  '((GSL_SUCCESS  0  "SUCCESS")
    (GSL_FAILURE  -1 "FAILURE")
    (GSL_CONTINUE -2  "iteration has not converged")
    (GSL_EDOM     1   "input domain error, e.g sqrt(-1)")
    (GSL_ERANGE   2   "output range error, e.g. exp(1e100)")
    (GSL_EFAULT   3   "invalid pointer")
    (GSL_EINVAL   4   "invalid argument supplied by user")
    (GSL_EFAILED  5   "generic failure")
    (GSL_EFACTOR  6   "factorization failed")
    (GSL_ESANITY  7   "sanity check failed - shouldn't happen")
    (GSL_ENOMEM   8   "malloc failed")
    (GSL_EBADFUNC 9   "problem with user-supplied function")
    (GSL_ERUNAWAY 10  "iterative process is out of control")
    (GSL_EMAXITER 11  "exceeded max number of iterations")
    (GSL_EZERODIV 12  "tried to divide by zero")
    (GSL_EBADTOL  13  "user specified an invalid tolerance")
    (GSL_ETOL     14  "failed to reach the specified tolerance")
    (GSL_EUNDRFLW 15  "underflow")
    (GSL_EOVRFLW  16  "overflow")
    (GSL_ELOSS    17  "loss of accuracy")
    (GSL_EROUND   18  "failed because of roundoff error")
    (GSL_EBADLEN  19  "matrix, vector lengths are not conformant")
    (GSL_ENOTSQR  20  "matrix not square")
    (GSL_ESING    21  "apparent singularity detected")
    (GSL_EDIVERGE 22  "integral or series is divergent")
    (GSL_EUNSUP   23  "requested feature is not supported by the hardware")
    (GSL_EUNIMPL  24  "requested feature not (yet) implemented")
    (GSL_ECACHE   25  "cache limit exceeded")
    (GSL_ETABLE   26  "table limit exceeded")
    (GSL_ENOPROG  27  "iteration is not making progress towards solution")
    (GSL_ENOPROGJ 28  "jacobian evaluations are not improving the solution")
    (GSL_ETOLF    29  "cannot reach the specified tolerance in F")
    (GSL_ETOLX    30  "cannot reach the specified tolerance in X")
    (GSL_ETOLG    31  "cannot reach the specified tolerance in gradient")
    (GSL_EOF      32  "end of file")))


(define (gsl_errno_element nr)
  (let ([res (filter (lambda (x) (= (second x) nr)) gsl_errno)])
    (if ( = (length res) 1)
        (first res)
        (error (string-append "Did not find correct gsl_errno_element. nr = " (~a nr) " res = " (~a res))))))


(define (gsl_errno_symbol nr)
  (first (gsl_errno_element nr)))

(define (gsl_errno_msg nr)
  (third (gsl_errno_element nr)))


(gsl gsl_integration_workspace_alloc (_fun _size -> _gsl_integration_workspace-pointer))
(gsl gsl_integration_workspace_free (_fun _gsl_integration_workspace-pointer -> _void))
(gsl gsl_integration_qng (_fun _gsl_function-pointer _double _double _double _double _double-pointer _double-pointer _size-pointer -> _int))
(gsl gsl_integration_qag (_fun _gsl_function-pointer _double _double _double _double _size _int _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_qags (_fun _gsl_function-pointer _double _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_qagp (_fun _gsl_function-pointer _double-pointer _size _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_qagi (_fun _gsl_function-pointer _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_qagiu (_fun _gsl_function-pointer _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_qagil (_fun _gsl_function-pointer _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_qawc (_fun _gsl_function-pointer _double _double _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(gsl gsl_integration_cquad_workspace_alloc (_fun _size -> _gsl_integration_cquad_workspace-pointer))
(gsl gsl_integration_cquad_workspace_free (_fun _gsl_integration_cquad_workspace-pointer -> _void))
(gsl gsl_integration_cquad (_fun _gsl_function-pointer _double _double _double _double _gsl_integration_cquad_workspace-pointer _double-pointer _double-pointer _size-pointer -> _int))
(gsl gsl_integration_romberg_alloc (_fun _size -> _gsl_integration_romberg_workspace-pointer))
(gsl gsl_integration_romberg_free (_fun _gsl_integration_romberg_workspace-pointer -> _void))
(gsl gsl_integration_romberg (_fun _gsl_function-pointer _double _double _double _double _double-pointer _size-pointer _gsl_integration_romberg_workspace-pointer -> _int))


(provide
 gsl_integration_workspace_alloc
 gsl_integration_workspace_free
 gsl_integration_qng
 gsl_integration_qag 
 gsl_integration_qags
 gsl_integration_qagp 
 gsl gsl_integration_qagi
 gsl_integration_qagiu
 gsl_integration_qagil
 gsl_integration_qawc
 gsl_integration_cquad_workspace_alloc
 gsl_integration_cquad_workspace_free
 gsl_integration_cquad
 gsl_integration_romberg_alloc
 gsl_integration_romberg_free
 gsl_integration_romberg
 gsl_errno_symbol
 gsl_errno_msg)
 
; don't run this file for testing:
(module test racket/base)
