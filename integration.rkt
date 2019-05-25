#lang racket

(require (rename-in ffi/unsafe (-> ~>))         
         "pointer.rkt"
         "wrap.rkt")
         
        
(begin-for-syntax
  (require racket/syntax syntax/parse))

(define (gc-free . args)
  (map (lambda (arg)
         (cond
           [(or (double-pointer? arg) (size-pointer? arg) (gsl_function? arg)) (free arg)]
           [(gsl_integration_workspace-pointer? arg) (gsl_integration_workspace_free arg)]
           [(gsl_integration_cquad_workspace-pointer? arg) (gsl_integration_cquad_workspace_free arg)]
           [(gsl_integration_romberg_workspace-pointer? arg) (gsl_integration_romberg_free arg)]
           [else (error (string-append "gc-free: Unknown pointer = "  (~a arg)))])) args))

(define (err status)
  (list status (gsl_errno_symbol status) (gsl_errno_msg status)))

(define err? (list/c integer? symbol? string?))

;(gsl gsl_integration_qng (_fun _gsl_function-pointer _double _double _double _double _double-pointer _double-pointer _size-pointer -> _int))
(define/contract (qng f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] )
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? ) (or/c (list/c integer? real? real? integer?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [result (alloc_double)]    
        [abserr (alloc_double)]
        [nevals (alloc_size)])
    (begin     
      (define status (gsl_integration_qng cb-f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) result abserr nevals ))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double) (ptr-ref nevals _size))
            (err status)))
      (gc-free cb-f result abserr nevals)
      rl)))

;(gsl gsl_integration_qag (_fun _gsl_function-pointer _double _double _double _double _size _int _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qag f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000] #:key [key 2])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? #:key exact-positive-integer?) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)])        
    (begin     
      (define status (gsl_integration_qag cb-f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) limit key w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr)      
      rl)))

;(gsl gsl_integration_qags (_fun _gsl_function-pointer _double _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qags f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)])        
    (begin     
      (define status (gsl_integration_qags cb-f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr)      
      rl)))

;(gsl gsl_integration_qagp (_fun _gsl_function-pointer _double-pointer _size _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qagp f pts #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000] )
  (->* ((-> flonum? flonum? ) (listof real?) ) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)]
        [_pts (make-double_array pts)])
    (begin     
      (define status (gsl_integration_qagp cb-f _pts (length pts) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr _pts)      
      rl)))

;(gsl gsl_integration_qagi (_fun _gsl_function-pointer _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qagi f #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? )) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)])        
    (begin     
      (define status (gsl_integration_qagi cb-f (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr)      
      rl)))

;(gsl gsl_integration_qagiu (_fun _gsl_function-pointer _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qagiu f a #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)])        
    (begin     
      (define status (gsl_integration_qagiu cb-f (exact->inexact a) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr)      
      rl)))

;(gsl gsl_integration_qagil (_fun _gsl_function-pointer _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qagil f b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)])        
    (begin     
      (define status (gsl_integration_qagil cb-f (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr)      
      rl)))

;(gsl gsl_integration_qawc (_fun _gsl_function-pointer _double _double _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qawc f a b c #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real? real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)])        
    (begin     
      (define status (gsl_integration_qawc cb-f (exact->inexact a) (exact->inexact b) (exact->inexact c) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double))
            (err status)))
      (gc-free cb-f w result abserr)      
      rl)))

;(gsl gsl_integration_cquad (_fun _gsl_function-pointer _double _double _double _double _gsl_integration_cquad_workspace-pointer _double-pointer _double-pointer _size-pointer -> _int))
(define/contract (cquad f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real? integer?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_cquad_workspace_alloc limit)]
        [result (alloc_double)]    
        [abserr (alloc_double)]
        [nevals (alloc_size)])
    (begin     
      (define status (gsl_integration_cquad cb-f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel)  w result abserr nevals ))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref abserr _double) (ptr-ref nevals _size))
            (err status)))
      (gc-free cb-f w result abserr nevals)
      rl)))

;(gsl gsl_integration_romberg (_fun _gsl_function-pointer _double _double _double _double _double-pointer _size-pointer _gsl_integration_romberg_workspace-pointer -> _int))
(define/contract (romberg f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:n [n 20])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:n exact-positive-integer? ) (or/c (list/c integer? real? integer?) err?))
  (let ([cb-f (gsl-callback-alloc f)]
        [w (gsl_integration_romberg_alloc n)]
        [result (alloc_double)]
        [nevals (alloc_size)])
    (begin     
      (define status (gsl_integration_romberg cb-f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) result nevals w))
      (define rl
        (if (= status 0) 
            (list status (ptr-ref result _double) (ptr-ref nevals _size))
            (err status)))
      (gc-free cb-f w result nevals)
      rl)))

(provide  
 qng
 qag
 qags
 qagp
 qagi
 qagiu
 qagil
 qawc
 cquad
 romberg)
 
 
; don't run this file for testing:
(module test racket/base)
