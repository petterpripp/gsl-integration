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

(define-syntax (call-no-raise stx)
  (syntax-parse stx 
    [(_ (gsl_f:id funksjon:id args ...) (ok ...) (free ...))     
     ;#:with f-no-raise (format-id #'funksjon "f-no-raise")
     ;#:with f-errmsg (format-id #'funksjon "f-errmsg")
     ;#:with cb-f (format-id #'funksjon "cb-f")
     ;#:with status (format-id #'funksjon "status")
     #'(begin
         (define f-errmsg "")
         (define (f-no-raise funksjon)
           (λ (x)
             (with-handlers ([exn:fail? (lambda (e)
                                          (begin
                                            (if (= (string-length f-errmsg) 0)
                                                (set! f-errmsg (string-append f-errmsg " f-errmsg = " (exn-message e) " x = " (~a x)))
                                                '())
                                            +nan.0))])        
               (if (= (string-length f-errmsg) 0)
                   (funksjon x)
                   +nan.0))))
         (define cb-f (gsl-callback-alloc (f-no-raise funksjon)))
         (define status (gsl_f cb-f  args ...))
         (define rl
          (cond
            [(and (= status 0) (= (string-length f-errmsg) 0))
             (list status ok ...)]
            [(and (= status 0) ((string-length f-errmsg) . > . 0))
             (list 1000 'f-errmsg f-errmsg)]
            [else
             (let ([e (err status)])
               (list (first e) (second e)  (string-append (third e) f-errmsg)))]))
        (gc-free cb-f free ...)
        rl)]))

         

;(gsl gsl_integration_qng (_fun _gsl_function-pointer _double _double _double _double _double-pointer _double-pointer _size-pointer -> _int))
(define/contract (qng f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] )
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? ) (or/c (list/c integer? real? real? integer?) err?))
  (define result (alloc_double))
  (define abserr (alloc_double))
  (define nevals (alloc_size))
  
  (call-no-raise
   (gsl_integration_qng f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) result abserr nevals )
   ((ptr-ref result _double) (ptr-ref abserr _double) (ptr-ref nevals _size))
   (result abserr nevals)))
   
; Same as qng, but raise error
(define/contract (qng-r f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] )
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? ) (list/c real? real? integer?))
  (define res (qng f a b #:epsabs epsabs  #:epsrel epsrel))
  (if (= (first res) 0)
      (rest res)
      (raise-arguments-error 'qng-r
                             (third res)                             
                             "gsl_errno_code" (first res)
                             "gsl_errno_symbol" (second res)
                             "a" a
                             "b" b
                             "epsabs" epsabs
                             "epsrel" epsrel)))
                             


;(gsl gsl_integration_qag (_fun _gsl_function-pointer _double _double _double _double _size _int _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qag f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000] #:key [key 2])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? #:key exact-positive-integer?) (or/c (list/c integer? real? real?) err?))
  (define w (gsl_integration_workspace_alloc limit))
  (define result (alloc_double))
  (define abserr (alloc_double))

  (call-no-raise
   (gsl_integration_qag f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) limit key w result abserr)
   ((ptr-ref result _double) (ptr-ref abserr _double))
   (w result abserr)))


; Same as qag, but raise error
(define/contract (qag-r f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000] #:key [key 2])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? #:key exact-positive-integer?) (list/c real? real?))
  (define res (qag f a b #:epsabs epsabs  #:epsrel epsrel #:limit limit #:key key))
  (if (= (first res) 0)
      (rest res)
      (raise-arguments-error 'qag-r
                             (third res)                             
                             "gsl_errno_code" (first res)
                             "gsl_errno_symbol" (second res)
                             "a" a
                             "b" b
                             "epsabs" epsabs
                             "epsrel" epsrel
                             "limit" limit
                             "key" key )))
                             



;(gsl gsl_integration_qags (_fun _gsl_function-pointer _double _double _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qags f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
    (define w (gsl_integration_workspace_alloc limit))
    (define result (alloc_double))
    (define abserr (alloc_double))
    (call-no-raise
     (gsl_integration_qags f (exact->inexact a) (exact->inexact b) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr)
     ((ptr-ref result _double) (ptr-ref abserr _double))
     (w result abserr)))

; Same as qags, but raise error
(define/contract (qags-r f a b #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) real? real?) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer?) (list/c real? real?))
  (define res (qags f a b #:epsabs epsabs  #:epsrel epsrel #:limit limit))
  (if (= (first res) 0)
      (rest res)
      (raise-arguments-error 'qags-r
                             (third res)                             
                             "gsl_errno_code" (first res)
                             "gsl_errno_symbol" (second res)
                             "a" a
                             "b" b
                             "epsabs" epsabs
                             "epsrel" epsrel
                             "limit" limit)))
                             


;(gsl gsl_integration_qagp (_fun _gsl_function-pointer _double-pointer _size _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qagp f pts #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000] )
  (->* ((-> flonum? flonum? ) (listof real?) ) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))
  (define w (gsl_integration_workspace_alloc limit))
  (define result (alloc_double))
  (define abserr (alloc_double))
  (define unique-sort-pts (sort (remove-duplicates pts =) <))
  (define _pts (make-double_array unique-sort-pts))
  (call-no-raise
     (gsl_integration_qagp f _pts (length unique-sort-pts) (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr)
     ((ptr-ref result _double) (ptr-ref abserr _double))
     (w result abserr _pts)))


; Same as qagp, but raise error
(define/contract (qagp-r f pts #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? ) (listof real?)) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer?) (list/c real? real?))
  (define res (qagp f pts #:epsabs epsabs  #:epsrel epsrel #:limit limit))
  (if (= (first res) 0)
      (rest res)
      (raise-arguments-error 'qagp-r
                             (third res)                             
                             "gsl_errno_code" (first res)
                             "gsl_errno_symbol" (second res)
                             "pts" pts
                             "epsabs" epsabs
                             "epsrel" epsrel
                             "limit" limit)))



;(gsl gsl_integration_qagi (_fun _gsl_function-pointer _double _double _size _gsl_integration_workspace-pointer _double-pointer _double-pointer -> _int))
(define/contract (qagi f #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? )) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (or/c (list/c integer? real? real?) err?))  
  (define w (gsl_integration_workspace_alloc limit))
  (define result (alloc_double))
  (define abserr (alloc_double))
  (call-no-raise
     (gsl_integration_qagi f (exact->inexact epsabs) (exact->inexact epsrel) limit w result abserr)
     ((ptr-ref result _double) (ptr-ref abserr _double))
     (w result abserr )))
  

(define/contract (qagi-r f #:epsabs [epsabs 0] #:epsrel [epsrel 1e-8] #:limit [limit 1000])
  (->* ((-> flonum? flonum? )) (#:epsabs real?  #:epsrel real? #:limit exact-positive-integer? ) (list/c real? real?))
  (define res (qagi f #:epsabs epsabs #:epsrel epsrel #:limit limit ))
  (if (= (first res) 0)
      (rest res)
      (raise-arguments-error 'qagi-r
                             (third res)                             
                             "gsl_errno_code" (first res)
                             "gsl_errno_symbol" (second res)
                             "epsabs" epsabs
                             "epsrel" epsrel
                             "limit" limit)))  
  


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
 qng-r 
 qag
 qag-r
 qags
 qags-r
 qagp
 qagp-r
 qagi
 qagi-r
 qagiu
 qagil
 qawc
 cquad
 romberg)
 
 
; don't run this file for testing:
(module test racket/base)
