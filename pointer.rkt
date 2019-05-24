#lang racket

(require ffi/unsafe ffi/unsafe/define)        

(begin-for-syntax
  (require racket/syntax syntax/parse))


(define-syntax (make-pointer stx)
  (syntax-parse stx 
    [(_ ctype)
     (with-syntax ([pointer (format-id #'ctype "_~a-pointer" (syntax->datum #'ctype))]
                   [pointer? (format-id #'ctype "~a-pointer?" (syntax->datum #'ctype))])
       #'(begin (define-cpointer-type pointer) (provide pointer pointer?)))]))


(make-pointer double)
(make-pointer size)
(make-pointer char)
(make-pointer gsl_integration_workspace)
(make-pointer gsl_integration_cquad_workspace)
(make-pointer gsl_integration_romberg_workspace)


(define (alloc_double) ; raw for use outside garbage collector
  (let ([p (malloc 'raw (ctype-sizeof _double))])
    (begin
      (cpointer-push-tag! p 'double-pointer)
      p)))
  
(define (alloc_size) ; raw for use outside garbage collector    
  (let ([p (malloc 'raw (ctype-sizeof _size))])
    (begin
      (cpointer-push-tag! p 'size-pointer)
      p)))

(define (make-double_array lst)
  (let ([p (malloc 'raw (* (ctype-sizeof _double) (length lst)))])         
    (begin
      (for-each
       (lambda (i) (ptr-set! p _double i (exact->inexact (list-ref lst i))))
       (range (length lst)))
      (cpointer-push-tag! p 'double-pointer)
      p)))      


(define-syntax (def-gsl-struct stx)
  (syntax-parse stx 
    [(def-gsl-struct struct-navn [param-navn type] ...)
     (with-syntax ([pointer (format-id #'struct-navn "_~a-pointer" (syntax->datum #'struct-navn))]
                   [pointer? (format-id #'struct-navn "~a?" (syntax->datum #'struct-navn))]
                   [snavn (format-id #'struct-navn "_~a" (syntax->datum #'struct-navn))]
                   [make (format-id #'struct-navn "make-~a" (syntax->datum #'struct-navn))])                   
       #'(begin
           (define-cstruct snavn ([param-navn type] ...) #:malloc-mode 'raw )
           (provide pointer pointer? make)))]))

; Using function f(x), not (f x params). The use of params in function make program unstable!
(def-gsl-struct gsl_function [function (_fun _double  -> _double)] [params _double-pointer/null] )

(define-syntax-rule (gsl-callback-alloc f)
  (make-gsl_function f #f ))



(provide
 gsl-callback-alloc
 alloc_double
 alloc_size
 make-double_array)