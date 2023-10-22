#lang racket

(require ffi/unsafe                   
         gsl-integration)

(require rackunit
         rackunit/text-ui)
         


(define (f1 x )
  (* (expt x 2.6) (log (/ 1 x))))    

(define (myfn1 x )
  (exp (- (- x) (* x x))))  

(define (myfn2 x )
  (exp x))    

(define (f454 x)
  (let* ([x2  (* x x)]
         [x3  (* x x2)])
    (* x3  (log (abs (* (- x2 1) (- x2 2)))))))  

(define (f455 x )
  (/ (log x) (+ 1 (* 100 x x))))    

(define (f459 x)
  (/ 1 (+ (* 5 x x x) 6)))  

(define (f-example x)
  (/ (log x) (sqrt x)))
  

(define (t-qng-etol a b)  
  (let ([rl (qng f-example a b #:epsabs 1e-1 #:epsrel 0.0 )])
    (check-equal? 14 (first rl) "status"))) ;(GSL_ETOL     14  "failed to reach the specified tolerance")

(define (t-qng a b)  
  (let ( [exp_result 7.716049379303083211e-02]    
         [exp_abserr 9.424302199601294244e-08]
         [rl (qng f1 a b #:epsabs 1e-1 #:epsrel 0.0 )])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) (if (< a b) exp_result (- exp_result))   1e-15 "result")
          (check-within (third rl) exp_abserr 1e-7 "abserr"))
        (error "qng status =" (~a rl)))))

(define (t-qng-lambda a b)  
  (let ( [exp_result 7.716049379303083211e-02]    
         [exp_abserr 9.424302199601294244e-08]
         [rl (qng (lambda (x) (* (expt x 2.6) (log (/ 1 x)))) a b #:epsabs 1e-1 #:epsrel 0.0 )])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) (if (< a b) exp_result (- exp_result))   1e-15 "result")
          (check-within (third rl) exp_abserr 1e-7 "abserr"))
        (error "qng-lambda status =" (~a rl)))))


(define (t-qag a b)  
  (let ( [exp_result 7.716049382715854665E-02]    
         [exp_abserr 6.679384885865053037E-12]
         [rl (qag f1 a b #:epsrel 1e-10 #:key 1 )])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) (if (< a b) exp_result (- exp_result))   1e-15 "result")
          (check-within (third rl) exp_abserr 1e-6 "abserr"))
        (error "qag status =" (~a rl)))))

(define (t-qag-ebadtol a b)  
  (let ([rl (qag f-example a b #:epsabs -1 #:epsrel 0.0 )])
    (check-equal? 13 (first rl) "status"))) ;(GSL_EBADTOL  13  "user specified an invalid tolerance")


(define (t-qags a b)  
  (let ( [exp_result 7.716049382715789440E-02]    
         [exp_abserr 2.216394961010438404E-12]
         [rl (qags f1 a b #:epsrel 1e-10)])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) (if (< a b) exp_result (- exp_result))   1e-15 "result")
          (check-within (third rl) exp_abserr 1e-6 "abserr"))
        (error "qags status =" (~a rl)))))


(define (t-qagp pts )
  (let ( [exp_result 5.274080611672716401E+01]    
         [exp_abserr 1.755703848687062418E-04]
         [rl (qagp f454 pts  #:epsrel 1e-3)])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) exp_result    1e-14 "result")
          (check-within (third rl) exp_abserr 1e-5 "abserr"))
        (error "qagp status =" (~a rl)))))


(define (t-qagi )  
  (let ( [exp_result 2.275875794468747770]    
         [exp_abserr 7.436490118267390744E-09]
         [rl (qagi myfn1 #:epsabs 1e-7 #:epsrel 0)])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) exp_result  1e-14 "result")
          (check-within (third rl) exp_abserr 1e-5 "abserr"))
        (error "qagi status =" (~a rl)))))

(define (t-qagiu a)  
  (let ( [exp_result -3.616892186127022568E-01]    
         [exp_abserr 3.016716913328831851E-06]
         [rl (qagiu f455 a #:epsrel 1e-3 )])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) exp_result  1e-14 "result")
          (check-within (third rl) exp_abserr 1e-5 "abserr"))
        (error "qagiu status =" (~a rl)))))

(define (t-qagil b)  
  (let ( [exp_result 2.718281828459044647]    
         [exp_abserr 1.588185109253204805E-10]
         [rl (qagil myfn2 b #:epsabs 1e-7 #:epsrel 0)])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) exp_result  1e-14 "result")
          (check-within (third rl) exp_abserr 1e-5 "abserr"))
        (error "qagiu status =" (~a rl)))))

(define (t-qawc a b c)  
  (let ( [exp_result -8.994400695837000137E-02]    
         [exp_abserr 1.185290176227023727E-06]
         [rl (qawc f459 a b c #:epsrel 1e-3)])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) (if (< a b) exp_result (- exp_result)) 1e-14 "result")
          (check-within (third rl) exp_abserr 1e-6 "abserr"))
        (error "qawc status =" (~a rl)))))


(define (t-cquad a b)  
  (let ( [exp_result 7.716049382715789440E-02]    
         [exp_abserr 2.216394961010438404E-12]
         [rl (cquad f1 a b #:epsrel 1e-10)])
    (if (= (first rl) 0)
        (begin
          (check-within (second rl) (if (< a b) exp_result (- exp_result))  1e-10 "result")
          (check-within (third rl) exp_abserr 1e-6 "abserr"))
        (error "cquad status =" (~a rl)))))


(define (t-romberg a b)  
  (let ([exp_result 1.0]             
        [rl (romberg sin a b #:epsrel 1e-10)])
    (if (= (first rl) 0)        
        (check-within (second rl) (if (< a b) exp_result (- exp_result)) 1e-15 "result")          
        (error "romberg status =" (~a rl)))))


(define i-tests
  (test-suite
   "Integral"

   (test-case
    "QNG GSL_ETOL"
    (t-qng-etol 0.0 1.0 ))      
   
   (test-case
    "QNG"
    (t-qng 0.0 1.0 )
    (t-qng 1.0 0.0))

   (test-case
    "QNG lambda"
    (t-qng-lambda 0.0 1.0 )
    (t-qng-lambda 1.0 0.0))

   (test-case
    "qng = qng-r"
    (check-equal?
     (rest (qng f1 0 1 #:epsabs 1e-1 #:epsrel 0.0))
     (qng-r f1 0 1 #:epsabs 1e-1 #:epsrel 0.0)))

   (test-case
    "qng-r raise error"
    (check-exn     
     (regexp "qng-r: failed to reach the specified tolerance\n  gsl_errno_code: 14\n  gsl_errno_symbol: 'GSL_ETOL\n  a: 0\n  b: 1\n  epsabs: 0.1\n  epsrel: 0.0")
     (λ () (qng-r f-example 0 1 #:epsabs 1e-1 #:epsrel 0.0 ))))
   
   (test-case
    "qng-r division by zero"
    (check-exn     
     (regexp "qng-r: failed to reach the specified tolerance f-errmsg = /: division by zero x = 0.0")
     (λ () (qng-r (λ (x) (if (positive? x) x (/ 1 0))) -1 1 ))))

   
   (test-case
    "QAG GSL_EBADTOL"
    (t-qag-ebadtol 0 1))
   
   (test-case
    "QAG"
    (t-qag 0.0 1.0 )
    (t-qag 1.0 0.0))
   
   (test-case
    "qag = qag-r"
    (check-equal?
     (rest (qag f1 0 1 #:epsrel 1e-10 #:key 1))
     (qag-r f1 0 1 #:epsrel 1e-10 #:key 1)))

   
   (test-case
    "Test the adaptive integrator with extrapolation QAGS"
    (t-qags 0.0 1.0 )
    (t-qags 1.0 0.0))

   (test-case
    "qags = qags-r"
    (check-equal?
     (rest (qags f1 0 1 #:epsrel 1e-10 ))
     (qags-r f1 0 1 #:epsrel 1e-10 )))
   

   (test-case
    "QAGP"
    (t-qagp (list 0.0 1.0 (sqrt 2.0) 3.0)))

   (test-case
    "QAGP duplicate and non-sorted"
    (check-equal?
     (qagp f454 '(4 5 1.0 1 3)  #:epsrel 1e-3)
     (qagp f454 '(1 3 4 5)     #:epsrel 1e-3)))

   (test-case
    "qagp = qagp-r"
    (check-equal?
     (rest (qagp f454 '( 4 5 1.0 1 3)  #:epsrel 1e-3))
     (qagp-r f454 '( 4 5 1.0 1 3)  #:epsrel 1e-3)))   
     
   
   (test-case
    "QAGI"
    (t-qagi ))

   (test-case
    "QAGIU"
    (t-qagiu 0.0 ))

   (test-case
    "Test infinite range integral myfn2 using an absolute error bound"   
    (t-qagil 1.0 ))        

   (test-case "Test cauchy integration using a relative error bound"
              (t-qawc -1.0  5.0 0)
              (t-qawc 5.0  -1.0 0))

   (test-case
    "Test the adaptive integrator with extrapolation CQUAD"
    (t-cquad 0.0 1.0 )
    (t-cquad 1.0 0.0))

   (test-case
    "Test Romberg integral"
    (t-romberg 0.0 (/ pi 2))
    (t-romberg (/ pi 2) 0.0))))


; failing test at https://pkgs.racket-lang.org/ because gnu gsl is missing.

(for-each (lambda (x) (run-tests i-tests)) (range 1))

