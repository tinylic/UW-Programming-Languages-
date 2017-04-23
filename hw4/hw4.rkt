
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map slist astr)
  (map (lambda (str) (string-append str astr)) slist))

(define (list-nth-mod xs n)
  (let ([nn (remainder n (length xs))])
      (cond [(< nn 0) (error "list-nth-mod: negative number")]
            [(null? xs) (error "list-nth-mod: empty list")]
            [(= nn 0) (car xs)]
            [#t (list-nth-mod (cdr xs) (- nn 1))])))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons x (
                         lambda () (if (= 0 (remainder (+ 1 x) 5))
                                                 (f (- 0 (+ x 1)))
                                                 (if (< x 0)
                                                     (f (+ 1 (- 0 x)))
                                                     (f (+ 1 x)))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (if (string=? x "dan.jpg") (f "dog.jpg") (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero stream)
  (letrec ([f (lambda (stream)
                (let ([pr (stream)])
                  (cons (cons 0 (car pr)) (lambda() (f (cdr pr))))))])
    (lambda () (f stream))))

(define (loop-list xs)
  (letrec ([f (lambda (cur orig)
                (if (null? cur)
                    (f orig orig)
                    (cons (car cur) (lambda() (f (cdr cur) orig)))))])
    (lambda () (f xs xs))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (s1 s2)
                (let ([pr1 (s1)]
                      [pr2 (s2)])
                  (cons (cons (car pr1) (car pr2)) (lambda() (f (cdr pr1) (cdr pr2))))))])
    (lambda() (f (loop-list xs) (loop-list ys)))))

(define (vector-assoc n v)
  (letrec ([f (lambda (n i v)
                 (cond [(= (vector-length v) i) #f]
                        [(not (pair? (vector-ref v i))) (f n (+ 1 i) v)]
                        [#t (let ([pr (vector-ref v i)])
                              (if (equal? (car pr) n)
                                  pr
                                  (f n (+ 1 i) v)))]))])
           (f n 0 v)))


(define (cached-assoc xs n)
  (letrec([memo (make-vector n)]
          [i 0]
          [pos (lambda (i) (remainder i (length memo)))]
          [f (lambda (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans 
                     ans
                     (let ([new-ans (assoc v xs)])
                       (begin 
                         (vector-set! memo (pos i) new-ans)
                         (set! i (+ 1 i))
                         new-ans)))))])                         
    f))                    
           
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([fixed_e1 e1])
       (letrec ([loop (lambda (it)
                        (if (< it fixed_e1)
                            e2
                            #t))])
         (loop e2)))]))
  
      
          
      