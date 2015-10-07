#lang scheme/base

(define (edge-is-covered? lst edge)
  (if (null? lst)
      #f
      (not (null? (filter
               (lambda (x)
                 (or (equal? (car edge) (car x))
                     (equal? (cadr edge) (cadr x))
                     (equal? (car edge) (cadr x))
                     (equal? (cadr edge) (car x))))
               lst)))))

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (my-member? lst v)
  (not (null? (filter (lambda (x)
                        (and
                         (equal? (car x) (car v))
                         (equal? (cadr x) (cadr v))))
                      lst))))

(define (calculate-cover chromosome lst lst-size result)
  (cond ((null? lst)
         (- lst-size result))
        ((edge-is-covered? chromosome (car lst))
         (calculate-cover chromosome (cdr lst) lst-size (+ result 1)))
        (else
         (calculate-cover chromosome (cdr lst) lst-size result))))

(define (make-first-population lst k result)
  (if (= k 0)
      result
      (make-first-population lst (- k 1) (cons (pick-random lst) result))))


(define (ask-graph)
  (print '(Please enter graph))
  (newline)
  (let ((graph (read))
        (k (read)))
    (let ((first (make-first-population graph k '())))
      (print first)
      (newline)
      (print (calculate-cover first graph (length graph) 0)))))
    
    
  