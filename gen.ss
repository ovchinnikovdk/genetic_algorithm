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

(define (calculate-cover individ lst lst-size result)
  (cond ((null? lst)
         (list (- lst-size result) individ))
        ((edge-is-covered? individ (car lst))
         (calculate-cover individ (cdr lst) lst-size (+ result 1)))
        (else
         (calculate-cover individ (cdr lst) lst-size result))))

(define (make-one-individ lst k result)
  (if (= k 0)
      result
      (make-one-individ lst (- k 1) (cons (pick-random lst) result))))

(define (generate-k-individs lst chromo-size k result)
  (if (= k 0)
      result
      (generate-k-individs lst chromo-size (- k 1) (cons (make-one-individ lst chromo-size '()) result)))) 

(define (genetic lst k result)
  (let ((first-population (map
                           (lambda (x) (calculate-cover x lst (length lst) 0))
                           (generate-k-individs lst k k '()))))
    (print '(here must be genetic-driver-loop))))

(define (genetic-driver-loop population lst k population-count)
  (let ((possible-answers (filter
                           (lambda (x) (= 0 (car x)))
                           population)))
    (cond ((not (null? possible-answers))
           (list #t (car possible-answers)))
          ((= population-count 0)
           (list #f))
          (else
           (genetic-driver-loop
            (next-generation population lst k)
            lst
            k
            (- population-count 1))))))

(define (next-generation population lst k)
  (#f))

(define (ask-graph)
  (print '(Please enter graph))
  (newline)
  (let ((graph (read))
        (k (read)))
    (let ((first (generate-k-individs graph k k '())))
      (print first)
      (newline)
      (print (calculate-cover (car first) graph (length graph) 0)))))
    
    
  