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

(define (calculate-edges-weights lst full-lst result)
  (if (null? lst)
      (reverse result)
      (begin (let ((calc (car (calculate-cover (list (car lst)) full-lst (length full-lst) 0))))
               (if (= calc 0)
                   (if (null? result)
                       (calculate-edges-weights (cdr lst) full-lst
                                                (cons 1000000.0 result))
                       (calculate-edges-weights (cdr lst) full-lst
                                                (cons (+ 1000000.0 (car result)) result)))
                   (if (null? result)
                       (calculate-edges-weights (cdr lst) full-lst
                                                (cons (/ 1 calc) result))
                       (calculate-edges-weights (cdr lst) full-lst
                                                (cons (+ (/ 1 calc) (car result)) result))))))))

(define (get-edge-weight prev elem lst weights)
  (if (null? (cdr lst))
      (- (car weights) prev)
      (if (and
           (equal? (car elem) (car lst))
           (equal? (cadr elem) (cadr lst)))
          (- (car weights) prev)
          (get-edge-weight (car weights) elem (cdr lst) (cdr weights)))))
                              

(define (calculate-cover individ lst lst-size result)
  (cond ((null? lst)
         (list (- lst-size result) individ))
        ((edge-is-covered? individ (car lst))
         (calculate-cover individ (cdr lst) lst-size (+ result 1)))
        (else
         (calculate-cover individ (cdr lst) lst-size result))))

(define (make-one-individ lst chromo-size result)
  (if (= chromo-size 0)
      result
      (make-one-individ lst (- chromo-size 1) (cons (pick-random lst) result))))

(define (generate-k-individs lst chromo-size k result)
  (if (or
        (= k 0)
        (= chromo-size 0))
      result
      (generate-k-individs lst chromo-size (- k 1) (cons (make-one-individ lst chromo-size '()) result)))) 

(define (genetic lst weights individs-count chromo-count result)
  (let ((first-population (map
                           (lambda (x) (calculate-cover x lst (length lst) 0))
                           (generate-k-individs lst chromo-count individs-count '()))))
    (let ((answer (genetic-driver-loop first-population lst weights 50)))
      (if (not (car answer))
          (car result)
          (genetic lst weights individs-count (- chromo-count 1) (cons answer result))))))
             
(define (genetic-driver-loop population lst weights population-count)
  (let ((possible-answers (filter
                           (lambda (x) (= 0 (car x)))
                           population)))
    (cond ((not (null? possible-answers))
           (list #t (length (cadr (car possible-answers))) (cadr (car possible-answers)) ))
          ((or
            (= population-count 0)
            (null? population))
           (list #f))
          (else
           (genetic-driver-loop
            (next-generation population lst weights)
            lst
            weights
            (- population-count 1))))))

(define (next-generation population lst weights)
  (let ((calc-popul (map
                     (lambda (x) (cons (/ 1 (car x)) (cdr x)))
                     population)))
    (map (lambda (x y)
           (crossingover (car (cdr x)) (car (cdr y)) lst weights))
         (get-parents calc-popul (length calc-popul) '() lst)
         (get-parents calc-popul (length calc-popul) '() lst))))

(define (get-parents population size result lst)
  (if (= size 0)
      result
      (let ((pick (calculate-cover (pick-random-by-weight population) lst (length lst) 0)))
        (get-parents population
                     (- size 1)
                     (cons (cons (/ 1 (car pick)) (cdr pick)) result)
                     lst))))
                   
(define (crossingover ind1 ind2 lst weights)
  (find-min (map
             (lambda (x) (calculate-cover x lst (length lst) 0))
             (merge-many ind1 ind2 lst weights 3 '()))))

(define (find-min lst)
  (let ((y (car lst)))
    (if (null? (cdr lst))
        y
        (let ((m (find-min (cdr lst))))
          (if (< (car m) (car y))
              m
              y)))))
   
(define (merge ind1 ind2 lst weights)
  #|(display "ind1:\n")
  (print ind1)|#
  (map (lambda (x y)
         (let ((rnd-elem (pick-random-by-weight2 lst weights)))
           #|(display "heeeey\nx ")
           (print x)
           (display "\ny ")
           (print y)
           (display "\nrnd-elem ")
           (print rnd-elem)|#
           (let ((possible-lst (list
                         (list (get-edge-weight 0 x lst weights) x)
                         (list (get-edge-weight 0 y lst weights) y)
                         (list (get-edge-weight 0 rnd-elem lst weights) rnd-elem))))
             (pick-random-by-weight possible-lst)))) ind1 ind2))

(define (merge-many ind1 ind2 lst weights size result)
  (if (= size 0)
      result
      (merge-many ind1 ind2 lst weights (- size 1) (cons (merge ind1 ind2 lst weights) result)))) 
     
#|Функция sum-weight.
Параметры: список, где в каждом вложенном списке первый элемент - вес; накопитель результата.
Результат: сумма весов всего списка|#
(define (sum-weight lst result)
  (if (null? lst)
      result
      (sum-weight (cdr lst) (+ (car (car lst)) result))))    

#|Функция pick-random-by-weight.
Параметры: сумма весов всего списка, сам список
Результат: случайный(с учетом веса) элемент списка|#
(define (pick-random-by-weight lst)
  (define (get-weights lst result)
    (if (null? lst)
        (reverse result)
        (if (null? result)
            (get-weights (cdr lst) (cons (car (car lst)) result))
            (get-weights (cdr lst) (cons (+ (car (car lst)) (car result)) result)))))
  (define (get-elems lst result)
    (if (null? lst)
        (reverse result)
        (get-elems (cdr lst) (cons (cadr (car lst)) result))))
  (let ((elems (get-elems lst '()))
        (weights (get-weights lst '())))
    ;(print elems)
    ;(newline)
    (let ((elem (pick-random-by-weight2 elems weights)))
      elem)))
      

(define (pick-random-by-weight2 lst weights)
  (define (loop-search number lst weights prev-sum)
    (cond ((or
            (null? (cdr lst))
            (null? (cdr weights)))
           (car lst))
          ((and (> number prev-sum) (< number (- (car weights) 0.001)))
           (car lst))
          (else (loop-search number (cdr lst) (cdr weights) (car weights))))) 
  (let ((sum (list-ref weights (- (length weights) 1))))
    (let ((rnd (/ (random (truncate (* sum 100000))) 100000.0)))
      (loop-search rnd lst weights 0))))

(define (ask-graph)
  (print '(Please enter graph))
  (newline)
  (let ((graph (read))
        (k (read)))
    (let ((result (genetic graph k k (list #f))))
      (print result))))
    
;(ask-graph)

(require racket/file)
(define (with-files)
  (define (call-genetic question)
    (genetic (car question) (calculate-edges-weights (car question) (car question) '()) 40 (cadr question) (list #f)))
  (define (progress out lst size)
    (if (null? lst)
        (display "all tests have done!\n")
        (begin
          (display "working on ")
          (display (- size (length (cdr lst))))
          (display " of ")
          (display size)
          (newline)
          (display (call-genetic (car lst)) out)
          (display "\n" out)
          (progress out (cdr lst) size))))
    
  (let ((cases (file->list "test.txt"))
        (out (open-output-file "genetic-algorithm-result.txt" #:exists 'truncate)))
    (progress out cases (length cases))
    (close-output-port out)))

(with-files)
   




