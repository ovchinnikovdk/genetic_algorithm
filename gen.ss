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
  (if (or
        (= k 0)
        (= chromo-size 0))
      result
      (generate-k-individs lst chromo-size (- k 1) (cons (make-one-individ lst chromo-size '()) result)))) 

(define (genetic lst individs-count chromo-count result)
  (let ((first-population (map
                           (lambda (x) (calculate-cover x lst (length lst) 0))
                           (generate-k-individs lst chromo-count individs-count '()))))
    (let ((answer (genetic-driver-loop first-population lst 20)))
      (if (not (car answer))
          (car result)
          (genetic lst individs-count (- chromo-count 1) (cons answer result))))))
             
            
          
          

(define (genetic-driver-loop population lst population-count)
  (let ((possible-answers (filter
                           (lambda (x) (= 0 (car x)))
                           population)))
    (cond ((not (null? possible-answers))
           (list #t (car possible-answers)))
          ((or
            (= population-count 0)
            (null? population))
           (list #f))
          (else
           (genetic-driver-loop
            (next-generation population lst)
            lst
            (- population-count 1))))))

(define (next-generation population lst)
  (let ((calc-popul (map
                     (lambda (x) (cons (/ 1 (car x)) (cdr x)))
                     population)))
    (map (lambda (x y)
           (crossingover (car (cdr x)) (car (cdr y)) lst))
         (get-parents calc-popul (length calc-popul) '())
         (get-parents calc-popul (length calc-popul) '()))))

(define (get-parents population size result)
  (if (= size 0)
      result
      (get-parents population
                   (- size 1)
                   (cons (pick-random-by-weight (length population) population) result))))
                   
(define (crossingover ind1 ind2 lst)
  (find-min (map
             (lambda (x) (calculate-cover x lst (length lst) 0))
             (merge-many ind1 ind2 lst 4 '()))))

(define (find-min lst)
  (let ((y (car lst)))
    (if (null? (cdr lst))
        y
        (let ((m (find-min (cdr lst))))
          (if (< (car m) (car y))
              m
              y)))))
   
(define (merge ind1 ind2 lst)
  (map (lambda (x y) (if (= (random 2) 0) x y)) ind1 ind2))

(define (merge-many ind1 ind2 lst size result)
  (if (= size 0)
      result
      (merge-many ind1 ind2 lst (- size 1) (cons (merge ind1 ind2 lst) result)))) 
     
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
(define (pick-random-by-weight sum lst)
  (let ((rnd (/ (random (truncate (* sum 100000))) 100000.0))
        (sumw-lst (sum-weight lst 0.0)))
    (cond ((null? (cdr lst))
           (car lst))
          ((and (> sumw-lst rnd) (< (- sumw-lst (car (car lst)) 0.01) rnd))
           (car lst))
          (else (pick-random-by-weight sum (cdr lst))))))
    
(define (ask-graph)
  (print '(Please enter graph))
  (newline)
  (let ((graph (read))
        (k (read)))
    (let ((result (genetic graph k k (list (list #f)))))
      (if (car result)
          (begin
            (print (car result))
            (newline)
            (print (length (car (cdr (car (cdr result))))))
            (newline)
            (print (car (cdr (car (cdr result))))))
          (print (car result))))))
    
(ask-graph) 