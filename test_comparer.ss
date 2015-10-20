#lang scheme/base

(require racket/file)

(define (compare)
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
  (define (covered? cover graph)
    (not (null? (filter (lambda (x) (edge-is-covered? cover x)) graph))))
  (define (statistics lst)
    (define (calculate-procent lst count)
      (if (null? lst)
          count
          (if (car lst)
              (calculate-procent (cdr lst) (+ count 1))
              (calculate-procent (cdr lst) count))))
    (newline)
    (let ((proc (calculate-procent lst 0)))
      (print proc)
      (display "/")
      (print (length lst))
      (display " ")
      (display "true answers")
      (newline)
      (print (truncate (* (/ proc (length lst)) 100.0)))
      (display "%\n")
      (print lst)))
  (let ((tests (file->list "test.txt"))
        (true-answers (file->list "test-results.txt"))
        (answers (file->list "genetic-algorithm-result.txt")))
    (statistics (map (lambda (test true-answer answer)
           (if (list? true-answer)
               (if (not (list? answer))
                   #f
                   (if (not (= (cadr true-answer) (cadr answer)))
                       #f
                       (if (covered? (caddr answer) (car test))
                           #t
                           #f)))
               (if (list? answer)
                   #f
                   #t)))
         tests
         true-answers
         answers))))