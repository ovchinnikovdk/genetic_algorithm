#lang scheme/base

(require racket/file)

#|
Функция compare
Результат: сравнение правильных результатов тестов из файла test-results.txt
и результата работы генетического алгоритма из файла genetic-algorithm-result.txt
|#

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
          (if (not (list? (car lst)))
              (calculate-procent (cdr lst) (+ count 1))
              (calculate-procent (cdr lst) count))))
    (define (print-lst lst count)
      (cond ((null? lst)
             (display "..."))
            ((list? (car lst))
             (begin
               (display count)
               (display ". ")
               (display (car lst))
               (display "\n")
               (print-lst (cdr lst) (+ count 1))))
            (else (print-lst (cdr lst) (+ count 1)))))
    (newline)
    (let ((proc (calculate-procent lst 0)))
      (print proc)
      (display "/")
      (print (length lst))
      (display " ")
      (display "true answers")
      (newline)
      (print (round (* (/ proc (length lst)) 100.0)))
      (display "%\n")
      (print-lst lst 1)))
  (let ((tests (file->list "test.txt"))
        (true-answers (file->list "test-results.txt"))
        (answers (file->list "genetic-algorithm-result.txt")))
    (statistics (map (lambda (test true-answer answer)
           (if (list? true-answer)
               (if (not (list? answer))
                   (list #f "got" answer "expected " true-answer)
                   (if (and
                        (not (= (cadr true-answer) (cadr answer)))
                        (not (> (cadr true-answer) (cadr answer))))
                         (list #f "got" answer "expected<>" true-answer)
                       (if (covered? (caddr answer) (car test))
                           #t
                           (list #f "not covered"))))
               (if (list? answer)
                   (list #f "got" answer "expected" true-answer)
                   #t)))
         tests
         true-answers
         answers))))
(compare)