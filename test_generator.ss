#lang scheme/base
;(require racket/file)
    
#|
generate-test - Главная функция программы генерации тестов
Параметры: number - число тестов.
Результат: генерирует заданное число тестов для основного задания.
Пояснения: Результаты записываются в файлы
test.txt - сами тесты
test-results.txt - ответы на тесты в том же порядке, в каком находятся в test.txt
На каждом шаге генерации, программа случайно выбирает какой именно будет граф и какого размера,
указываются лишь максимальные границы.
Функции, генерирующие различные виды графов описаны ниже.
|#

(define (generate-tests number)
  (define (pick-random lst)
    (list-ref lst (random (length lst))))
  (define (writing-to-file out1 out2 lst)
    (if (null? lst)
        (print '(generating succesfully done))
        (begin
          (display (car (car lst)) out1)
          (display "\n" out1)
          (display (cadr (car lst)) out2)
          (display "\n" out2)
          (writing-to-file out1 out2 (cdr lst)))))
  (define (loop n result)
    (if (< n 1)
        result
        (loop (- n 1) (cons ((pick-random (list create-full-graph create-stars create-polygon)) 8) result))))
  (let ((out1 (open-output-file "test.txt" #:exists 'truncate))
        (out2 (open-output-file "test-results.txt" #:exists 'truncate)))
    (writing-to-file out1 out2 (loop number '()))
    (close-output-port out1)
    (close-output-port out2)))

(define (create-polygon max)
  (define (polygon-driver-loop start current end result)
    (if (or (= current end) (> current end))
        (cons (list current start) result)
        (polygon-driver-loop start (+ current 1) end (cons (list current (+ current 1)) result))))
  (let ((n (+ (random (- max 2)) 2)))
    (let ((answer (ceiling (/ n 3)))
          (k (random (+ n 1))))
      (if (or (< answer k) (= answer k))
          (list (list (polygon-driver-loop 1 1 n '()) k) (list #t answer))
          (list (list (polygon-driver-loop 1 1 n '()) k) #f)))))


(define (create-stars max)
  (define (stars-driver-loop current n max result)
    (if (or (= current max) (> current max))
        result
        (stars-driver-loop (+ current 1) n max (cons (list (+ (random (- n 1)) 1) current) result))))
  (define (re-calculate-stars current lst n result)
    (define (is-used-vertex lst v result)
      (if (null? lst)
          result
          (is-used-vertex (cdr lst) v
                          (or result
                              (equal? v (car (car lst)))
                              (equal? v (cadr (car lst)))))))
    (if (= current n)
        result
        (if (is-used-vertex lst current #f)
            (re-calculate-stars (+ current 1) lst n (+ result 1))
            (re-calculate-stars (+ current 1) lst n result))))
  (let ((stars (+ (random (- max 3)) 4))
        (k (random max)))
    (let ((lst (stars-driver-loop (+ stars 1) stars (* max 2) '())))
      (let ((answer (re-calculate-stars 1 lst stars 0)))
        (if (not (< k answer))
            (list
             (list lst k)
             (list #t answer))
            (list
             (list lst k)
             #f))))))

(define (create-full-graph max)
  (define (loop-support current start finish result)
    (if (> start finish)
        result
        (loop-support  current (+ start 1) finish (cons (list current start) result))))
  (define (main-loop current size result)
    (if (> current size)
        result
        (main-loop (+ current 1) size (append result (loop-support current (+ current 1) size '())))))
  (let ((k (random max))
        (res (main-loop 1 (+ (random (- max 3)) 3) '())))
    (if (< k 1)
        (list (list res k) #f)
        (list (list res k) (list #t 1)))))
  
