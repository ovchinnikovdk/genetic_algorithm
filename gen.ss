#lang scheme/base
#|
Генетический алгоритм для построения минимального покрывающего множества ребер для графов.
Автор: Дмитрий Овчинников (2015 г)
В качестве "особей" взяты потенциальные минимальные доминирующие множества.
В качестве "хоромосомы" берется одно ребро.
Скрещивание хромосом см. функцию сrossingover.
Время работы над полным графом с 40 ребрами ~ 5-7 сек.
Параметры алгоритма, которые нужно модифицировать для достижения
оптимального (качество/время) результата:
1) Количество итераций (поколений)
   - параметр к genetic-driver-loop пол названием population-count.
(вызывается в функции genetic)
2) Количество потомков у каждых двух родителей
  - параметр к merge-many - "size", вызывается в crossingover.
3) Количество особей в популяции параметр "individs-count" в функции genetic 
|#



#|
Функция edge-is-covered?.
Вход: lst - мно-во ребер, edge - ребро.
Выход: #t or #f, в зависимости от того, покрывается ли ребро множеством.
(Т.е. является ли смежным с каким-нибудь ребром или само входит в множество.)
|#
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

#|
Функция pick-random.
Выдает случайный элемент из списка lst.
|#
(define (pick-random lst)
  (list-ref lst (random (length lst))))

#|
Функция calculate-edges-weights.
Вход: lst - мно-во ребер, для которого нужно посчитать веса,
      full-lst - все, множество ребер.
      result  - накопитель результата.
Выход: Список размером с lst, в котором хранятся "веса" для каждого ребра.
("вес" = количество смежных с этим ребром ребер)
|#
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

#|
Функция get-edge-weight.
Параметры:
prev - вес предыдущего элемента (для первого 0)
elem - элемент, для которого нужно найти вес
lst - список элементов
weights - соответствующий список весов.
Результат: Возвращает вес определенного элемента, если он имеется в lst.
(примечание: размер lst должен совпадать с weights.)
|#
(define (get-edge-weight prev elem lst weights)
  (if (null? (cdr lst))
      (- (car weights) prev)
      (if (and
           (equal? (car elem) (car lst))
           (equal? (cadr elem) (cadr lst)))
          (- (car weights) prev)
          (get-edge-weight (car weights) elem (cdr lst) (cdr weights)))))
                              
#|
calculate-cover.
Параметры:
individ - особь, для которой нужно посчитать, насколько она покрывает граф,
lst - граф (список ребер),
lst-size - размер графа.
result - накопитель результата.
Результат: Число - насколько особь покрывает граф.
(Прим.: Результатом является не число смежных ребер,
а количество всех ребер "минус" количество смежных с этим множеством ребер.)ы
|#
(define (calculate-cover individ lst lst-size result)
  (cond ((null? lst)
         (list (- lst-size result) individ))
        ((edge-is-covered? individ (car lst))
         (calculate-cover individ (cdr lst) lst-size (+ result 1)))
        (else
         (calculate-cover individ (cdr lst) lst-size result))))

#|
make-one-individ.
lst - множество ребер графа,
chromo-size - количество хромосом в одной особи,
result - накопитель результата.
Результат: Создается особь, с количеством хромосом, равным chromo-size
|#
(define (make-one-individ lst chromo-size result)
  (if (= chromo-size 0)
      result
      (make-one-individ lst (- chromo-size 1) (cons (pick-random lst) result))))

#|
generate-k-individs.
Параметры - те же самые, что и у функции выше + k - количество особей.
Результат: Генерируется случайная популяция из k особей с количеством хромосом - chromo-size
|#
(define (generate-k-individs lst chromo-size k result)
  (if (or
        (= k 0)
        (= chromo-size 0))
      result
      (generate-k-individs lst chromo-size (- k 1) (cons (make-one-individ lst chromo-size '()) result)))) 

#|
GENETIC. - Основная функция программы, реализует генетический алгоритм для данной задачи.
Параметры:
individs-count - количество особей в популяции,
chromo-count - количество хромосом у особи.
Результат: Ответ на поставленный в задаче вопрос -
"Имеется ли в данном графе lst, минимальное доминирующее множество ребер размером
не больше k (= chromo-count)?"
Если #t ("ДА"), то каков размер и само множество (результат в одном списке вида (#t k (множество)) )
|#
(define (genetic lst weights individs-count chromo-count result)
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
  (let ((first-population (map
                           (lambda (x) (calculate-cover x lst (length lst) 0))
                           (generate-k-individs lst chromo-count individs-count '()))))
    (let ((answer (genetic-driver-loop first-population lst weights 120)))
      (if (not (car answer))
          (car result)
          (genetic lst weights individs-count (- chromo-count 1) (cons answer result))))))
             

#|
next-generation.
Параметры:
population - текущая популяция.
lst - граф,
wieghts - веса ребер графа.
Результат: Генерация нового поколения. 
|#
(define (next-generation population lst weights)
  (let ((calc-popul (map
                     (lambda (x) (cons (/ 1 (car x)) (cdr x)))
                     population)))
    (map (lambda (x y)
           (crossingover (car (cdr x)) (car (cdr y)) lst weights))
         (get-parents calc-popul (length calc-popul) '() lst)
         (get-parents calc-popul (length calc-popul) '() lst))))
#|
get-parents.
Результат:
Выборка родителей для следующего поколения (используется pick-random-by-weight)
|#
(define (get-parents population size result lst)
  (if (= size 0)
      result
      (let ((pick (calculate-cover (pick-random-by-weight population) lst (length lst) 0)))
        (get-parents population
                     (- size 1)
                     (cons (cons (/ 1 (car pick)) (cdr pick)) result)
                     lst))))
#|
crossingover.
Скрещивание происходит одновременно с мутацией,
т.е. новое ребро может браться как от родителей, так и просто с графа,
это выбирается случайно, с учетом весов этих ребер.
|#            
(define (crossingover ind1 ind2 lst weights)
  (define (merge ind1 ind2 lst weights)
  (map (lambda (x y)
         (let ((rnd-elem (pick-random-by-weight2 lst weights)))
           (let ((possible-lst (list
                         (list (get-edge-weight 0 x lst weights) x)
                         (list (get-edge-weight 0 y lst weights) y)
                         (list (get-edge-weight 0 rnd-elem lst weights) rnd-elem))))
             (pick-random-by-weight possible-lst)))) ind1 ind2))

(define (merge-many ind1 ind2 lst weights size result)
  (if (= size 0)
      result
      (merge-many ind1 ind2 lst weights (- size 1) (cons (merge ind1 ind2 lst weights) result)))) 
  (find-min (map
             (lambda (x) (calculate-cover x lst (length lst) 0))
             (merge-many ind1 ind2 lst weights 2 '()))))

#|
find-min.
Параметры: lst - список вида ( (number1 elem), (number2 elem), (number3 elem) ...)
Результат: Возрвращается элемент с минимальным number*
|#
(define (find-min lst)
  (let ((y (car lst)))
    (if (null? (cdr lst))
        y
        (let ((m (find-min (cdr lst))))
          (if (< (car m) (car y))
              m
              y)))))

     
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
    (let ((elem (pick-random-by-weight2 elems weights)))
      elem)))
      
#|
Модифицированная функция выбора случайного элемента с учетом веса
(для случаев, когда веса находятся в отдельном списке)
|#
(define (pick-random-by-weight2 lst weights)
  (define (bin-search number lst weights a b)
    (let ((mid (+ a (truncate (/ (- b a) 2)))))
      (cond ((or
             (= a b)
             (> a b))
             (list-ref lst b))
            ((> (list-ref weights mid) number)
             (bin-search number lst weights a mid))
            (else (bin-search number lst weights (+ mid 1) b)))))
  (let ((sum (list-ref weights (- (length weights) 1))))
    (let ((rnd (/ (random (truncate (* sum 100000))) 100000.0)))
      (bin-search rnd lst weights 0 (- (length lst) 1)))))

#|
Функция для работы алгоритма через консоль.
|#
(define (ask-graph)
  (print '(Please enter graph))
  (newline)
  (let ((graph (read))
        (k (read)))
    (let ((result (genetic graph k k (list #f))))
      (print result))))
    
;(ask-graph)
#|
Функция для работы алгоритма через файлы (удобнее работать с тестами)
|#
(require racket/file)
(define (with-files)
  (define (call-genetic question)
    (genetic (car question)
             (calculate-edges-weights (car question) (car question) '())
             ;(ceiling (* (length (car question)) 0.8))
             40
             (cadr question)
             (list #f)))
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
   




