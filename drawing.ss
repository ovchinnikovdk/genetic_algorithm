#lang scheme
(require racket/gui/base)
(require racket/math)
(require racket/draw)
(require plot)

(define no-pen (new pen% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))


#|
Инициализация переменных для создания окнаы
|#

(define frame (new frame%
                     [label "Genetic algorithm"]
                     [width 800]
                     [height 640]
                     [x 300]
                     [y 100]
                     [alignment '(left center)]))
(define canvas1 (new canvas% [parent frame]
                             [min-width 800]
                             [min-height 600]
                             [paint-callback
                              (lambda (canvas dc)
                                (set! DC dc)
                                (draw-graph dc))]))
(define (get-canvas)
  canvas1)
(define subframe (new horizontal-panel%
                      [parent frame]  
                      [alignment '(left center)]))
#|(define btn (new button% [parent subframe]
                 [label "Выключить обновление графа"]
                 [callback (lambda (button event)
                             (if (not stop-refresh)
                                 (begin
                                   (set! stop-refresh #t)
                                   (send canvas1 resume-flush)
                                   (send button set-label "Выключить обновление графа"))
                                 (begin
                                   (set! stop-refresh #f)
                                   (send canvas1 suspend-flush)
                                   (send button set-label "Включить обновление графа"))))]))
(define (get-btn) btn) |#
(define msg (new message% [parent subframe]
                 [label "genetic algorithm"]
                 [auto-resize #t]))
(define (get-msg)
  msg)
  
;
;       SHOW!
;
(define (show-frame)
  (send frame show #t))
;
;
;

;/////////////////////////
;       GLOBAL NAMES
;   ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ

#|
VERTICES - переменная, хранит все вершины графа и его координаты.
|#
(define VERTICES (list "one" "two" "three" "four"
                                 "five" "six" "seven" "eight"))
(define (set-VERTICES value)
  (set! VERTICES value))
(define (get-VERTICES) VERTICES)

#|
EDGES - переменная, список всех ребер графа.
|#
(define EDGES (list (list "one" "six") (list "two" "three") (list "one" "five") (list "two" "seven")(list "seven" "eight")))
(define (set-EDGES value)
  (set! EDGES value))
(define (get-EDGES) EDGES)


#|
DC - переменная - Drawing-context, хранит контекст для прорисовки графа на холсте. 
|#
(define DC null)
(define (get-DC)
  DC)

#|
stop-refresh - переменная, (boolean), - показатель обновления экрана. 
|#
(define stop-refresh #t)
;  //////////////////////


#|
Функция draw-graph.
Рисует сначала ребра, потом вершины.
|#
(define (draw-graph dc)
  (send dc set-smoothing 'aligned)
  (draw-edges dc EDGES "LightBlue" 2)
  (draw-vertices dc VERTICES))

#|
get-vertices.
Функция достает из списка ребер все имеющиеся в нем вершины.ы
|#
(define (get-vertices lst)
  (define (in-list? elem lst)
    (= 0 (length (filter (lambda (x) (equal? elem x))
                         lst))))
  (define (search-loop lst result)
    (if (null? lst)
        result
        (cond ((in-list? (car (car lst)) result)
               (if (in-list? (cadr (car lst)) result)
                   (search-loop (cdr lst) (cons (cadr (car lst)) (cons (car (car lst)) result)))
                   (search-loop (cdr lst) (cons (car (car lst)) result))))
              ((in-list? (cadr (car lst)) result)
               (search-loop (cdr lst) (cons (cadr (car lst)) result)))
              (else (search-loop (cdr lst) result)))))
  (search-loop lst '()))


#|
get-one-coordinate.
Функция достает координаты для определенной вершины.
Параметры:
elem - вершина,
coords - список координат в виде (x y "string_name")
|#
(define (get-one-coordinate elem coords)
  (if (null? coords)
      (list 0 0 "not found")
      (if (equal? elem (caddr (car coords)))
          (car coords)
          (get-one-coordinate elem (cdr coords)))))


#|
draw-edges
Функция рисует ребра используя сплайны.
параметры:
dc- drawing context
edges - list
color - string
size - integer
|#
(define (draw-edges dc edges color size)
  (send dc set-pen color size 'solid)
  (define (get-third-point elem1 elem2)
    (define (make-projection x1 y1 x2 y2)
      (list (- x1 x2) (- y2 y1)))
    (let ((x1 (+ 5 (car elem1)))
           (y1 (+ 5 (cadr elem1)))
           (x2 (+ 5 (car elem2)))
           (y2 (+ 5 (cadr elem2))))
       (let ((proj (make-projection x1 y1 x2 y2)))
         (list (+ (/ (+ x1 x2) 2) (/ (car proj) 8))
               (+ (/ (+ y1 y2) 2) (/ (cadr proj) 8))))))  
  (define (loop dc edges)
    (if (null? edges)
        null
        (begin
          (let ((elem1 (get-one-coordinate (car (car edges)) VERTICES))
                (elem2 (get-one-coordinate (cadr (car edges)) VERTICES)))
            (let ((third (get-third-point elem1 elem2)))
              (send dc draw-spline (+ 5 (car elem1)) (+ 5 (cadr elem1))
                    (car third) (cadr third)
                    (+ 5 (car elem2)) (+ 5 (cadr elem2))))
            (loop dc (cdr edges))))))
  (loop dc edges))


#|
get-coordinates.
Функция располагает список из вершин на плоскости.(задает координаты)
Параметры:
rad - (integer), - радиус круга, по которому будут располагаться вершины
angle - (real) угол в радианах.
cen-x, cen-y - (integer, integer) - координаты центра,
lst - (list) - список вершин.
result (list) - накопитель результата.ы
|#
(define (get-coordinates rad angle cen-x cen-y lst result)
    (if (not (null? lst))
        (if (null? result)
            (let ((res (list (- cen-x rad) cen-y (car lst))))
              (get-coordinates rad angle cen-x cen-y (cdr lst) (cons res result)))
            (let ((cur-angle (* (length result) angle))
                  (prev-x (car (list-ref result (- (length result) 1))))
                  (prev-y (cadr (list-ref result (- (length result) 1)))))
              (let ((res (list (+ cen-x (- (* (- prev-x cen-x) (cos cur-angle)) (* (- prev-y cen-y) (sin cur-angle))))
                               (+ cen-y (+ (* (- prev-x cen-x) (sin cur-angle)) (* (- prev-y cen-y) (cos cur-angle))))
                               (car lst))))
              (get-coordinates rad angle cen-x cen-y (cdr lst) (cons res result)))))
            result))


(define (re-calculate-coordinates vertices result)
  (define (find-all-neighbours elem)
    (filter (lambda (x) (not (null? x))) (map (lambda (x)
                                                (if (equal? (caddr elem) (car x))
                                                    (let ((coord (get-one-coordinate (cadr x) VERTICES)))
                                                      (list
                                                       (- (car coord) (car elem))
                                                       (- (cadr coord) (cadr elem))))
                                                     (if (equal? (caddr elem) (cadr x))
                                                         (let ((coord (get-one-coordinate (car x) VERTICES)))
                                                           (list
                                                            (- (car coord) (car elem))
                                                            (- (cadr coord) (cadr elem))))
                                                         null)))
                                                EDGES)))
  (define (calc-sum lst size sum1 sum2)
    (if (null? lst)
        (list (/ sum1 size) (/ sum2 size))
        (calc-sum (cdr lst) size (+ sum1 (car (car lst))) (+ sum2 (cadr (car lst)))))) 
  (if (null? vertices)
      (set! VERTICES result)
      (let ((neighbours (find-all-neighbours (car vertices))))
        (let ((sum (calc-sum neighbours (length neighbours) 0 0)))
          (re-calculate-coordinates (cdr vertices)
                                    (cons (list
                                       (+ (/ (car sum) 3) (car (car vertices)))
                                       (+ (/ (cadr sum) 3) (cadr (car vertices)))
                                       (caddr (car vertices))) result))))))
  
#|
draw-vetices.
Функция рисует вершины графа в соответствующих координатах на холсте.
Параметры:
dc - (dc%) - Drawing-context  - контекст холста.
coords - (list) - список координат вершин.
|#
(define (draw-vertices dc coords)
  (define (draw-one-vertex dc lst)
    (if (null? lst)
        '()
        (begin
          (let ((vertex (car lst)))
            (send dc draw-ellipse (car vertex) (cadr vertex) 10 10)
            (send dc draw-text (caddr vertex) (+ 20 (car vertex)) (+ 3 (cadr vertex)))
            (draw-one-vertex dc (cdr lst))))))
  (send dc set-pen "red" 1 'solid)
  (send dc set-brush "blue" 'solid)
  (draw-one-vertex dc coords))


#|
lst->lst-string.
Функция переводит название вершины в строковый формат.
(Нужно для прорисовки названия вершины)
Параметры:
lst - (list) - список ребер.
|#
(define (lst->lst-string lst)
  (map (lambda (x)
         (list (cond ((string? (car x))
                      (car x))
                     ((number? (car x))
                      (number->string (car x)))
                     ((symbol? (car x))
                      (symbol->string (car x)))
                     (else "uknown format"))
               (cond ((string? (cadr x))
                      (cadr x))
                     ((number? (cadr x))
                      (number->string (cadr x)))
                     ((symbol? (cadr x))
                      (symbol->string (cadr x)))
                     (else "uknown format")))) lst))

#|
draw-graphics.
Функция рисует графики в файлы.
Параметры:
lst - (list) - статистика, накопленная в ходе работы генетического алгоритма.
file-name - (string) - имя файла, для сохранения.
|#
(define (draw-graphics lst file-name)
  (define (summ lst lst-size res-sum1 res-sum2)
    (if (null? lst)
        (ivl (/ res-sum1 lst-size) (/ res-sum2 lst-size))
        (summ (cdr lst) lst-size (+ (cadr (car lst)) res-sum1) (+ (caddr (car lst)) res-sum2))))
        
  (define (loop count lst result)
    (let ((lst1 (filter (lambda (x) (or (= count (car x)) (= (+ count 1) (car x)))) lst)))
      (if (= (length lst1) 0)
          (reverse result)
          (loop (+ count 2) lst (cons (vector count (summ lst1 (length lst1) 0 0)) result)))))
  (plot-width 2000)
  (plot-height 600)
  (plot-file (discrete-histogram
        (loop 0 lst '())
         #:label "Best and worst parametres on each iterations"
         #:x-min 0
         )
             #:x-label "Количество итераций"
             #:y-label "Минимальные и максимальные параметры"
             #:title "График среднего количества покрывающихся ребер на каждом этапе"
             file-name 'png))

(define (draw-graphics-time lst file-name)
  (plot-width 2000)
  (plot-height 600)
  (plot-file (discrete-histogram
        lst
         #:label "Время выполнения алгоритма"
         #:x-min 0
         )
             #:x-label "Количество ребер"
             #:y-label "Время выполнения алгоритма"
             #:title "График времени выполнения"
             file-name 'png))

(set! VERTICES (get-vertices EDGES))
(set! VERTICES (get-coordinates 280 (/ (* 2 pi) (length VERTICES)) 400 290 VERTICES '()))


(provide set-VERTICES)
(provide set-EDGES)
(provide get-VERTICES)
(provide get-EDGES)
;(provide get-btn)
(provide stop-refresh)
(provide get-canvas)
(provide DC)
(provide get-DC)
(provide show-frame)
(provide draw-vertices)
(provide get-coordinates)
(provide get-vertices)
(provide draw-edges)
(provide draw-graph)
(provide draw-graphics)
(provide draw-graphics-time)
(provide get-msg)
(provide lst->lst-string)
(provide re-calculate-coordinates)
