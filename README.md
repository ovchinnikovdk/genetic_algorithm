# Genetic algorithm for the minimum edge cover set
Генетический алгоритм для построения минимального покрывающего множества ребер для графов. <br/>
Автор: Дмитрий Овчинников (2015 г)<br/>
В качестве "особей" взяты потенциальные минимальные доминирующие множества.<br/>
В качестве "хоромосомы" берется одно ребро.<br/>
Скрещивание хромосом см. функцию сrossingover.<br/>
Время работы над полным графом с 40 ребрами ~ 5-7 сек.<br/>
Параметры алгоритма, которые нужно модифицировать для достижения<br/>
оптимального (качество/время) результата:<br/>
1) Количество итераций (поколений)<br/>
   - параметр к genetic-driver-loop под названием population-count.<br/>
(вызывается в функции genetic)<br/>
2) Количество потомков у каждых двух родителей<br/>
  - параметр к merge-many - "size", вызывается в crossingover.<br/>
3) Количество особей в популяции параметр "individs-count" в функции genetic <br/><br/>

Genetic algorithm, which solves the problem of searching minimum edge cover set. <br/>
Author: Dmitriy Ovchinnikov<br/>
Language: Scheme/base, Racket.<br/>
Also there are test generator, test comparer and set of tests. <br/>
As a "individ" taken the potential minimum dominating sets. <br/>
As a "chromosome" taken one edge. <br/>
Crossing chromosome see. "crossingover" function. <br/>
While working on a complete graph with 40 edges ~ 5-7 sec. <br/>
Algorithm parameters that need to be modified to achieve <br/>
the best (quality / time) result: <br/>
1) The number of iterations (generations) <br/>
   - Option to "genetic-driver-loop"  called "population-count". <br/>
(called in a function "genetic") <br/>
2) Number of offspring from each of two parents <br/>
  - Option to merge-many - "size", called in crossingover. <br/>
3) The number of individuals in a population setting "individs-count" as a function of genetic <br/> <br/>
