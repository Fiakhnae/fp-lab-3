<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гармаш Дмитро Олегович КВ-13</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.

1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
   конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
   списку. Не допускається використання: деструктивних операцій, циклів, функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Також реалізована функція не має
   бути функціоналом (тобто приймати на вхід функції в якості аргументів).

2. Імперативний варіант реалізації має базуватись на використанні циклів і
   деструктивних функцій (псевдофункцій). Не допускається використання функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Тим не менш, оригінальний список
   цей варіант реалізації також не має змінювати, тому перед виконанням
   деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
   Також реалізована функція не має бути функціоналом (тобто приймати на вхід
   функції в якості аргументів).
   Алгоритм, який необхідно реалізувати, задається варіантом (п. 3.1.1). Зміст і шаблон
   звіту наведені в п. 3.2.
   Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
   мають бути оформленні у вигляді модульних тестів (наприклад, як наведено у п. 2.3).

## Варіант 3

Алгоритм сортування обміном №3 (із запам'ятовуванням місця останньої перестановки) за незменшенням.

## Лістинг функції з використанням конструктивного підходу

```lisp
(defun bubble-step (lst last-swap)
  (cond ((or (null lst) (null (cdr lst))) (values lst last-swap))
        (t (let ((a (car lst))
                 (b (cadr lst)))
             (if (> a b)
                 (let* ((swapped (cons b (cons a (cddr lst)))))
                   (multiple-value-bind (sorted-lst new-last-swap)
                       (bubble-step (cdr swapped) t)
                     (values (cons (car swapped) sorted-lst)
                             (or new-last-swap t))))
                 (multiple-value-bind (sorted-lst new-last-swap)
                     (bubble-step (cdr lst) nil)
                   (values (cons a sorted-lst)
                           (or new-last-swap last-swap))))))))

(defun recursive-bubble-sort (lst)
  (labels ((recursive-sort (lst)
             (multiple-value-bind (new-lst last-swap)
                 (bubble-step lst nil)
               (if last-swap
                   (recursive-sort new-lst)
                   new-lst))))
    (recursive-sort lst)))
```

### Тестові набори

```lisp
(defun test-sort-func ()
  (check-sort-func "Test 1" '(4 3 2 1) '(1 2 3 4))
  (check-sort-func "Test 2" '(1 2 3 4) '(1 2 3 4))
  (check-sort-func "Test 3" '(5 1 3 2 4) '(1 2 3 4 5)))
```

### Тестування

```lisp
Testing sort-func:
passed... Test 1
passed... Test 2
passed... Test 3
```

## Лістинг функції з використанням деструктивного підходу

```lisp
(defun sort-imp (lst)
  (let (R k tmp)
    (setf R (- (length lst) 1))
    (loop while (> R 0) do
      (setf k 0)
      (loop for i from 0 below R do
        (if (> (nth i lst) (nth (+ i 1) lst))
            (let ((tmp (nth i lst)))
              (setf (nth i lst) (nth (+ i 1) lst))
              (setf (nth (+ i 1) lst) tmp)
              (setf k i)))) 
      (setf R k))  
    lst))
```

### Тестові набори

```lisp
(defun test-sort-imp ()
  (check-sort-imp "Test 1" '(4 3 2 1) '(1 2 3 4))
  (check-sort-imp "Test 2" '(1 2 3 4) '(1 2 3 4))
  (check-sort-imp "Test 3" '(5 1 3 2 4) '(1 2 3 4 5)))
```

### Тестування

```lisp
Testing sort-imp:
passed... Test 1
passed... Test 2
passed... Test 3
```
