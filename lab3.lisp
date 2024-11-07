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

(defun bubble-step-functional (lst last-swap-position current-position)
  (cond
   ((or (null (cdr lst)) (not last-swap-position)) (values lst last-swap-position)) 
   ((> (car lst) (cadr lst))  
    (multiple-value-bind (new-tail new-last-swap-position)
        (bubble-step-functional (cons (cadr lst) (cdr (cdr lst))) current-position (1+ current-position))
      (values (cons (car lst) new-tail) new-last-swap-position)))
   (t  
    (multiple-value-bind (new-tail new-last-swap-position)
        (bubble-step-functional (cdr lst) last-swap-position (1+ current-position))
      (values (cons (car lst) new-tail) new-last-swap-position)))))

(defun sort-func (lst)
  (labels ((recursive-sort (lst)
             (multiple-value-bind (new-lst last-swap-position) 
                 (bubble-step-functional lst nil 1)  
               (if (not last-swap-position)
                   new-lst
                   (recursive-sort new-lst)))))
    (recursive-sort lst)))

(defun check-sort-func (name input expected)
  "Execute `sort-func` on `input`, compare result with `expected` and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (sort-func (copy-list input)) expected)
          name))

(defun test-sort-func ()
  (check-sort-func "Test 1" '(4 3 2 1) '(1 2 3 4))
  (check-sort-func "Test 2" '(1 2 3 4) '(1 2 3 4))
  (check-sort-func "Test 3" '(5 1 3 2 4) '(1 2 3 4 5)))

(defun check-sort-imp (name input expected)
  "Execute `sort-imp` on `input`, compare result with `expected` and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (sort-imp (copy-list input)) expected)
          name))

(defun test-sort-imp ()
  (check-sort-imp "Test 1" '(4 3 2 1) '(1 2 3 4))
  (check-sort-imp "Test 2" '(1 2 3 4) '(1 2 3 4))
  (check-sort-imp "Test 3" '(5 1 3 2 4) '(1 2 3 4 5)))

(defun run-all-tests ()
  (format t "Testing sort-func:~%")
  (test-sort-func)
  (format t "Testing sort-imp:~%")
  (test-sort-imp))

(run-all-tests)
