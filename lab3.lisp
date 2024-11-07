(defun sort-imp (lst)
  (let (R k)
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


(defun check-sort-func (name input expected)
  "Execute `sort-func` on `input`, compare result with `expected` and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (recursive-bubble-sort (copy-list input)) expected)
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
