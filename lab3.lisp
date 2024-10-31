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

(defun generate-indices (n &optional (index 0))
  (if (>= index n)
      '()
      (cons index (generate-indices n (1+ index)))))

(defun swap (lst i j)
  (mapcar (lambda (index)
            (cond ((= index i) (nth j lst))
                  ((= index j) (nth i lst))
                  (t (nth index lst))))
          (generate-indices (length lst))))

(defun bubble-step (lst i R last-swap reverse)
  (if (>= i R)
      (values lst last-swap)
      (let ((a (nth i lst))
            (b (nth (1+ i) lst)))
        (if (if reverse (< a b) (> a b))
            (bubble-step (swap lst i (1+ i)) (1+ i) R i reverse)
            (bubble-step lst (1+ i) R last-swap reverse)))))

(defun sort-func (lst &optional (reverse nil))
  (labels ((recursive-sort (lst R)
             (multiple-value-bind (new-lst last-swap) (bubble-step lst 0 R nil reverse)
               (if (not last-swap)
                   new-lst
                   (recursive-sort new-lst last-swap)))))
    (recursive-sort lst (1- (length lst)))))

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
