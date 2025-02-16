
(defun my-merge (l1 l2)
  "Merge two sorted lists L1 and L2 into one sorted list."
  (cond ((null l1) l2)
        ((null l2) l1)
        ((<= (car l1) (car l2))
         (cons (car l1) (my-merge (cdr l1) l2)))
        (t (cons (car l2) (my-merge l1 (cdr l2))))))

;; Partitioning function: processes two elements at a time.
(defun partition-list (lst)
  "Partition LST into two nearly equal halves.
Each recursive call takes the next two items (if available)
and puts the first into LEFT and the second into RIGHT."
  (cond ((null lst) (values '() '()))
        ((null (cdr lst)) (values (list (car lst)) '()))
        (t (multiple-value-bind (left right) (partition-list (cddr lst))
             (values (cons (car lst) left)
                     (cons (cadr lst) right))))))

;; Mergesort function: uses partition-list to divide, then recursively sorts
(defun mergesort (lst)
  "Sort LST using mergesort. For lists of size 0 or 1, return LST."
  (if (or (null lst) (null (cdr lst)))
      lst
      (multiple-value-bind (left right) (partition-list lst)
        (my-merge (mergesort left) (mergesort right)))))

;; Example test:
(print (mergesort '(5 2 8 1 9 3 7 4)))