;; Use the same merge function.
(defun my-merge (l1 l2)
  "Merge two sorted lists L1 and L2 into one sorted list."
  (cond ((null l1) l2)
        ((null l2) l1)
        ((<= (car l1) (car l2))
         (cons (car l1) (my-merge (cdr l1) l2)))
        (t (cons (car l2) (my-merge l1 (cdr l2))))))

;; Step a: Partition LST into sorted pairs.
(defun make-sorted-pairs (lst)
  "Partition LST into a list of sublists, each of one or two elements.
For two elements, the pair is sorted in ascending order."
  (cond ((null lst) '())
        ((null (cdr lst)) (list lst))
        (t (let* ((first (car lst))
                  (second (cadr lst))
                  (pair (if (<= first second)
                            (list first second)
                            (list second first))))
             (cons pair (make-sorted-pairs (cddr lst)))))))

;; Step b: Merge adjacent sublists into a new list of merged sublists.
(defun merge-pairs (lists)
  "Merge adjacent sublists in LISTS.
If an odd number of sublists remain, the last is kept as is."
  (cond ((null lists) '())
        ((null (cdr lists)) lists)
        (t (cons (my-merge (car lists) (cadr lists))
                 (merge-pairs (cddr lists))))))

;; Recursive function that applies merge passes until one sorted list remains.
(defun merge-pass (lists)
  "Perform merge passes on the list of sublists until a single sorted list remains."
  (if (null (cdr lists))
      (first lists)
      (merge-pass (merge-pairs lists))))

;; Bottom-Up Mergesort: first convert into sorted pairs then repeatedly merge.
(defun bottom-up-mergesort (lst)
  "Sort LST using bottom-up mergesort."
  (if (or (null lst) (null (cdr lst)))
      lst
      (merge-pass (make-sorted-pairs lst))))

;; Example test:
(print (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))