;; Test CAR/CDR sequences
(format t "Result 1: ~a~%" (car (cdr (cdr '(a b x d)))))
(format t "Result 2: ~a~%" (car (cdr (car (cdr '(a (b (x d))))))))
(format t "Result 3: ~a~%" (car (cdr (car (cdr (car (car '(((a (b (x) d)))))))))))

;; Test CONS constructions
(format t "List 1: ~a~%" (cons 'a (cons 'b (cons 'x (cons 'd '())))))
(format t "List 2: ~a~%" (cons 'a (cons (cons 'b (cons (cons 'x (cons 'd '())) '())) '())))
(format t "List 3: ~a~%" (cons (cons (cons 'a (cons (cons 'b (cons (cons 'x '()) (cons 'd '()))) '())) '()) '()))
