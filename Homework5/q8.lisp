;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser Implementation with Error Reporting  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *tokens* nil)
(defvar *lookahead* nil)
(defvar *position* 1)  ; Keeps track of the current token position

(defun init-parser (tokens)
  (setf *tokens* tokens)
  (setf *position* 1)
  (if tokens
      (setf *lookahead* (car tokens))
      (setf *lookahead* nil)))

(defun consume (expected)
  (if (eq *lookahead* expected)
      (progn
        (incf *position*)
        (setf *tokens* (cdr *tokens*))
        (setf *lookahead* (if *tokens* (car *tokens*) nil)))
      (error "Unexpected symbol '~A' at position ~A: expected '~A'" 
             *lookahead* *position* expected)))

;;; Grammar (Left-Factored):
;;; I  → i E S I′
;;; I′ → e S | ε
;;; E  → G E′
;;; E′ → o G E′ | ε
;;; G  → x | y | z | w
;;; S  → s | d L b
;;; L  → s L′
;;; L′ → s L′ | ε

(defun I ()
  (if (eq *lookahead* 'i)
      (progn
        (consume 'i)
        (E)
        (S)
        (I-Prime))
      (error "Unexpected symbol '~A' at position ~A in I: expected 'i'" 
             *lookahead* *position*)))

(defun I-Prime ()
  (if (eq *lookahead* 'e)
      (progn
        (consume 'e)
        (S))
      ;; ε-production: do nothing.
      nil))

(defun E ()
  (if (member *lookahead* '(x y z w))
      (progn
        (G)
        (E-Prime))
      (error "Unexpected symbol '~A' at position ~A in E: expected one of 'x', 'y', 'z', or 'w'"
             *lookahead* *position*)))

(defun E-Prime ()
  (if (eq *lookahead* 'o)
      (progn
        (consume 'o)
        (G)
        (E-Prime))
      ;; ε-production: do nothing.
      nil))

(defun G ()
  (cond ((eq *lookahead* 'x) (consume 'x))
        ((eq *lookahead* 'y) (consume 'y))
        ((eq *lookahead* 'z) (consume 'z))
        ((eq *lookahead* 'w) (consume 'w))
        (t (error "Unexpected symbol '~A' at position ~A in G: expected one of 'x', 'y', 'z', or 'w'"
                  *lookahead* *position*))))

(defun S ()
  (cond ((eq *lookahead* 's)
         (consume 's))
        ((eq *lookahead* 'd)
         (consume 'd)
         (L)
         (if (eq *lookahead* 'b)
             (consume 'b)
             (error "Unexpected symbol '~A' at position ~A in S: expected 'b' after L" 
                    *lookahead* *position*)))
        (t (error "Unexpected symbol '~A' at position ~A in S: expected 's' or 'd'"
                  *lookahead* *position*))))

(defun L ()
  (if (eq *lookahead* 's)
      (progn
        (consume 's)
        (L-Prime))
      (error "Unexpected symbol '~A' at position ~A in L: expected 's'" 
             *lookahead* *position*)))

(defun L-Prime ()
  (if (eq *lookahead* 's)
      (progn
        (consume 's)
        (L-Prime))
      ;; ε-production: do nothing.
      nil))

(defun parse (tokens)
  "Main parse function. On success, prints a success message.
If an error occurs, an error is signaled with position information."
  (init-parser tokens)
  (I)
  (if *lookahead*
      (error "Unexpected symbol '~A' at position ~A: extra input after valid parse" 
             *lookahead* *position*)
      (format t "Parsing succeeded!~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing the 14 Strings from Q7              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-tests ()
  (format t "### Running Valid Strings Tests ###~%")
  (dolist (test 
           '(("Valid1" (i x s e s))
             ("Valid2" (i x d s b e s))
             ("Valid3" (i z o w d s b e s))
             ("Valid4" (i y o x d s b e d s b))
             ("Valid5" (i x o y o z d s b e d s b))
             ("Valid6" (i x o z o w d s s b e d s s b))
             ("Valid7" (i w o x o y o z d s s b e d s s b))))
    (destructuring-bind (name tokens) test
      (format t "~%Test ~A with tokens: ~A~%" name tokens)
      (handler-case 
           (progn
             (parse tokens)
             (format t "Test ~A: Passed.~%" name))
        (error (e)
          (format t "Test ~A: Failed with error: ~A~%" name e)))))
  
  (format t "~%### Running Invalid Strings Tests ###~%")
  (dolist (test 
           '(("Invalid1" (i a s e s))
             ("Invalid2" (i x d x b e s))
             ("Invalid3" (i z p w d s b e s))
             ("Invalid4" (i y o x d s b e d s d))
             ("Invalid5" (i x p y o z d s b e d s b))
             ("Invalid6" (i x o z o w f s s b e d s s b))
             ("Invalid7" (j w o x o y o z d s s b e d s s b))))
    (destructuring-bind (name tokens) test
      (format t "~%Test ~A with tokens: ~A~%" name tokens)
      (handler-case 
           (progn
             (parse tokens)
             (format t "Test ~A: Passed (unexpected)!~%" name))
        (error (e)
          (format t "Test ~A: Failed as expected with error: ~A~%" name e))))))


(run-tests)