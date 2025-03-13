;;; Global parser state variables.
(defvar *tokens* nil)
(defvar *lookahead* nil)

;;; Initialize the parser with a list of tokens.
(defun init-parser (tokens)
  (setf *tokens* tokens)
  (if tokens
      (setf *lookahead* (car tokens))
      (setf *lookahead* nil)))

;;; Consume the expected token and advance the lookahead.
(defun consume (expected)
  (if (eq *lookahead* expected)
      (progn
        (setf *tokens* (cdr *tokens*))
        (setf *lookahead* (if *tokens* (car *tokens*) nil)))
      (error "Parse error: Expected ~A but found ~A" expected *lookahead*)))

;;; Grammar rules implemented as functions:

;;; I  → i E S I′
(defun I ()
  (if (eq *lookahead* 'i)
      (progn
        (consume 'i)
        (E)
        (S)
        (I-Prime))
      (error "Parse error in I: Expected 'i' but found ~A" *lookahead*)))

;;; I′ → e S | ε
(defun I-Prime ()
  (if (eq *lookahead* 'e)
      (progn
        (consume 'e)
        (S))
      ;; ε-production: do nothing.
      nil))

;;; E  → G E′
(defun E ()
  (if (or (eq *lookahead* 'x)
          (eq *lookahead* 'y)
          (eq *lookahead* 'z)
          (eq *lookahead* 'w))
      (progn
        (G)
        (E-Prime))
      (error "Parse error in E: Expected one of x, y, z, or w but found ~A" *lookahead*)))

;;; E′ → o G E′ | ε
(defun E-Prime ()
  (if (eq *lookahead* 'o)
      (progn
        (consume 'o)
        (G)
        (E-Prime))
      ;; ε-production: do nothing.
      nil))

;;; G  → x | y | z | w
(defun G ()
  (cond ((eq *lookahead* 'x) (consume 'x))
        ((eq *lookahead* 'y) (consume 'y))
        ((eq *lookahead* 'z) (consume 'z))
        ((eq *lookahead* 'w) (consume 'w))
        (t (error "Parse error in G: Expected one of x, y, z, or w but found ~A" *lookahead*))))

;;; S  → s | d L b
(defun S ()
  (cond ((eq *lookahead* 's)
         (consume 's))
        ((eq *lookahead* 'd)
         (consume 'd)
         (L)
         (if (eq *lookahead* 'b)
             (consume 'b)
             (error "Parse error in S: Expected 'b' after L in production S → d L b")))
        (t (error "Parse error in S: Expected 's' or 'd' but found ~A" *lookahead*))))

;;; L  → s L′
(defun L ()
  (if (eq *lookahead* 's)
      (progn
        (consume 's)
        (L-Prime))
      (error "Parse error in L: Expected 's' but found ~A" *lookahead*)))

;;; L′ → s L′ | ε
(defun L-Prime ()
  (if (eq *lookahead* 's)
      (progn
        (consume 's)
        (L-Prime))
      ;; ε-production: do nothing.
      nil))

;;; Main parse function: start with I and verify all tokens are consumed.
(defun parse (tokens)
  (init-parser tokens)
  (I)
  (if *lookahead*
      (error "Parse error: Extra input after parsing")
      (format t "Parsing succeeded!~%")))
      
(parse '(i x s e s))