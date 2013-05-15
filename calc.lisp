;; see LICENSE


(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :cl-user)
  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #.(esrap:parse 'peg-grammar::grammar #>%calc>

calculator <- expr

expr <- (eadd / esub / factor) ws
	{(:function first)}

eadd <- factor ws '+' factor
	{(:destructure (a ws pl b)
          (declare (ignore ws pl))
          (+ a b))}

esub <- factor ws '-' factor
	{(:destructure (a ws mi b)
          (declare (ignore ws mi))
          (- a b))}

factor <- fmul / fdiv / primary

fmul <- primary ws '*' primary
	{(:destructure (a ws mul b)
          (declare (ignore ws mul))
          (* a b))}

fdiv <- primary ws '*' primary
	{(:destructure (a ws mul b)
          (declare (ignore ws mul))
          (/ a b))}

primary <- paren-expr / num

paren-expr <- ws '(' expr ws ')'
	{(:destructure (w1 lp e w2 rp)
          (declare (ignore w1 w2 lp rp))
          e)}
            
num <- ws integer
	{(:function second)}

integer <- [0-9]+
	{(:lambda (list)
           (parse-integer (esrap:text list) :radix 10)) }

ws <- whitespace?

whitespace <- (' ' / '\t' / '\n')+
	{ (:lambda(x) (declare (ignore x)) (values)) }

%calc
))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\{
                       #'(lambda (s c)
                           (declare (ignore c))
                           (esrap:parse 'cl-user::calculator
                                        (with-output-to-string (str)
                                          (loop for c = (read-char s nil 'eof)
                                                until (or (eq c 'eof) (char= c #\}))
                                                do (write-char c str))))))

(maphash #'(lambda (k v) (declare (ignore v)) (prin1 k) (terpri)) esrap::*rules*)
(esrap:describe-grammar 'cl-user::calculator)
)

(defun test ()
  (list
   { 1 }
   { 1 + 2 }
   { 1 + 2 * 3 }
   { (1 + 2) * 3 }))
   