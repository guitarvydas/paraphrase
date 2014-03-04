;; see LICENSE

;; PEG grammar (incomplete) for Prolog, based on 1993 ISO standard
;; http://www.amzi.com/AdventureInProlog/a9struct.php


(in-package :prolog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #.(esrap:parse 'peg-grammar::grammar #>%prolog>

Prolog 		<- Spacing (Fact / CompoundQuery / Rule)+ EndOfFile
                     { (:destructure (spc fcr eof)
                        (declare (ignore spc eof))
                        `(progn ,@fcr)) }

Fact		<- Predicate Args? '.' Spacing
                   { (:destructure (pred args dot spc)
                      (declare (ignore dot spc))
                      (if args
                          `(defrel (,pred ,@args))
                        `(defrel (,pred)))) }

CompoundQuery	<- '?-' Spacing Query (COMMA Query)* '.' Spacing
                   { (:destructure (q sp query queries dot spc)
                      (declare (ignore q sp dot spc))
                      `(defquery ,query ,@(mapcar #'second queries))) }

Rule            <- (RuleWithBody / RuleNoBody) Spacing
                   { (:function first) }

RuleNoBody      <- Head Spacing '.'
                   { (:destructure (head sp1 dot)
                      (declare (ignore sp1 dot))
                      `(defrule ,head)) }

RuleWithBody	<- Head Spacing ':-' Spacing Body '.'
                   { (:destructure (head sp1 rule sp2 body dot)
                      (declare (ignore sp1 rule sp2 dot))
                      `(defrule ,head ,@body)) }

Query		<- Small-Ident Args
                   { (:destructure (id args)
                      `(,id ,@args)) }

Head		<- Small-Ident Args?
                   { (:destructure (id args)
                      (if args
                          `(,id ,@args)
                        `(,id))) }

Body		<- Goal (COMMA Goal)*
                   { (:destructure (g1 gs)
                      (if gs
                          `(,g1 ,@(mapcar #'second gs))
                        `(,g1))) }

Goal		<- Small-Ident-with-Args / Call / Small-Ident / Arith / Cut

Small-Ident-with-Args <- Small-Ident Args
                   { (:destructure (id args)
                      `(,id ,@args)) }
                   
Call            <- Small-Ident LPAR Goal RPAR
                   { (:destructure (id lp arg rp)
                      (declare (ignore lp rp))
                      `(,id (,@arg))) }

Structure	<- Functor Args
Functor		<- Small-Ident

Arith		<- Variable IS Expr / Equ
Equ		<- Expr ('=' Expr)*     
Expr		<- Term (RELOP Term)*
Term		<- Factor (PLOP Factor)*
Factor		<- Primary (MULOP Primary)*
Primary		<- Fundament / LPAR Equ RPAR

RELOP		<- ( '<' / '>' / '=<' / '=>' ) Spacing
PLOP		<- ( '+' / '-' ) Spacing
MULOP		<- ( '*' / '/' ) Spacing

Predicate	<- Small-Ident
Args		<- LPAR Fundament (COMMA Fundament)* RPAR
                   { (:destructure (lp f fs rp)
                      (declare (ignore lp rp))
                      `(,f ,@(mapcar #'second fs))) }

Fundament	<- Atom / Number / Variable / Structure / String / '_' / List / Cons / Structure

List		<- LBRACKET Fundament (COMMA Fundament)* RBRACKET

Cons		<- LBRACKET Fundament '|' Fundament RBRACKET
                   { (:destructure (lb car bar cdr rb)
                      (declare (ignore lb bar rb))
                      (cons car cdr)) }

Variable	<- [_A-Z] [_A-Za-z0-9]*
                   { (:destructure (a b) (intern (string-upcase (esrap:text "?" a b)))) }

Atom		<- Small-Ident / Special+ / '[]' / '{}' / SQString
Small-Ident	<- [a-z] [_A-Za-z0-9]*
                   { (:destructure (a b) (intern (string-upcase (esrap:text a b)))) }

Special		<- '#' / '$' / '&' / '*' / '+' / '-' / '.' / '/' / ':' / '<' / '>' / '=' / '?' / '@' / '^' / '~' / '\\'
Number		<- Integer / Float

Integer		<- Sign Unsigned-Int
                   { (:destructure (s i)
                      (if (eq 'minus s)
                          (- i)
                        i)) }                              

Unsigned-Int	<- [0-9]+
                   { (:lambda (n)
                       (parse-integer (esrap:text n))) }

Float		<- Sign Unsigned-Float / Unsigned-Float
Unsigned-Float	<- [0-9]+ '.' [0-9]+ Exponent*
Exponent	<- 'E' [+-] [0-9]+
String		<- SQString / DQString / Char-List
SQString	<- ['] [!\']+ [']
                   { (:destructure (q1 str q2)
                      (declare (ignore q1 q2))
                      (esrap:text str)) }
DQString	<- ["] [!\"]+ ["]
                   { (:destructure (q1 str q2)
                      (declare (ignore q1 q2))
                      (esrap:text str)) }
Char-List	<- LBRACKET Char (COMMA Char)* RBRACKET
                   { (:destructure (lb c str rb)
                      (declare (ignorable lb rb))
                      (format t "~A /~A/ ~A ~A%" lb c str rb)
                      (concatenate 'string (esrap:text c) (esrap:text str)))
                   }
Char            <- Char1 / Char2
Char1		<- [!\']
                   { (:text t) }
Char2           <- ['] ' ' [']
                   {(:constant " ")}
Sign		<- ('+' / '-')* Spacing
                   { (:destructure (sign sp)
                      (declare (ignore sp))
                      (or (and sign (string= "-" (first sign)) 'minus)
                          'plus)) }

Cut		<- '!' Spacing
                   { (:constant '!) }

IS		<- [iI][sS] Spacing
LPAR		<- '(' Spacing
RPAR		<- ')' Spacing
LBRACKET	<- '[' Spacing
RBRACKET	<- ']' Spacing
COMMA		<- ',' Spacing

Spacing 	<- (Space / Comment)*
	           { (:lambda(x) (declare (ignore x)) (values)) }
Comment 	<- Line-Comment
Line-Comment 	<- '%' (!EndOfLine .)* (EndOfLine / EndOfFile)
Space   	<- ' ' / '\t' / EndOfLine
EndOfLine 	<- '\r\n' / '\n' / '\r'
EndOfFile 	<- !.

%prolog
))

