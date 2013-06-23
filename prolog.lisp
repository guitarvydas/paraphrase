;; see LICENSE

;; PEG grammar (incomplete) for Prolog, based on 1993 ISO standard
;; http://www.amzi.com/AdventureInProlog/a9struct.php


(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :cl-user)
  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #.(esrap:parse 'peg-grammar::grammar #>%prolog>

Prolog 		<- (Fact / CompoundQuery / Rule)+ Spacing EndOfFile
Fact		<- Predicate Args '.' / Predicate '.'
CompoundQuery	<- '?-' Spacing Query (COMMA Query)* '.'
Rule		<- Head Spacing ':-' Spacing Body '.'

Query		<- Small-Ident Args
Head		<- Small-Ident Args / Small-Ident
Body		<- Goal (COMMA Goal)*
Goal		<- Small-Ident Args / Small-Ident / Arith / Cut
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
Fundament	<- Atom / Number / Variable / Structure / String / '_' / List / Structure

List		<- LBRACKET Fundament (COMMA Fundament)* RBRACKET

Variable	<- [_A-Z] [_A-Za-z0-9]*
Atom		<- Small-Ident / Special+ / '[]' / '{}' / SQString
Small-Ident	<- [a-z] [_A-Za-z0-9]*
Special		<- '#' / '$' / '&' / '*' / '+' / '-' / '.' / '/' / ':' / '<' / '>' / '=' / '?' / '@' / '^' / '~' / '\\'
Number		<- Integer / Float
Integer		<- Sign Unsigned-Int / Unsigned-Int
Unsigned-Int	<- [0-9]+
Float		<- Sign Unsigned-Float / Unsigned-Float
Unsigned-Float	<- [0-9]+ '.' [0-9]+ Exponent*
Exponent	<- 'E' [+-] [0-9]+
String		<- SQString / DQString / Char-List
SQString	<- ['] [!\']+ [']
                   { (:destructure (q1 str q2)
                      (declare (ignore q1 q2))
                      (text str)) }
DQString	<- ["] [!\"]+ ["]
                   { (:destructure (q1 str q2)
                      (declare (ignore q1 q2))
                      (text str)) }
Char-List	<- LBRACKET Char (COMMA Char)* RBRACKET
                   { (:destructure (lb c str rb)
                      (declare (ignore lb rb))
                      (format t "~A /~A/ ~A~ ~A%" lb c str rb)
                   ;   (concatenate 'string (text c) (text str)))
                   }
Char            <- Char1 / Char2
Char1		<- [!\']
                   { (:text t) }
Char2           <- ['] ' ' [']
                   {(:constant " ")}
Sign		<- ('+' / '-')* Spacing
Cut		<- '!' Spacing
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\{
                       #'(lambda (s c)
                           (declare (ignore c))
                           (esrap:parse 'cl-user::prolog
                                        (with-output-to-string (str)
                                          (loop for c = (read-char s nil 'eof)
                                                until (or (eq c 'eof) (char= c #\}))
                                                do (write-char c str))))))


(defun test-prolog ()
  ; from http://www.cs.toronto.edu/~hojjat/384f06/simple-prolog-examples.html
  '{
likes(mary,food).
})