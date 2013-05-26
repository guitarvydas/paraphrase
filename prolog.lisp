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
DQString	<- ["] [!\"]+ ["]
Char-List	<- LBRACKET Char (COMMA Char)* RBRACKET
Char		<- [!\'] / ['] ' ' [']
Sign		<- ('+' / '-')* Spacing
Cut		<- '!' Spacing
IS		<- [iI][sS] Spacing
LPAR		<- '(' Spacing
RPAR		<- ')' Spacing
LBRACKET	<- '[' Spacing
RBRACKET	<- ']' Spacing
COMMA		<- ',' Spacing

Comment 	<- Line-Comment
Spacing 	<- (Space / Comment)*
Line-Comment 	<- '%' (!EndOfLine .)* (EndOfLine / EndOfFile)
Space   	<- ' ' / '\t' / EndOfLine
EndOfLine 	<- '\r\n' / '\n' / '\r'
EndOfFile 	<- !.

%prolog
))