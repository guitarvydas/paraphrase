(in-package :prolog)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\{
                       #'(lambda (s c)
                           (declare (ignore c))
                           (esrap:parse 'prolog::prolog
                                        (with-output-to-string (str)
                                          (loop for c = (read-char s nil 'eof)
                                                until (or (eq c 'eof) (char= c #\}))
                                                do (write-char c str)))))))

(defun test-prolog ()
  ; from http://www.cs.toronto.edu/~hojjat/384f06/simple-prolog-examples.html
  (pprint '{
          likes(mary,food).
          likes(paul,food).
          ?- likes(mary,food).
          ?- likes(X,food).
          ?- likes(paul,X).
          member(Item,[Item|Rest]).
          member(Item,[X|Rest]) :- member(Item,Rest).
          }))

(defun test2 ()
  (pprint '{
          male(john).
          male(thomas).
          male(william).
          male(james).

          female(X) :- not(male(X)).

          not(Call) :- call(Call), !, fail.
          not(Call).

          equal(X, X).

          father(william, thomas).
          father(william, sue).
          father(john, william).
          father(james, anne).

          mother(anne, thomas).
          mother(anne, sue).
          mother(jeanne, william).
          mother(denise, anne).

          parent(X, Y) :- father(X, Y).
          parent(X, Y) :- mother(X, Y).

          grandparent(X, Y) :- parent(X, Z), parent(Z, Y).
          grandfather(X, Y) :- grandparent(X, Y), male(X).
          grandmother(X, Y) :- grandparent(X, Y), female(X).

          brother(X, Y) :- male(X), parent(Z, X), not(equal(X, Y)).

          ?- male(X).

          }))
