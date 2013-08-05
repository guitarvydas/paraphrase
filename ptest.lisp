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

