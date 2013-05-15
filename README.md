PEG parser for writing CL reader macros.

A Common Lisp PEG parser with PEG syntax.  Translates PEG syntax into ESRAP syntax.  The resulting (esrap) parser is meant to be used in reader macros, allowing for reading rich DSL syntaxes and converting them into lisp for the reader/compiler.

How to load it manually (in the interim):

>  (ql:quickload "esrap")
>  (ql:quickload "cl-heredoc")
>  (set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
>  (ql:register-local-projects)
>  (compile-file "peg.lisp")
>  (load "peg")

Examples:

pegself.lisp: peg written in itself ; was used to generate peg.lisp.


