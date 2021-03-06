(ql:quickload "esrap")
(ql:quickload "cl-heredoc")
(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
(ql:register-local-projects)

(defsystem peg (:optimize ((speed 0) (space 0) (safety 3) (debug 3)))
  :members (
            "package"
            "peg"
            )
  :rules ((:compile :all (:requires (:load :previous)))))

(defsystem prolog (:optimize ((speed 0) (space 0) (safety 3) (debug 3)))
  :members (
            "package"
            "peg"
            "prolog"
            "trace"
            "ptest"
            )
  :rules ((:compile :all (:requires (:load :previous)))
          (:in-order-to :compile ("ptest")
           (:caused-by (:compile "prolog")))))
