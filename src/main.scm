(declare (uses compiler))
(import (chicken process-context))

(define *args* (command-line-arguments))

(when (< (length *args*) 1)
  (print "Usage: foo input.c")
  (exit 1))

(compile-file (list-ref *args* 0) '())
