(declare (uses parser))
(import (chicken process-context))
(import (chicken pretty-print))

(define *args* (command-line-arguments))

(when (< (length *args*) 1)
  (print "Usage: foo input.c")
  (exit 1))

;; TODO: utility thing
(define (read-source filename)
  (with-input-from-file filename
        (lambda ()
          (let loop ()
            (let ((c (read-char)))
              (cond ((eof-object? c) '())
                    (else (cons c (loop)))))))))

;(import lexgen)
;(pp (parse-c-top-level (read-source (list-ref *args* 0))))
(let ((ast (parse-c-top-level (read-source (list-ref *args* 0)))))
  (if (eq? (car ast) 'syntax-error)
    (begin
      (print "fatal error, exiting.")
      (exit 1))
    (print "parsed successfully.")))
;(pp (lex unnamed-program err (list '() (read-source (list-ref *args* 0)))))

;(write (unnamed-program caar err (list '() (read-source (list-ref *args* 0)))))
