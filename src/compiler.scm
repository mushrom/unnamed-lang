(declare (unit compiler)
         (uses parser)
         (uses emitter-llvm))

(import (chicken pretty-print))

(define (has-syntax-error? ast)
  (eq? (car ast) 'syntax-error))

(define (analysis-passes ast dump-flags)
  ;; TODO
  ast)

;; takes a parse tree generated from (parse-c-top-level)
(define (compile-file filename flags)
  (let ((ast (parse-c-top-level (read-source filename))))
    (if (eq? (car ast) 'syntax-error)
      (begin
        (pp ast)
        (print "fatal error, exiting.")
        (exit 1))
      (begin
        (emit-llvm-object (analysis-passes ast '())
                          (current-output-port))
        (print "parsed successfully.")))))

;; TODO: utility thing
(define (read-source filename)
  (with-input-from-file filename
        (lambda ()
          (let loop ()
            (let ((c (read-char)))
              (cond ((eof-object? c) '())
                    (else (cons c (loop)))))))))
