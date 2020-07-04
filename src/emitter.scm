(declare (unit emitter-llvm))
(import (chicken pretty-print))

(define *counter* 0)
(define (alloc-type-id)
  (let ((c *counter*))
    (set! *counter* (+ *counter* 1))
    (string-append "%T" (number->string c))))

(define (alloc-variable-id)
  (let ((c *counter*))
    (set! *counter* (+ *counter* 1))
    (string-append "%x" (number->string c))))

(define (emit-llvm-object ast port)
  ;(pp ast)
  (emit-llvm-translation-unit ast))

(define (emit-llvm-translation-unit ast)
  (assert (is-translation-unit? ast))
  (print "^ translation unit")
  (emit-llvm-external-declaration (caadr ast))
  (when (not (null? (cdadr ast)))
    (emit-llvm-translation-unit (car (cdadr ast)))))

(define (emit-llvm-external-declaration ast)
  (assert (is-external-declaration? ast))
  (let ((decl (caadr ast)))
    (cond ((is-function-definition? decl)
           (emit-llvm-function-definition decl))
          ((is-declaration? decl)
           (emit-llvm-declaration decl))
          (else (assert #f)))))

(define (emit-llvm-declaration ast)
  (assert (is-declaration? ast))
  ;(pp ast)
  (print "^ have a declaration")

  (let looper ((xs (cadr ast))
               (types '()))
    (cond
      ((null? xs)
       (print "Reached end of declaration, unused types: " types)
       '())

      ((is-typedef-specifier? (car xs))
       (emit-typedef xs))

      ((is-type-specifier? (car xs))
       (let ((type (car (cadar xs))))
         (print "^ type specifier " type)
         (looper (cdr xs)
                 (cons type types))))

      ((is-declarator? (car xs))
       (emit-llvm-declarator (car xs) types))

      (else
        (pp (car xs))
        (looper (cdr xs) types)))))

(define (emit-typedef ast)
  (assert is-typedef-specifier? (car ast))
  (print "TODO: emit llvm type here"))

(define (emit-llvm-declarator ast return-types)
  (print "declare " return-types )
  (print "^ have declarator, return types " return-types))

(define (emit-llvm-function-definition ast)
  (print "^ have a function definition"))
