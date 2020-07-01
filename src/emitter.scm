(declare (unit emitter-llvm))
(import (chicken pretty-print))

(define (emit-llvm-object ast port)
  (assert (eq? (caaar ast) 'top-level))
  (emit-llvm-translation-unit (caadr (caar ast))))

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
  (pp ast)
  (print "^ have a declaration")

  (let looper ((xs (cadr ast))
               (types '()))
    (cond
      ((null? xs)
       (print "Reached end of declaration, unused types: " types)
       '())

      ((is-typedef-specifier? (car xs))
       (pp xs)
       (print "TODO: emit llvm type here")
       '())

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

(define (emit-llvm-declarator ast return-types)
  (print "^ have declarator, return types " return-types))

(define (emit-llvm-function-definition ast)
  (print "^ have a function definition"))

;; TODO: seperate unit for type checks like this
(define (type-check-car tag)
  (lambda (xs)
    (and (list? xs)
         (eq? (car xs) tag))))

(define is-translation-unit?     (type-check-car 'translation-unit))
(define is-external-declaration? (type-check-car 'external-declaration))
(define is-declaration?          (type-check-car 'declaration))
(define is-declarator?           (type-check-car 'declarator))
(define is-function-definition?  (type-check-car 'function-definition))
(define is-type-specifier?       (type-check-car 'type-specifier))
(define is-type-qualifier?       (type-check-car 'type-qualifier))
