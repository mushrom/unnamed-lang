(declare (unit emitter-vm))
(import (chicken pretty-print))

(define (emit-vm-object ast port)
  (assert (eq? (caaar ast) 'top-level))
  (emit-vm-translation-unit (caadr (caar ast)) port))

(define (emit-vm-translation-unit ast port)
  (assert (is-translation-unit? ast))
  (pp (caadr ast))
  (print "^ translation unit")
  (emit-vm-external-declaration (caadr ast) port)
  (when (not (null? (cdadr ast)))
    (emit-vm-translation-unit (car (cdadr ast)) port)))

(define (emit-vm-external-declaration ast port)
  (assert (is-external-declaration? ast))
  (let ((decl (caadr ast)))
    (cond ((is-function-definition? decl)
           (emit-function-definition decl port))
          ((is-declaration? decl)
           (emit-declaration decl port))
          (else (assert #f)))))

(define (emit-declaration ast port)
  (assert (is-declaration? ast))
  (print "^ have a declaration"))

(define (emit-function-definition ast port)
  (print "^ have a function definition"))

;; TODO: seperate unit for type checks like this
(define (type-check-car tag)
  (lambda (xs)
    (and (list? xs)
         (eq? (car xs) tag))))

(define is-translation-unit?     (type-check-car 'translation-unit))
(define is-external-declaration? (type-check-car 'external-declaration))
(define is-declaration?          (type-check-car 'declaration))
(define is-function-definition?  (type-check-car 'function-definition))
