(declare (unit ast-types))

;; TODO: seperate unit for type checks like this
(define (type-check-car tag)
  (lambda (xs)
    (and (list? xs)
         (not (null? xs))
         (eq? (car xs) tag))))

(define is-syntax-error?              (type-check-car 'syntax-error))
(define is-translation-unit?          (type-check-car 'translation-unit))
(define is-external-declaration?      (type-check-car 'external-declaration))
(define is-declaration?               (type-check-car 'declaration))
(define is-declarator?                (type-check-car 'declarator))
(define is-identifier?                (type-check-car 'identifier))
(define is-function-definition?       (type-check-car 'function-definition))
(define is-type-specifier?            (type-check-car 'type-specifier))
(define is-type-qualifier?            (type-check-car 'type-qualifier))
(define is-struct-or-union-specifier? (type-check-car 'struct-or-union-specifier))
