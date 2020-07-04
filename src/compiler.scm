(declare (unit compiler)
         (uses parser)
         (uses emitter-llvm)
         (uses ast-types))

(import (chicken pretty-print))

;; takes a parse tree generated from (parse-c-top-level)
(define (compile-file filename flags)
  (let ((ast (parse-c-top-level (read-source filename))))
    (if (is-syntax-error? ast)
      (begin
        (pp ast)
        (print "fatal error, exiting.")
        (exit 1))
      (begin
        (assert (eq? (caaar ast) 'top-level))
        (let* ((top (caadr (caar ast)))
               (processed (analysis-passes top '())))
          ;(pp top)
          (emit-llvm-object processed (current-output-port))
          (print "parsed successfully."))))))

;; TODO: utility thing
(define (read-source filename)
  (with-input-from-file filename
        (lambda ()
          (let loop ()
            (let ((c (read-char)))
              (cond ((eof-object? c) '())
                    (else (cons c (loop)))))))))

(define (has-syntax-error? ast)
  (eq? (car ast) 'syntax-error))

(define (has-flag? flag-symbol flags)
  (cond ((null? flags)
         #f)
        ((eq? flag-symbol (car flags))
         #t)
        (else
          (has-flag? flag-symbol flags))))

(define (flag-print flag-symbol flags xs)
  (when (has-flag? flag-symbol flags)
    (pp xs))
  xs)

(define (analysis-passes ast dump-flags)
  ;; TODO
  (define types (flag-print 'defined-types dump-flags
                            (find-composite-types ast)))

  (print "==> TYPES: ")
  (pp types)
  ast)

;; find-composite-*: these functions find type definitions on the top level,
;;                   including structs/enums, function definitions and
;;                   declarators, and typedefs.
;;                   they kinda do a lot, but basically at the end of
;;                   find-composite-types here should have a nice list of
;;                   (global) types in the program.
;;
;; TODO: also need to find types in function definitions, but doesn't make
;;       sense to find them in this collection of functions since the defintions
;;       are limited to the block scope
;;
;; TODO: maybe move this to another file
(define (find-composite-types ast)
  (assert (is-translation-unit? ast))

  (append 
    (find-composite-types-extern-decl (caadr ast))
    (if (null? (cdadr ast))
      '()
      (find-composite-types (car (cdadr ast))))))

(define (find-composite-types-extern-decl ast)
  (assert (is-external-declaration? ast))
  (let ((decl (caadr ast)))
    (cond ((is-function-definition? decl)
           ;(pp decl)
           '())
          ((is-declaration? decl)
           (find-composite-types-decl decl))
          (else (assert #f)))))

(define (find-composite-types-decl ast)
  (assert (is-declaration? ast))

  (let looper ((xs (cadr ast))
               (types '()))
    (cond
      ((null? xs)
       ;;(print "Reached end of declaration, unused types: " types)
       (find-composite-types-struct-union types))

      ((is-typedef-specifier? (car xs))
       (when (not (null? types))
         (print "^ NOTE: unused types " types))
       (find-composite-types-typedef xs))

      ((is-type-specifier? (car xs))
       (let ((type (car (cadar xs))))
         (print "^ type specifier " type)
         (looper (cdr xs)
                 (cons type types))))

      ((is-declarator? (car xs))
       (find-composite-types-declarator (car xs) types))

      (else
        (pp (car xs))
        (looper (cdr xs) types)))))

(define (find-composite-types-typedef ast)
  (assert (is-typedef-specifier? (car ast)))
  (pp ast)

  (let* ((rev      (reverse ast))
         (ident    (get-declarator-identifier (cadr rev))))
    (if (or (eq? (cadr rev) #\})
            (is-type-specifier? (cadr rev)))
      ;; TODO: output to stderr
      (begin
        (print "NOTE: useless empty typedef")
        '())

      ;; else
      (let* ((deftype  (get-defining-type ast))
             (stripped (strip-tag is-type-specifier? deftype)))
        (print "have typedef: " ident " => " stripped)
        (append `((,ident ,stripped))
                ;; TODO: recursive call for typedef structs
                (find-composite-types-struct-union (cadar deftype)))))))

(define (strip-tag typechecker? xs)
  (cond ((typechecker? xs) (strip-tag typechecker? (caadr xs)))
        ((list? xs) (map (lambda (x) (strip-tag typechecker? x)) xs))
        (else xs)))

(define (find-composite-types-declarator ast types)
  (print "> find-composite-types-declarator here")
  '())

(define (find-composite-types-struct-union ast)
  (pp (car ast))
  (cond
    ((not (list? ast)) '())

    ((> (length ast) 1)
     (print "ERROR: two or more data types in declaration specifiers")
     '())

    ((is-struct-or-union-specifier? (car ast))
     (let ((str (cadar ast)))
       (cond
         ((and (string=? (car str) "struct")
                (is-identifier? (cadr str)))
          `((,(string-append "struct " (caadr (cadr str))) ,str)))
         ((and (string=? (car str) "union")
               (is-identifier? (cadr str)))
          `((,(string-append "union " (caadr (cadr str))) ,str)))
         (else '()))))

    (else '())))
