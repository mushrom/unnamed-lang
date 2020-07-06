;; Direct translation of the grammar in annex A of ISO/IEC 9899:1999

;; TODO: things to fix:
;;     - typedefs with identifiers that begin with primitive types
;;       will fail to match (eg. int16_t)
(declare (unit parser)
         (uses ast-types))

(import abnf   
        (prefix abnf abnf:)
        (rename abnf
                (concatenation =>)
                (alternatives =bar)
                (optional-sequence =?)
                (lit =s)
                (char =c)
                (repetition =*)
                (repetition1 =+)))
(import lexgen (prefix lexgen lexgen:))
(import utf8-srfi-14)

;; parser that never matches (abnf:pass always matches)
(define null-parser (abnf:set char-set:empty))

;; XXX: dynamically build a parser as typedef declarations are encountered.
;;      This is reset on a call to the top-level parser
(define *typedef-defined-names* null-parser)
(define (reset-defined-names) (set! *typedef-defined-names* null-parser))

;; recursion helper from json-abnf
(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))

(define-syntax define-tagged-rule
  (syntax-rules ()
    ((_ name body ...)
     (define name
       (bind-tag 'name
           (vac body ...))))))

(define-syntax define-debug-rule
  (syntax-rules ()
    ((_ name body ...)
     (define name
       (bind-tag 'name
           (vac (begin
                  (print "trying " 'name)
                  body ...)))))))

(define-syntax define-rule
  (syntax-rules ()
    ((_ name body ...)
     (define name
       (vac body ...)))))

(define-syntax debug-stub
  (syntax-rules ()
    ((_ fn)
     (lambda args
       (print "got here")
       (apply fn args)))))

;; translate character streams into scheme values
(define (bind-tag t p)
  (abnf:bind (lambda (x) `((,t ,(reverse x))))
             p))

(define (bind->integer p)
  (abnf:bind (lambda (x)
               (list (string->number (list->string (reverse x)))))
             p))

(define (bind->string p)
  (abnf:bind (lambda (x)
               (list (list->string (reverse x))))
             p))

(define (=s-bind str)
  (bind->string (=s str)))

;; TODO: better errors, this is unused
(define (error-check msg matcher)
  (lambda args
    (let ((res (apply matcher args)))
      (when (and (not (null? res)) (eq? (car res) 'error))
             (print msg))
      res)))

(define ws
    (abnf:drop-consumed
      (=* (=bar wsp crlf lf cr))))

(define ws-included
  (=? (=* (=bar wsp crlf lf cr))))

(define (wssep ch)
  (=> ws (=c ch) ws))

(define (wssep-str ch)
  (=> ws (bind->string (=s ch)) ws))

(define (wssep-proc matcher)
  (=> ws matcher ws))

(define identifier-char-old
  (lexgen:bar
    (lexgen:bar (lexgen:range #\a #\z) (lexgen:range #\A #\Z))
    (lexgen:char #\_)))

(define identifier-char
  (=bar (abnf:set  char-set:letter)
        (abnf:char #\_)))

;; A.1 Lexical grammar
(define digit
  (abnf:set char-set:digit))

(define nonzero-digit
  (abnf:set "123456789"))

(define octal-digit
  (abnf:set "01234567"))

(define hexadecimal-digit
  (abnf:set char-set:hex-digit))

(define hexadecimal-prefix
  (=bar (=s "0x")
        (=s "0X")))

(define-tagged-rule identifier
  (bind->string 
    (=> identifier-char
        (=? (=+ (=bar digit identifier-char))))))

(define unsigned-suffix  (lexgen:set "uU"))
(define long-suffix      (lexgen:set "lL"))
(define long-long-suffix (lexgen:bar (lexgen:lit "ll") (lexgen:lit "LL")))

(define integer-suffix
  (=bar (lexgen:seq unsigned-suffix long-long-suffix)
        (lexgen:seq unsigned-suffix (lexgen:opt long-suffix))
        (lexgen:seq long-long-suffix (lexgen:opt unsigned-suffix))
        (lexgen:seq long-suffix (lexgen:opt unsigned-suffix))))

(define-tagged-rule decimal-constant
  (=> (bind->integer (=+ digit))))

(define-tagged-rule octal-constant
  ;; TODO: bind->octal-integer
  (=> (=c #\0) (=* octal-digit)))

(define-tagged-rule hexadecimal-constant
  ;; TODO: bind->hex-integer
  (=> hexadecimal-prefix (=+ hexadecimal-digit)))

(define-tagged-rule integer-constant
  (=bar (=> hexadecimal-constant (=? integer-suffix))
        (=> octal-constant (=? integer-suffix))
        (=> decimal-constant (=? integer-suffix))))

(define-tagged-rule floating-constant
  (=bar decimal-floating-constant
        ;; TODO
        ;hexadecimal-floating-constant
        ))

(define-rule decimal-floating-constant
  (=bar (=> fractional-constant (=? exponent-part) (=? floating-suffix))
        (=> digit-sequence exponent-part (=? floating-suffix))))

(define-rule fractional-constant
  (=bar (=> (wssep #\.) digit-sequence)
        (=> digit-sequence (wssep #\.) (=? digit-sequence))))

(define-rule exponent-part
  (=> (=bar (wssep-str "e") (wssep-str "E")) (=? sign) digit-sequence))

(define-rule sign
  (wssep-proc
    (=bar (=c #\+)
          (=c #\-))))

(define-rule digit-sequence
  (=> digit (=? digit-sequence)))

(define-rule floating-suffix
  (wssep-proc
    (=bar (=c #\f)
          (=c #\l)
          (=c #\F)
          (=c #\L))))

(define enumeration-constant identifier)
(define-tagged-rule character-constant
  (=> (=c #\')
      (=bar (=> (=c #\\) (abnf:set char-set:full))
            (abnf:set char-set:full))
      (=c #\')))

(define-tagged-rule constant
  (=bar floating-constant
        integer-constant
        ; TODO
        enumeration-constant
        character-constant
        ))

(define foo (char-set-delete char-set:full #\"))

(define-tagged-rule string-lit
  (=> (=c #\") (=* (abnf:set foo)) (=c #\")))

;; TODO
(define-tagged-rule expression
  (=bar (=> assignment-expression (=? (=> (wssep #\,) expression)))))

;; phrase structure (A.2)

;; these functions prevent an excessive number of tags being applied when
;; parsing expressions, if an expression doesn't parse more than it's first
;; item then there's nothing gained by adding a tag
(define (bind-tag-when-expanded tag proc)
  (abnf:bind (lambda (x) (if (> (length x) 1)
                           `((,tag ,(reverse x)))
                           x))
             proc))

(define-syntax define-collapsable-rule
  (syntax-rules ()
    ((_ name body)
     (define name
       (bind-tag-when-expanded 'name
         (vac (begin body)))))))

(define-collapsable-rule primary-expression
  (=bar (=> (wssep #\() expression (wssep #\)))
        constant
        string-lit
        identifier))

(define-collapsable-rule argument-expr-list
  (=bar (=> assignment-expression (=? (=> (wssep #\,) argument-expr-list)))))

(define-collapsable-rule postfix-primary
  (=bar (=> (wssep #\() type-name (wssep #\))
            (wssep #\{) initializer-list (wssep #\})
            (=? (wssep #\,)))
        (=> primary-expression
            (=? (=bar (=> (wssep #\[) expression (wssep #\]))
                      (=> (wssep #\() (=? argument-expr-list) (wssep #\))))))))

;(define postfix-expression
;  (debug-stub
;    (vac
;    (=bar (=> postfix-primary postfix-expression)
;          (=> postfix-primary (wssep #\.)      identifier)
;          (=> postfix-primary (wssep-str "->") identifier)
;          (=> postfix-primary (wssep-str "++"))
;          (=> postfix-primary (wssep-str "--"))
;          postfix-primary))))

(define-collapsable-rule postfix-expression
  (=> postfix-primary
      ;; TODO: for some reason this goes into an infinite recursion...
      ;; FIXME
      ;(=? postfix-expression)
      (=? (=bar (=> (wssep #\.) identifier)
                (=> (wssep-str "->") identifier)
                (=> (wssep-str "++"))
                (=> (wssep-str "--"))))))

;; TODO
;;(define postfix-expression postfix-primary)
;(define postfix-expression primary-expression)

(define unary-operator
  (wssep-proc
    (=bar
      (=c #\&)
      (=c #\*)
      (=c #\+)
      (=c #\-)
      (=c #\~)
      (=c #\!))))

(define assignment-operator
  (wssep-proc
    (=bar
      (=s "=")
      (=s "*=")
      (=s "/=")
      (=s "%=")
      (=s "+=")
      (=s "-=")
      (=s "<<=")
      (=s ">>=")
      (=s "&=")
      (=s "^=")
      (=s "|="))))

(define-collapsable-rule cast-expression
  (=bar
    (bind-tag 'cast-expression
              (=> (wssep #\() type-name (wssep #\)) cast-expression))
    unary-expression))

(define increment-op
  (wssep-proc
    (=bar
      (=s "++")
      (=s "--"))))

(define-collapsable-rule unary-expression
  (=bar
    (bind-tag 'unary-expression
              (=bar
                (=> increment-op unary-expression)
                (=> unary-operator cast-expression)))
    postfix-expression))

(define multiply-op
  (wssep-proc
    (=bar (=c #\*)
          (=c #\/)
          (=c #\%))))

(define-collapsable-rule multiplicative-expression
  (=> cast-expression
      (=? (=> multiply-op multiplicative-expression))))

(define add-op
  (wssep-proc
    (=bar (=c #\+)
          (=c #\-))))

(define-collapsable-rule additive-expression
  (=> multiplicative-expression
      (=? (=> add-op additive-expression))))

(define shift-op
  (wssep-proc
    (=bar (=s "<<")
          (=s ">>"))))

(define-collapsable-rule shift-expression
  (=> additive-expression
      (=? (=> shift-op shift-expression))))

(define relational-op
  (wssep-proc
    (=bar (=s "<=")
          (=s ">=")
          (=s "<")
          (=s ">"))))

(define equality-op
  (wssep-proc
    (=bar (=s "==")
          (=s "!="))))

(define-collapsable-rule relational-expression
  (=> shift-expression
      (=? (=> relational-op shift-expression))))

(define-collapsable-rule equality-expression
  (=> relational-expression
      (=? (=> equality-op equality-expression))))

(define-collapsable-rule AND-expression
  (=> equality-expression
      (=? (=> (wssep-str "&") AND-expression))))

(define-collapsable-rule exclusive-OR-expression
  (=> AND-expression
      (=? (=> (wssep-str "^") exclusive-OR-expression))))

(define-collapsable-rule inclusive-OR-expression
  (=> exclusive-OR-expression
      (=? (=> (wssep-str "|") inclusive-OR-expression))))

(define-collapsable-rule logical-AND-expression
  (=> inclusive-OR-expression
      (=? (=> (wssep-str "&&") logical-AND-expression))))

(define-collapsable-rule logical-OR-expression
  (=> logical-AND-expression
      (=? (=> (wssep-str "||") logical-OR-expression))))

(define-collapsable-rule conditional-expression
  (=> logical-OR-expression
      (=? (=> (wssep #\?) expression
              (wssep #\:) conditional-expression))))

(define-collapsable-rule assignment-expression
  (=bar (=> unary-expression assignment-operator assignment-expression)
        conditional-expression))

(define-tagged-rule constant-expression conditional-expression)

(define (is-typedef-specifier? x)
  (equal? x '(storage-class-specifier ("typedef"))))

(define (get-defining-type typedef)
  ;; find the elements in [1, end-2]
  (let loop ((ys (cdr typedef)))
    (cond ((null? (cdddr ys))
           (list (car ys)))
          (else
            (cons (car ys) (loop (cdr ys)))))))

(define (find-identifier xs)
  (cond ((null? xs) '())
        ((and (list? xs) (is-identifier? xs))
         xs)
        ((list? xs)
         (let ((k (find-identifier (car xs))))
           (if (eq? k '())
             (find-identifier (cdr xs))
             k)))
        (else '())))

(define (get-declarator-identifier x)
  (if (is-declarator? x)
    (let ((rev (find-identifier x)))
      (caadr rev))
    "#<invalid>"))

(define (add-typedef-declaration ident)
  (print "adding matcher for '" ident "'")
  (set! *typedef-defined-names*
    (=bar (=s-bind ident) *typedef-defined-names*)))

(define (handle-typedef-declaration xs)
  (let ((rev (reverse xs)))
        (when (is-typedef-specifier? (car rev))
          (let ((ident (get-declarator-identifier (cadr xs))))
            ;(print "have a type definition! " (cadr rev) " => " (cadr xs))
            (add-typedef-declaration ident)))

        ;; return the list unscathed
        xs))

;; Declarations
;; TODO: would it be more efficient to have the bind after tagging?
;;       avoid redundantly reversing the list
(define-tagged-rule declaration
  (abnf:bind handle-typedef-declaration
    (=> declaration-specifiers (=? init-declarator-list) (wssep #\;))))

(define-rule declaration-specifiers
  (=bar (=> storage-class-specifier (=? declaration-specifiers))
        (=> type-specifier          (=? declaration-specifiers))
        (=> type-qualifier          (=? declaration-specifiers))
        (=> function-specifier      (=? declaration-specifiers))))

(define-rule init-declarator-list
  (=> declarator (=? (=> (wssep #\=) initializer))))

(define-tagged-rule storage-class-specifier
  (bind->string
    (wssep-proc
      (=bar (=s "typedef")
            (=s "extern")
            (=s "static")
            (=s "auto")
            (=s "register")))))

(define typedef-name identifier)

(define-tagged-rule type-specifier
  (wssep-proc
    (=bar (=s-bind "void")
          (=s-bind "char")
          (=s-bind "short")
          (=s-bind "int")
          (=s-bind "long")
          (=s-bind "float")
          (=s-bind "double")
          (=s-bind "signed")
          (=s-bind "unsigned")
          (=s-bind "_Bool")
          (=s-bind "_Complex")
          struct-or-union-specifier
          enum-specifier
          *typedef-defined-names*)))

(define-tagged-rule struct-or-union-specifier
  (=> struct-or-union
      (=bar (=> (=? identifier)
                (wssep #\{) struct-declaration-list (wssep #\}))
            (=> identifier))))

(define-rule struct-or-union
  (bind->string 
    (wssep-proc
      (=bar (=s "struct")
            (=s "union")))))

(define-tagged-rule struct-declaration-list
  (=> struct-declaration (=? struct-declaration-list)))

(define-tagged-rule struct-declaration
  (=> specifier-qualifier-list struct-declarator-list (wssep #\;)))

(define-tagged-rule specifier-qualifier-list
  (=+ (=bar type-specifier type-qualifier)))

(define-tagged-rule struct-declarator-list
  (=> struct-declarator (=? (=> (wssep #\,) struct-declarator-list))))

(define-rule struct-declarator
  (=bar declarator
        (=> (=? declarator) (wssep #\:) constant-expression)))

(define-tagged-rule enum-specifier
  (=> (wssep-str "enum")
      (=bar
        ;; TODO: can do this without repeating initial identifier?
        ;;       (for performance)
        (=> (=? identifier) (wssep #\{) enumerator-list (wssep #\,) (wssep #\}) )
        (=> (=? identifier) (wssep #\{) enumerator-list (wssep #\}))
        identifier)))

(define-rule enumerator-list
  (=> enumerator (=? (=> (abnf:drop-consumed (wssep #\,)) enumerator-list))))

(define-tagged-rule enumerator
  (=> enumeration-constant (=? (=> (wssep #\=) constant-expression))))

(define-tagged-rule type-qualifier
  (bind->string
    (=bar (=s "const")
          (=s "restrict")
          (=s "volatile"))))

(define function-specifier (bind->string (=s "inline")))

(define-tagged-rule declarator
  (=> (=? pointer) direct-declarator))

(define-tagged-rule direct-declarator
  (=> (=bar identifier
            (=> (wssep #\() declarator (wssep #\))))
      (=? direct-declarator)
      (=? (=bar 
            (=> (wssep #\[) type-qualifier-list (wssep #\*) (wssep #\]))
            (=> (wssep #\[)
                type-qualifier-list (wssep-str "static") assignment-expression
                (wssep #\]))
            (=> (wssep #\[)
                (wssep-str "static")
                (=? type-qualifier-list) assignment-expression
                (wssep #\]))
            (=> (wssep #\[)
                (=? type-qualifier-list) (=? assignment-expression)
                (wssep #\]))
            (=> (wssep #\() parameter-type-list (wssep #\)))
            (=> (wssep #\() (=? identifier-list) (wssep #\)))))))

(define-tagged-rule pointer
  (=bar (=> (wssep #\*)
            (=? type-qualifier-list)
            (=? pointer))))

(define-tagged-rule type-qualifier-list
  (=bar (=> type-qualifier (=? type-qualifier-list))))

(define-tagged-rule parameter-type-list
  (=> parameter-list (=? (=> (wssep #\,) (wssep-str "...")))))

(define-tagged-rule parameter-list
    (=> parameter-declaration (=? (=> (wssep #\,) parameter-list))))

(define-tagged-rule parameter-declaration
  (=> declaration-specifiers (=bar declarator
                                   (=? abstract-declarator))))

(define-tagged-rule identifier-list
  (=> identifier (=? (=> (wssep #\,) identifier-list))))

(define-tagged-rule type-name
  (=> specifier-qualifier-list (=? abstract-declarator)))

(define-tagged-rule direct-abstract-declarator
  (=bar
    (=> (wssep #\() abstract-declarator (wssep #\)))))

(define-tagged-rule abstract-declarator
  (=bar (=> (=? pointer) direct-abstract-declarator)
        pointer))

(define-tagged-rule initializer
  (=bar (=> (wssep #\{) initializer-list
            (=? (wssep #\,))
            (wssep #\}))
        (=> (wssep #\{) initializer-list (wssep #\}))
        assignment-expression))

(define-tagged-rule initializer-list
  (=bar (=> (=? designation) initializer
            (=? (=> (wssep #\,) initializer-list)))))

(define-tagged-rule designation
  (=> designator (wssep #\=)))

(define-tagged-rule  designator
  (=bar
    (=> (wssep #\[) constant-expression (wssep #\]))
    (=> (wssep #\.) identifier)))

(define-tagged-rule designator-list
  (=> designator (=? designator-list)))

;; A.2.3 Statements
(define-tagged-rule statement
  (=bar labeled-statement
        compound-statement
        expression-statement
        selection-statement
        iteration-statement
        jump-statement))

(define-tagged-rule labeled-statement
  (=bar (=> identifier (wssep #\:) statement)
        (=> (wssep-str "case")
            constant-expression (wssep #\:) statement)
        (=> (wssep-str "default")
            (wssep #\:) statement)))

(define-tagged-rule compound-statement
    (=> (wssep #\{) (=? block-item-list) (wssep #\})))

(define-tagged-rule block-item-list
  (=> block-item (=? block-item-list)))

(define-tagged-rule block-item
  (=bar declaration
        statement))

(define-tagged-rule expression-statement
  (=> (=? expression) (wssep #\;)))

(define-tagged-rule selection-statement
  (=bar (=> (wssep-str "if") (wssep #\() expression (wssep #\))
            statement (=? (=> (wssep-str "else") statement)))
        (=> (wssep-str "switch") (wssep #\() expression (wssep #\))
            statement)))

(define-tagged-rule iteration-statement
  (=bar
    (=> (wssep-str "while") (wssep #\() expression (wssep #\))
        statement)
    (=> (wssep-str "do") statement  (wssep-str "while")
        (wssep #\() expression (wssep #\)) (wssep #\;))
    (=> (wssep-str "for")
        (wssep #\() declaration (=? expression) (wssep #\;) (=? expression)
        (wssep #\))
        statement)
    (=> (wssep-str "for")
        (wssep #\() (=? expression) (wssep #\;)
        (=? expression) (wssep #\;) (=? expression) (wssep #\))
        statement)))

(define-tagged-rule jump-statement
  (=bar (=> (wssep-str "goto") identifier (wssep #\;))
        (=> (wssep-str "continue") (wssep #\;))
        (=> (wssep-str "break") (wssep #\;))
        (=> (wssep-str "return") (=? expression))))

;; A.2.4 External definitions
(define-tagged-rule translation-unit
  (=bar (=> external-declaration (=? translation-unit))))

(define-tagged-rule external-declaration
  (=bar function-definition
        declaration))

(define-tagged-rule function-definition
  (=> declaration-specifiers declarator
      (=? declaration-list) compound-statement))

(define-tagged-rule declaration-list
  (=> declaration declaration-list))

;; top level, return translation-units
(define wutunnamed-program
  (bind-tag 'top-level
            (=bar
              storage-class-specifier
              type-specifier)))

(define unnamed-program
  (bind-tag 'top-level
    (error-check "Expected a thing"
                 translation-unit)))

(define (err s)
  ;(print "Syntax error on stream: " s)
  (begin 
    (display "syntax error, at/after: ")
    (let loop ((unmatched (cadr s)))
      (when (not (or (null? unmatched)
                     (eq? (car unmatched) #\newline)))
        (display (car unmatched))
        (loop (cdr unmatched))))
    (newline)
    (print   "                        ^ TODO: better error message"))
  `(syntax-error error-message-here ,s))

;; for some reason lexgen doesn't export this
(define *eof-token* (read (open-input-string "")))

;; matches on end of stream
(define (eof-token)
  (lambda (sk fk strm)
    (cond ((equal? (cadr strm) *eof-token*) (sk strm))
          ((null? (cadr strm))              (sk strm))
          (else                             (fk strm)))))

;; main parsing function
(define (parse-c-top-level chars)
  (reset-defined-names)
  (let* ((p (bind-tag 'top-level (=> translation-unit)))
         (result (lex p err (list '() chars))))
    (cond ((or (null? (cadr result))
               (equal? (cadr result) *eof-token*)) result)
          ((is-syntax-error? result) result)
          (else (err result)))))
