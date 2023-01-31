# AST
```haskell
-- data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast]
data Ast = Integer Int | Symbol String | Call String [Ast] | Define [String] Ast
```

The function translated :
```lisp
(define (> a b)
    (if (eq? a b)
        #f
        (if (< a b)
            #f
            #t
        )
    ))

(define x (lambda (a b) (+ a b)) )
(x 1 2) ; = 3

(define y ((lambda (a b) (+ a b)) 1 2) ) ; y = 3
```
Giving us :
```
AST:
    - Define [>, a, b]:
        - Call (if):
            - Call (eq?):
                - Symbol (a)
                - Symbol (b)
            - Boolean(#f == 0)
            - Call (if):
                - Call (<):
                    - Symbol (a)
                    - Symbol (b)
                - Boolean (#f == 0)
                - Boolean (#t == 1)
```