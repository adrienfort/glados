# AST
```haskell
-- data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast]
data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast] | Define [String] Ast
```

Change AST string new_value

(define a 1)
(define a 2)



The function translated :
```lisp
(define (> a b)
    (if (eq? a b)
        #f
        (if (< a b)
            #f
            #t
        )
    )
)
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