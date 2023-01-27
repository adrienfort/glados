# AST
```haskell
-- data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast]
data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast] | Func String [String] Ast

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
    )
)
```
Giving us :
```
AST:
    - Call(define):
        - Call(>):
            - Symbol(a)
            - Symbol(b)
        - Call(if):
            - Call(eq?)
                - Symbol(a)
                - Symbol(b)
            - Bolean(#f)
            - Call (if):
                - Call(<):
                    - Symbol(a)
                    - Symbol(b)
                - Bolean(#t)
                - Bolean(#t)
```