<program> ::= <statement> | <statement> <program>

<statement> ::= <import> | <assignment> | <expression> | <comment>

<import> ::= "import" <filename>

<filename> ::= <string>

<assignment> ::= <identifier> "=" <expression> | <identifier> "define" <expression>

<expression> ::= <infix-expression> | <identifier> | <number>

<infix-expression> ::= "(" <expression> <operator> <expression> ")"

<operator> ::= "+" | "add" | "-" | "min" | "*" | "mul" | "/" | "div" | "%" | "mod" | "==" | "eq?" | "<" | ">"

<identifier> ::= <string>

<number> ::= <integer> | <float>

<integer> ::= <digit>+

<float> ::= <digit>+ "." <digit>+

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<string> ::= '"' <char>* '"'

<char> ::= <letter> | <digit> | <symbol>

<letter> ::= "a" | "b" | "c" | ... | "z" | "A" | "B" | "C" | ... | "Z"

<symbol> ::= "-" | "_" | "." | "?" | "!"

<comment> ::= "#" <char>* "\n"
