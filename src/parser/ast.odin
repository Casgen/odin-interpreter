package parser

import tok "../token"

// Holds a name for an identifier (for ex. 'x' or 'value')
// TODO: Eventually we have to somehow deal with one source of truth of 
// these identifiers. Probably with a map.
Identifier :: struct {
	token: ^tok.Token,
}

// Struct for holding an integer value (for ex. 12)
IntegerLiteral :: struct {
    token: ^tok.Token,
    value: i64
}

// Struct for holding a bool value.
Boolean :: struct {
    token: ^tok.Token,
    value: bool
}

/*
Struct for holding a series of statements which are enclosed in a scope
(meaning by braces) for ex.:

```
//...
{
    let x = 6;
    let y = 4;

    result = x + y;
}
//...
*/
BlockStatement :: struct {
    token: ^tok.Token,
    statements: []Statement
}

FunctionLiteral :: struct {
    token: ^tok.Token,
    params: []Identifier,
    body: ^BlockStatement,
}

/*
object for holding expressions.

Expression can hold the following:
- Identifier (for ex. "foobar") which can be evaluated
- Equation (for ex. "5 + 5")
- Function call (for ex. add(5, 5))
*/

Expression :: union {
    ^Identifier,
    ^IntegerLiteral,
    ^PrefixExpression,
    ^InfixExpression,
    ^Boolean,
    ^IfExpression,
    ^FunctionLiteral,
}

ExpressionStatement :: struct {
    token:  ^tok.Token,
    expr:   Expression,
}

// object for holding unary expressions, for ex. (-5)
PrefixExpression :: struct {
    token:      ^tok.Token,
    operator:   string,
    right:      Expression
}

// object for holding binary expressions, for ex. (5 * 2)
InfixExpression :: struct {
    token:          ^tok.Token,
    operator:       string,
    left, right:    Expression,
}

IfExpression :: struct {
    token: ^tok.Token,
    condition: Expression,
    consequence: ^BlockStatement,
    alternative: ^BlockStatement,
}

Statement :: union {
    ^ReturnStatement,        // example: return 5;
    ^LetStatement,           // example: let x = 6;
    ^ExpressionStatement,    // example: x + 10;
}

ReturnStatement :: struct {
    token: ^tok.Token,
    value: Expression,
}

LetStatement :: struct {
    token:  ^tok.Token,
    // use it as a pointer to have only one existing identifier!
	ident:  ^Identifier,
	value:  Expression,
}

