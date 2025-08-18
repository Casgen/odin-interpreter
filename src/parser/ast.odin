package parser

import tok "../token"

Identifier :: struct {
	token: tok.Token,
}

IntegerLiteral :: struct {
    token: ^tok.Token,
    value: i64
}

Boolean :: struct {
    token: ^tok.Token,
    value: bool
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

