package parser

import "core:strings"
import "core:fmt"

import tok "../token"
import "../utils"

// Holds a name for an identifier (for ex. 'x' or 'value')
Identifier :: struct {
	token: ^tok.Token,
}

// Struct for holding an integer value (for ex. 12)
IntegerLiteral :: struct {
    token: ^tok.Token,
    value: i64
}

// Struct for holding literal string values:
// for ex. "foo bar"
StringLiteral :: struct {
    token: ^tok.Token,
    value: string,
}

// Struct for holding a bool value.
Boolean :: struct {
    token: ^tok.Token,
    value: bool
}

/*
Struct for holding a call expression
for ex.:
- "add(2, 3);"
- "fn(x, y) {return x + y;}(2, 3);"
- "add(2 * 2, 3 * 3);"
*/
CallExpression :: struct {
    token: ^tok.Token,
    function: Expression,
    arguments: []Expression
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
/*
Struct for holding a definition of a function.
for ex.:

```
fn(x, y) { return a + b; }
```

*/
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
    ^StringLiteral,
    ^PrefixExpression,
    ^InfixExpression,
    ^Boolean,
    ^IfExpression,
    ^FunctionLiteral,
    ^CallExpression,
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

// Creates a deep copy
copy_token :: proc(token: ^tok.Token) -> ^tok.Token {
    token_literal_copy, err := strings.clone(token.literal)
    fmt.assertf(err == .None,
        "Failed to clone token literal string '%v'!", err)

    token_copy := new(tok.Token)
    token_copy.type = token.type
    token_copy.literal = token_literal_copy

    return token_copy
}

free_token :: proc(token: ^tok.Token) {
    delete(token.literal)
    free(token)
}


// Creates a deep copy
copy_block_statement :: proc(block: ^BlockStatement) -> ^BlockStatement {
    
    block_copy := new(BlockStatement)
    block_copy.token = copy_token(block.token)
    block_copy.statements = copy_statements(block.statements)
    
    return block_copy
}

free_block_statement :: proc(block: ^BlockStatement) {

    free_token(block.token)
    free_statements(block.statements)

    free(block)
}

copy_identifier :: proc(ident: ^Identifier) -> ^Identifier{
    copy_ident := new(Identifier)
    copy_ident.token = copy_token(ident.token)
    
    return copy_ident
}

free_identifier :: proc(ident: ^Identifier) {
    free_token(ident.token)
    free(ident)
}

copy_identifiers :: proc(idents: []Identifier) -> []Identifier {
    idents_copy := make([]Identifier, len(idents))

    for ident, i in idents {
        idents_copy[i] = Identifier{ token = copy_token(ident.token) }
    }

    return idents_copy
}

free_identifiers :: proc(idents: []Identifier) {
    for ident, i in idents {
        free_token(ident.token)
    }

    delete(idents)
}

// Creates a deep copy
copy_expression :: proc(expr: Expression) -> Expression {
    switch variant in expr {
    case ^Identifier: return copy_identifier(variant)
    case ^IntegerLiteral:
        literal := new(IntegerLiteral)
        literal.token = copy_token(variant.token)
        literal.value = variant.value

        return literal
    case ^StringLiteral:
        literal := new(StringLiteral)
        literal.token = copy_token(variant.token)
        literal.value = strings.clone(variant.value)

        return literal
    case ^PrefixExpression:
        prefix_op_clone, err := strings.clone(variant.operator)
        fmt.assertf( err == .None,
            "Failed to copy operator string!", err)

        prefix_expr := new(PrefixExpression)
        prefix_expr.token = copy_token(variant.token)
        prefix_expr.operator = prefix_op_clone
        prefix_expr.right = copy_expression(variant.right)


        return prefix_expr
    case ^InfixExpression:
        operator_clone, err := strings.clone(variant.operator)
        
        fmt.assertf( err == .None,
            "Failed to copy operator string!", err)

        infix_expr := new(InfixExpression)
        infix_expr.token = copy_token(variant.token)
        infix_expr.operator = operator_clone
        infix_expr.right = copy_expression(variant.right)
        infix_expr.left = copy_expression(variant.left)

        return infix_expr
    case ^Boolean:
        boolean := new(Boolean)
        boolean.token = copy_token(variant.token)
        boolean.value = variant.value

        return boolean
    case ^IfExpression:
        condition_copy := copy_expression(variant.condition)
        alternative_copy := copy_block_statement(variant.alternative)
        consequence_copy := copy_block_statement(variant.consequence)

        if_expr := new(IfExpression)
        if_expr.token = copy_token(variant.token)
        if_expr.condition = condition_copy
        if_expr.alternative = alternative_copy
        if_expr.consequence = consequence_copy

        return if_expr
    case ^FunctionLiteral:
        params_copy := make([]Identifier, len(variant.params))

        for par, i in variant.params {
            ident := Identifier{token = copy_token(par.token)}
            params_copy[i] = ident
        }

        fn_literal := new(FunctionLiteral)
        fn_literal.token = copy_token(variant.token)
        fn_literal.body = copy_block_statement(variant.body)
        fn_literal.params = params_copy
        
        return fn_literal
    case ^CallExpression:

        args_copy := make([]Expression, len(variant.arguments))

        for arg, i in variant.arguments {
            args_copy[i] = copy_expression(arg)
        }

        call_expr := new(CallExpression)
        call_expr.arguments = args_copy
        call_expr.function = copy_expression(variant.function)
        call_expr.token = copy_token(variant.token)

        return call_expr
    }

    panic("Unhandled Case of expression!")
}

free_expression :: proc(expr: Expression) {
    switch variant in expr {
    case ^Identifier:
        free_identifier(variant)
        return
    case ^IntegerLiteral:
        free_token(variant.token)
        free(variant)
        return
    case ^StringLiteral:
        free_token(variant.token)
        delete(variant.value)
        free(variant)
        return
    case ^PrefixExpression:
        free_token(variant.token)
        delete(variant.operator)
        free_expression(variant.right)

        free(variant)
        return
    case ^InfixExpression:
        free_token(variant.token)
        delete(variant.operator)
        free_expression(variant.right)
        free_expression(variant.left)

        free(variant)
        return
    case ^Boolean:
        free_token(variant.token)
        free(variant)
        return
    case ^IfExpression:
        free_token(variant.token)
        free_block_statement(variant.consequence)
        free_block_statement(variant.alternative)
        free_expression(variant.condition)

        free(variant)
        return
    case ^FunctionLiteral:
        for par, i in variant.params {
            free_token(par.token)
        }
        delete(variant.params)

        free_token(variant.token)
        free_block_statement(variant.body)

        free(variant)
        return
    case ^CallExpression:
        for arg in variant.arguments {
            copy_expression(arg)
        }

        free_expression(variant.function)
        free_token(variant.token)

        free(variant)
        return
    }

    panic("Unhandled Case of expression!")
}

// Creates a deep copy
copy_statements :: proc(stmts: []Statement) -> []Statement {
    stmts_copy := make([]Statement, len(stmts))

    for stmt, i in stmts {
        stmts_copy[i] = copy_statement(stmt)
    }

    return stmts_copy
}

free_statements :: proc(stmts: []Statement) {

    for stmt in stmts {
        free_statement(stmt)
    }

    delete(stmts)
}

// Creates a deep copy
copy_statement :: proc(stmt: Statement) -> Statement {
    switch variant in stmt {
    case ^ReturnStatement:
        ret_stmt := new(ReturnStatement)
        ret_stmt.token = copy_token(variant.token) // Pass in the whole struct.
        ret_stmt.value = copy_expression(variant.value)

        return ret_stmt
    case ^LetStatement:
        let_stmt := new(LetStatement)
        let_stmt.value = copy_expression(variant.value)
        let_stmt.token = copy_token(variant.token)
        let_stmt.ident = copy_identifier(variant.ident)

        return let_stmt
    case ^ExpressionStatement:
        expr_stmt := new(ExpressionStatement)
        expr_stmt.token = copy_token(variant.token)
        expr_stmt.expr = copy_expression(variant.expr)

        return expr_stmt
    }

    panic("Unhandled Statement case!")
}

free_statement :: proc(stmt: Statement) {
    switch variant in stmt {
    case ^ReturnStatement:
        free_expression(variant.value)
        free_token(variant.token)

        free(variant)
    case ^LetStatement:
        free_expression(variant.value)
        free_token(variant.token)
        free_identifier(variant.ident)

        free(variant)
    case ^ExpressionStatement:
        free_token(variant.token)
        free_expression(variant.expr)

        free(variant)
    }
}

