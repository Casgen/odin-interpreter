#+feature dynamic-literals
package parser

import "core:testing"
import "core:fmt"
import "../lexer"
import tok "../token"
import "../parser"
import "core:log"
import "core:strconv"
import "core:mem/virtual"
import "../arena_utils"
import "core:reflect"

test_literal_expression :: proc{
    test_literal_expression_i64,
    test_literal_expression_i32,
    test_literal_expression_identifier,
}

test_literal_expression_i64 :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    expected: i64
) -> bool {
    return test_integer_literal(t, expr, expected)
}

test_literal_expression_i32 :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    expected: i32
) -> bool {
    return test_integer_literal(t, expr, i64(expected))
}

test_literal_expression_bool :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    expected: bool
) -> bool {
    return test_boolean_expression(t, expr, expected)
}

test_literal_expression_identifier :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    expected: string
) -> bool {
    ident, ok := expr.(^parser.Identifier)

    testing.expectf(t, ok,
        "Unexpected expression type! Expected parser.Identifier, got %v",
        reflect.union_variant_typeid(expr)) or_return

    return test_identifier(t, ident, expected)
}

test_identifier :: proc(
    t: ^testing.T,
    identifier: ^parser.Identifier,
    value: string
) -> bool {

    testing.expectf(t, identifier.token.literal == value,
        "Unexpected identifier name! Expected %s, got %s", value,
        identifier.token.literal) or_return

    return true
}

LiteralValue :: union {
    i32, i64, string, bool
}


test_infix_expression :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    left: LiteralValue,
    operator: string,
    right: LiteralValue,
) -> bool {

    infix_expr, ok := expr.(^parser.InfixExpression)

    testing.expectf(t, ok,
        "Unexpected Expression. Expected InfixExpression, got %v",
        reflect.union_variant_typeid(expr)) or_return

    switch left_variant in left {
    case i64:
        test_literal_expression_i64(
            t,
            infix_expr.left,
            left_variant
        ) or_return
    case i32:
        test_literal_expression_i32(
            t,
            infix_expr.left,
            left_variant
        ) or_return
    case string:
        test_literal_expression_identifier(
            t,
            infix_expr.left,
            left_variant
        ) or_return
    case bool:
        test_literal_expression_bool(
            t,
            infix_expr.left,
            left_variant
        ) or_return
    }

    testing.expectf(t, infix_expr.operator == operator,
        "Unexpected Infix operator. Expected %s, got %s", operator,
        infix_expr.operator) or_return

    switch right_variant in right {
    case i64:
        test_literal_expression_i64(
            t,
            infix_expr.right,
            right_variant
        ) or_return
    case i32:
        test_literal_expression_i32(
            t,
            infix_expr.right,
            right_variant
        ) or_return
    case string:
        test_literal_expression_identifier(
            t,
            infix_expr.right,
            right_variant
        ) or_return
    case bool:
        test_literal_expression_bool(
            t,
            infix_expr.right,
            right_variant
        ) or_return
    }

    return true
}

@(test)
test_booleans :: proc(t: ^testing.T) {
    BooleanTests :: struct {
        input: string,
        expected: string,
    }

    bool_tests := [?]BooleanTests{
        { input = "true", expected = "true" },
        { input = "false", expected = "false" },
        { input = "3 > 5 == false", expected = "((3 > 5) == false)" },
        { input = "3 < 5 == true", expected = "((3 < 5) == true)" },
    }

    for &test in bool_tests {

        par := parser.new_parser(test.input)
        defer parser.destroy_parser(par)

        program := parser.parse_program(par)
        defer parser.free_program(program)

        program_str := parser.get_program_string(program)
        defer delete(program_str)

        testing.expectf(t,
            program_str == test.expected,
            `Boolean test does not match!:
                Expected: '%s'
                Got: '%s'`,
            test.expected,
            program_str)
    }
}

test_boolean_expression :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    expected: bool,
) -> bool {

    bool_expr, ok := expr.(^parser.Boolean)

    testing.expectf(t, ok,
        "Expression is not of type 'Boolean'! got %v",
        reflect.union_variant_typeid(expr)) or_return

    testing.expectf(t, bool_expr.value == expected,
        "Unexpected boolean value: Expected %v, got %v",
        expected, bool_expr.value) or_return

    bool_str := expected ? "true" : "false"

    testing.expectf(t, bool_expr.token.literal == bool_str,
        "Unexpected boolean token literal: Expected %v, got %v",
        expected, bool_str) or_return

    bool_enum := expected ? tok.TokenType.True : tok.TokenType.False

    testing.expectf(t, bool_expr.token.type == bool_enum,
        "Unexpected boolean token type: Expected %v, got %v",
        expected, bool_enum) or_return

    return true
}

@(test)
test_statements :: proc(t: ^testing.T) {
    input := `let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y;
    };

    let result = add(five, ten);
    !-/*5;

    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;`
	tests := [?]tok.Token {
		{tok.TokenType.Let, "let"},
		{tok.TokenType.Identifier, "five"},
		{tok.TokenType.Assign, "="},
		{tok.TokenType.Integer, "5"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Let, "let"},
		{tok.TokenType.Identifier, "ten"},
		{tok.TokenType.Assign, "="},
		{tok.TokenType.Integer, "10"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Let, "let"},
		{tok.TokenType.Identifier, "add"},
		{tok.TokenType.Assign, "="},
		{tok.TokenType.Function, "fn"},
		{tok.TokenType.Left_Paren, "("},
		{tok.TokenType.Identifier, "x"},
		{tok.TokenType.Comma, ","},
		{tok.TokenType.Identifier, "y"},
		{tok.TokenType.Right_Paren, ")"},
		{tok.TokenType.Left_Brace, "{"},
		{tok.TokenType.Identifier, "x"},
		{tok.TokenType.Plus, "+"},
		{tok.TokenType.Identifier, "y"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Right_Brace, "}"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Let, "let"},
		{tok.TokenType.Identifier, "result"},
		{tok.TokenType.Assign, "="},
		{tok.TokenType.Identifier, "add"},
		{tok.TokenType.Left_Paren, "("},
		{tok.TokenType.Identifier, "five"},
		{tok.TokenType.Comma, ","},
		{tok.TokenType.Identifier, "ten"},
		{tok.TokenType.Right_Paren, ")"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Bang, "!"},
		{tok.TokenType.Minus, "-"},
		{tok.TokenType.Slash, "/"},
		{tok.TokenType.Asterisk, "*"},
		{tok.TokenType.Integer, "5"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Integer, "5"},
		{tok.TokenType.Lt, "<"},
		{tok.TokenType.Integer, "10"},
		{tok.TokenType.Gt, ">"},
		{tok.TokenType.Integer, "5"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.If, "if"},
		{tok.TokenType.Left_Paren, "("},
		{tok.TokenType.Integer, "5"},
		{tok.TokenType.Lt, "<"},
		{tok.TokenType.Integer, "10"},
		{tok.TokenType.Right_Paren, ")"},
		{tok.TokenType.Left_Brace, "{"},
		{tok.TokenType.Return, "return"},
		{tok.TokenType.True, "true"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Right_Brace, "}"},
		{tok.TokenType.Else, "else"},
		{tok.TokenType.Left_Brace, "{"},
		{tok.TokenType.Return, "return"},
		{tok.TokenType.False, "false"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Right_Brace, "}"},
		{tok.TokenType.Integer, "10"},
		{tok.TokenType.Equal, "=="},
		{tok.TokenType.Integer, "10"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.Integer, "10"},
		{tok.TokenType.Not_Equal, "!="},
		{tok.TokenType.Integer, "9"},
		{tok.TokenType.Semicolon, ";"},
		{tok.TokenType.EOF, ""},
	}

	l := lexer.new_lexer(input)
    defer lexer.destroy_lexer(l)

	for test in tests {
		tok := lexer.next_token(l)

        testing.expectf(t, tok.type == test.type,
            "Token types do not match! Expected %q, got %q",
            test.type, tok.type)

        testing.expectf(t, tok.literal == test.literal,
            "Token literals do not match! Expected %s, got %s",
            test.literal, tok.literal)
	}
}

@(test)
test_let_statements :: proc(t: ^testing.T) {

    LetStatementTest :: struct {
        input: string,
        expected_ident: string,
        expected_value: LiteralValue
    }
    input := `
    let x = 5;
    let y = 10;
    let foobar = 838383;
    `

    lex := lexer.new_lexer(input)
    par := parser.new_parser(lex)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)
    
    testing.expect(t, program != nil, "parse_program() returned nil!")
    testing.expectf(t, len(program.statements) == 3,
        `The number of program statements is not correct!
        Actual: %d != Expected: %d`, len(program.statements), 3)

    check_parser_errors(t, par)
}

check_parser_errors :: proc(t: ^testing.T, parser: ^parser.Parser) {

    if len(parser.errors) == 0 {
        return 
    }

    log.errorf("Parser has %d errors!:", len(parser.errors))

    for msg, _ in parser.errors {
        log.errorf(msg)
    }

    testing.fail(t)

}

@(test)
test_string :: proc(t: ^testing.T) {
    program := new(parser.Program)

    arena := virtual.Arena{}
    init_err := virtual.arena_init_static(&arena)

    testing.expectf(t, init_err == .None, "Failed to allocate an arena! %v",
        init_err)

    ident1, err_1 := arena_utils.push_struct(&arena, parser.Identifier{
        token = parser.alloc_token(&arena, tok.Token{
            literal = "myVar",
            type = .Identifier
        })
    })

    testing.expectf(t, err_1 == .None,
        "Allocation of the First identifier failed! %v", err_1)

    ident2, err_2 := arena_utils.push_struct(&arena , parser.Identifier{
        token = parser.alloc_token(&arena,tok.Token{
            literal = "anotherVar",
            type = .Identifier
        })
    })

    testing.expectf(t, err_2 == .None,
        "Allocation of the second identifier failed! %v", err_2)

    let_stmt, err := arena_utils.push_struct(&arena, parser.LetStatement{
        token = nil,
        ident = ident1,
        value = ident2, 
    })

    let_stmt.token = parser.alloc_token(
        &arena,
        tok.Token{literal = "let", type = .Let}
    )
    
    program.arena = arena
    program.statements = {let_stmt}

    defer parser.free_program(program)

    program_str := parser.get_program_string(program)
    defer delete(program_str)

    if program_str != "let myVar = anotherVar;" {
        log.errorf("get_program_string() is wrong, got %s", program_str)
    }

}

@(test)
test_identifier_expressions :: proc(t: ^testing.T) {

    input := "foobar;"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "Program has not enough statements. got=%d", len(program.statements))

    stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, stmt_ok,
        "Statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(program.statements[0]))

    ident, ident_ok := stmt.expr.(^parser.Identifier)

    testing.expectf(t, ident_ok,
        "Expression is not of type '^parser.Identifier'!, got %v",
        reflect.union_variant_typeid(stmt.expr))

    test_identifier(t, ident, "foobar")
}

@(test)
test_integer_literal_expression :: proc(t: ^testing.T) {

    input := "5;"

    lex := lexer.new_lexer(input)
    par := parser.new_parser(lex)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "Program has not enough statements. got=%d", len(program.statements))

    stmt, ok := program.statements[0].(^parser.ExpressionStatement)
    
    testing.expectf(t, ok, "Statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(program.statements[0]))

    test_integer_literal(t, stmt.expr, 5)
}

@(test)
test_parsing_prefix_expressions :: proc(t: ^testing.T) {
    LiteralUnion :: union { i64, bool }

    PrefixTests :: struct {
        input:          string,
        operator:       string,
        value:  LiteralUnion
    }

    prefix_tests := []PrefixTests{
        { input = "!5;", operator = "!", value = 5},
        { input = "-15;", operator = "-", value = 15},
    }

    for &test in prefix_tests {

        par := parser.new_parser(test.input)
        defer parser.destroy_parser(par)

        program := parser.parse_program(par)
        defer parser.free_program(program)

        check_parser_errors(t, par)

        testing.expectf(t, len(program.statements) == 1,
            "program.statements does not contain 1 statement, got %d",
            len(program.statements))

        stmt, ok := program.statements[0].(^parser.ExpressionStatement)

        testing.expectf(t, ok, "Statement is not an 'ExpressionStatement', got %v",
            reflect.union_variant_typeid(program.statements[0]))

        expr, prefix_ok := stmt.expr.(^parser.PrefixExpression)

        testing.expectf(t, prefix_ok, "variant is not 'PrefixExpression'!, got %v",
            reflect.union_variant_typeid(stmt.expr))

        testing.expectf(t, expr.operator == test.operator,
            "expr.operator is not '%s', got '%s'", test.operator, expr.operator)

        switch val in test.value {
            case i64: test_literal_expression_i64(t, expr.right, val)
            case bool: test_boolean_expression(t, expr.right, val)
        }
    }
}

test_integer_literal :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    value: i64
) -> bool {

    integer, ok := expr.(^parser.IntegerLiteral)

    testing.expectf(t, ok, "il not 'IntegerLiteral', got %v",
        reflect.union_variant_typeid(expr)) or_return

    testing.expectf(t, integer.value == value, "integer.value not %d, got %d",
        value, integer.value) or_return

    testing.expectf(t, integer.token.literal == fmt.tprintf("%d", value),
        "integer.token.literal is not equal to '%s', got '%s'", value,
        integer.token.literal) or_return

    return true
}

@(test)
test_parsing_infix_expressions :: proc(t: ^testing.T) {

    LiteralUnion :: union { i64, bool }

    InfixTests :: struct {
        input:      string,
        left_value: LiteralUnion,
        operator:   string,
        right_value: LiteralUnion,
    }

    infix_tests := [?]InfixTests{
        {"5 + 5;", 5, "+", 5},
        {"5 - 5;", 5, "-", 5},
        {"5 * 5;", 5, "*", 5},
        {"5 / 5;", 5, "/", 5},
        {"5 > 5;", 5, ">", 5},
        {"5 < 5;", 5, "<", 5},
        {"5 == 5;", 5, "==", 5},
        {"5 != 5;", 5, "!=", 5},
        {"true == true", true, "==", true},
        {"true != false", true, "!=", false},
        {"false == false", false, "==", false},
    }

    for &test in infix_tests {

        par := parser.new_parser(test.input)
        defer parser.destroy_parser(par)

        program := parser.parse_program(par)
        defer parser.free_program(program)

        check_parser_errors(t, par)

        testing.expectf(t, len(program.statements) == 1,
            "program.Statements does not contain 1 statements, got %d", 1,
            len(program.statements))

        stmt, ok := program.statements[0].(^parser.ExpressionStatement)

        testing.expectf(t, ok,
            "program.statements[0] is not an 'ExpressionStatement', got=%v",
            reflect.union_variant_typeid(program.statements[0]))

        expr, infix_ok := stmt.expr.(^parser.InfixExpression)

        testing.expectf(t, infix_ok,
            "stmt.expr is not an 'InfixExpression', got=%v",
            reflect.union_variant_typeid(program.statements[0]))

        switch val in test.left_value {
        case i64:
            if test_integer_literal(t, expr.left, val) do return
        case bool:
            if test_boolean_expression(t, expr.left, val) do return
        }

        testing.expectf(t, expr.operator == test.operator,
            "expr.operator is not '%s', got='%s'", test.operator,
            expr.operator)

        switch val in test.right_value {
        case i64:
            if test_integer_literal(t, expr.left, val) do return
        case bool:
            if test_boolean_expression(t, expr.left, val) do return
        }
    }
}

@(test)
test_operator_precedence_parsing :: proc(t: ^testing.T) {
    PrecedenceTests :: struct {
        input: string,
        expected: string,
    }

    precedence_tests := [?]PrecedenceTests{
        {
            "-a * b",
            "((-a) * b)",
        },
        {
            "!-a",
            "(!(-a))",
        },
        {
            "a + b + c",
            "((a + b) + c)",
        },
        {
            "a + b - c",
            "((a + b) - c)",
        },
        {
            "a * b * c",
            "((a * b) * c)",
        },
        {
            "a * b / c",
            "((a * b) / c)",
        },
        {
            "a + b / c",
            "(a + (b / c))",
        },
        {
            "a + b * c + d / e - f",
            "(((a + (b * c)) + (d / e)) - f)",
        },
        {
            "3 + 4; -5 * 5",
            "(3 + 4)((-5) * 5)",
        },
        {
            "5 > 4 == 3 < 4",
            "((5 > 4) == (3 < 4))",
        },
        {
            "5 < 4 != 3 > 4",
            "((5 < 4) != (3 > 4))",
        },
        {
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        {
            "true",
            "true"
        },
        {
            "false",
            "false"
        },
        {
            "3 > 5 == false",
            "((3 > 5) == false)"
        },
        {
            "3 < 5 == true",
            "((3 < 5) == true)"
        },
        {
            "1 + (2 + 3) + 4",
            "((1 + (2 + 3)) + 4)",
        },
        {
            "(5 + 5) * 2",
            "((5 + 5) * 2)",
        },
        {
            "2 / (5 + 5)",
            "(2 / (5 + 5))",
        },
        {
            "-(5 + 5)",
            "(-(5 + 5))",
        },
        {
            "!(true == true)",
            "(!(true == true))",
        },
        {
            "a + add(b * c) + d",
            "((a + add((b * c))) + d)",
        },
        {
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        {
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        },
    }

    for &test, i in precedence_tests {

        par := parser.new_parser(test.input)
        defer parser.destroy_parser(par)

        program := parser.parse_program(par)
        defer parser.free_program(program)

        check_parser_errors(t, par)

        actual_string := parser.get_program_string(program)
        defer delete(actual_string)

        testing.expectf(t, actual_string == test.expected,
            "Got wrong output!\nExpected: '%s'\n, got: '%s'",
            test.expected, actual_string)

    }

}

@(test)
test_if_expressions :: proc(t: ^testing.T) {
    input := "if (x < y) { x }"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "program.statements does not contain 1 statement, got %d",
        len(program.statements))

    stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, stmt_ok,
        "Statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(program.statements[0]))

    if_expr, if_ok := stmt.expr.(^parser.IfExpression)

    testing.expectf(t, if_ok,
        "Statement is not of type 'IfExpression'!, got %v",
        reflect.union_variant_typeid(stmt.expr))

    test_infix_expression(t, if_expr.condition, "x", "<", "y")

    testing.expectf(t, len(if_expr.consequence.statements) == 1,
        "Consequence is not 1 statement, got %d",
        len(if_expr.consequence.statements))

    consequence, cons_ok := if_expr.consequence.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, if_ok,
        "Consequence statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(if_expr.consequence.statements[0]))

    testing.expect(t, test_literal_expression(t, consequence.expr, "x"))

    testing.expectf(t, if_expr.alternative == nil,
        "Alternative was not nil!, got '%v'", if_expr.alternative)
}

@(test)
test_if_else_expressions :: proc(t: ^testing.T) {
    input := "if (x < y) { x } else { y }"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "program.statements does not contain 1 statement, got %d",
        len(program.statements))

    stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, stmt_ok,
        "Statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(program.statements[0]))

    if_expr, if_ok := stmt.expr.(^parser.IfExpression)

    testing.expectf(t, if_ok,
        "Statement is not of type 'IfExpression'!, got %v",
        reflect.union_variant_typeid(stmt.expr))

    test_infix_expression(t, if_expr.condition, "x", "<", "y")

    // Testing 'if'
    testing.expectf(t, len(if_expr.consequence.statements) == 1,
        "Consequence is not 1 statement, got %d",
        len(if_expr.consequence.statements))

    consequence, cons_ok := if_expr.consequence.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, if_ok,
        "Consequence statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(if_expr.consequence.statements[0]))

    testing.expect(t, test_literal_expression(t, consequence.expr, "x"))
    testing.expect(t, if_expr.alternative != nil, "Alternative was nil!")

    // Testing 'else'
    testing.expectf(t, len(if_expr.alternative.statements) == 1,
        "Alternative is not 1 statement, got %d",
        len(if_expr.alternative.statements))

    alt, alt_ok := if_expr.alternative.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, if_ok,
        "Alternative statement is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(if_expr.alternative.statements[0]))

    testing.expect(t, test_literal_expression(t, alt.expr, "y"))
}

@(test)
test_function_literal :: proc(t: ^testing.T) {
    input := "fn(x, y) { x + y; }"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "program.statements does not contain 1 statement, got %d",
        len(program.statements))

    
    expr_stmt, expr_stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, expr_stmt_ok,
        "program.statements[0] is not of type 'ExpressionStatement'!, got %v",
        reflect.union_variant_typeid(program.statements[0]))

    fn_expr, fn_ok := expr_stmt.expr.(^parser.FunctionLiteral)

    testing.expectf(t, fn_ok,
        "expression is not of type 'FunctionLiteral'!, got %v",
        reflect.union_variant_typeid(expr_stmt.expr))

    testing.expectf(t, len(fn_expr.params) == 2,
        "Expected 2 paramaters, got %d", len(fn_expr.params))

    test_identifier(t, &fn_expr.params[0], "x")
    test_identifier(t, &fn_expr.params[1], "y")

    testing.expectf(t, len(fn_expr.body.statements) == 1,
        "Expected 1 statement in the function body, got %d",
        len(fn_expr.body.statements))

    body_stmt, body_ok :=
        fn_expr.body.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, body_ok,
        "Body statement is not of type 'ExpressionStatement'! got %v",
        reflect.union_variant_typeid(fn_expr.body.statements[0]))

    test_infix_expression(t, body_stmt.expr, "x", "+", "y")
}

@(test)
test_function_parameter_parsing :: proc(t: ^testing.T) {
    ParamTest :: struct {
        input: string,
        expected_params: []string,
    }

    tests := [?]ParamTest{
        { "fn() {}", {} },
        { "fn(x) {}", {"x"} },
        { "fn(x, y, z) {}", {"x", "y", "z"} },
    }

    for &test in tests {
        par := parser.new_parser(test.input)
        defer parser.destroy_parser(par)

        program := parser.parse_program(par)
        defer parser.free_program(program)

        check_parser_errors(t, par)

        testing.expectf(t, len(program.statements) == 1,
            "program.statements does not contain 1 statement, got %d",
            len(program.statements))

        stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

        testing.expectf(t, stmt_ok,
            "Statement is not of type 'ExpressionStatement'! got %v",
            reflect.union_variant_typeid(program.statements[0]))

        fn_literal, fn_ok := stmt.expr.(^parser.FunctionLiteral)

        testing.expectf(t, fn_ok,
            "ExpressionStatement doesn't have an expression of type 'FunctionLiteral' got %v",
            reflect.union_variant_typeid(stmt.expr))

        for test_ident, i in test.expected_params {
            test_identifier(t, &fn_literal.params[i], test_ident)
        }

    }

}

@(test)
test_call_expression_parsing :: proc(t: ^testing.T) {
    input := "add(1, 2 * 3, 4 + 5)"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "program.statements does not contain 1 statement, got %d",
        len(program.statements))

    stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, stmt_ok,
        "Statement is not of type 'ExpressionStatement'! got %v",
        reflect.union_variant_typeid(program.statements[0]))

    call_expr, call_ok := stmt.expr.(^parser.CallExpression)

    testing.expectf(t, call_ok,
        "Expression statement is not of type 'CallExpression'! got %v",
        reflect.union_variant_typeid(stmt.expr))

    call_ident, call_ident_ok := call_expr.expr.(^parser.Identifier)

    testing.expectf(t, call_ident_ok,
        "CallExpression.expr is not of type 'Identifier'! got %v",
        reflect.union_variant_typeid(call_expr.expr))

    if !test_identifier(t, call_ident, "add") {
        return
    }

    testing.expectf(t, len(call_expr.arguments) == 3,
        "Expected number of arguments to be 3, got %d",
        len(call_expr.arguments))

    test_literal_expression(t, call_expr.arguments[0], i32(1))
    test_infix_expression(t, call_expr.arguments[1], i32(2), "*", i32(3))
    test_infix_expression(t, call_expr.arguments[2], i32(4), "+", i32(5))
}


