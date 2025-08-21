#+feature dynamic-literals
package main

import "core:testing"
import "core:fmt"
import "lexer"
import tok "token"
import "parser"
import "core:log"
import "core:strconv"
import "core:mem/virtual"
import "arena_utils"

// TODO: use reflect to write out the variant name of a union.
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

test_literal_expression_identifier :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    expected: string
) -> bool {
    return test_identifier(t, expr, expected)
}

test_identifier :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    value: string
) -> bool {

    ident, ok := expr.(^parser.Identifier)

    testing.expectf(t, ok,
        "Unexpected expression type! Expected parse.Identifier, got %v",
        typeid_of(type_of(expr))) or_return

    testing.expectf(t, ident.token.literal == value,
        "Unexpected identifier name! Expected %s, got %s", value,
        ident.token.literal) or_return

    return true
}



test_infix_expression :: proc(
    t: ^testing.T,
    expr: parser.Expression,
    left: $T,
    operator: string,
    right: $E,
) -> bool {

    infix_expr, ok := expr.(^parser.InfixExpression)

    testing.expectf(t, ok,
        "Unexpected Expression. Expected InfixExpression, got %v",
        typeid_of(type_of(infix_expr))) or_return

    test_literal_expression(t, infix_expr.left, left) or_return

    testing.expectf(t, infix_expr.operator == operator,
        "Unexpected Infix operator. Expected %s, got %s", operator,
        infix_expr.operator) or_return

    test_literal_expression(t, infix_expr.right, right) or_return

    return true
}

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
        typeid_of(type_of(expr))) or_return

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

    program.identifiers = make([dynamic]parser.Identifier, 2)
    program.identifiers[0] = parser.Identifier{
        token = tok.Token{
            literal = "myVar",
            type = .Identifier
        }
    }

    program.identifiers[1] = parser.Identifier{
        token = tok.Token{
            literal = "anotherVar",
            type = .Identifier
        },
    }

    let_stmt, err := arena_utils.push_struct(&arena, parser.LetStatement{
        token = nil,
        ident = &program.identifiers[0],
        value = &program.identifiers[1], 
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
        typeid_of(type_of(stmt)))

    ident, ident_ok := stmt.expr.(^parser.Identifier)

    testing.expectf(t, ident_ok,
        "Expression is not of type '^parser.Identifier'!, got %v",
        typeid_of(type_of(ident)))

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
        typeid_of(type_of(program.statements[0])))

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
            typeid_of(type_of(program.statements[0])))

        expr, prefix_ok := stmt.expr.(^parser.PrefixExpression)

        testing.expectf(t, prefix_ok, "variant is not 'PrefixExpression'!, got %v",
            typeid_of(type_of(stmt.expr)))

        testing.expectf(t, expr.operator == test.operator,
            "expr.operator is not '%s', got '%s'", test.operator, expr.operator)

        switch val in test.value {
            case i64: test_literal_expression_i64(t, expr.right, val)
            case bool: test_boolean_expression(t, expr.right, val)
        }
    }
}

test_integer_literal :: proc(t: ^testing.T, il: parser.Expression,
    value: i64) -> bool {

    integer, ok := il.(^parser.IntegerLiteral)

    test_ok := testing.expectf(t, ok, "il not 'IntegerLiteral', got %v",
        typeid_of(type_of(il)))

    test_ok &= testing.expectf(t, integer.value == value, "integer.value not %d, got %d",
        value, integer.value)

    test_ok &= testing.expectf(t, integer.token.literal == fmt.tprintf("%d", value),
        "integer.token.literal is not equal to '%s', got '%s'", value,
        integer.token.literal)

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
            typeid_of(type_of(program.statements[0])))

        expr, infix_ok := stmt.expr.(^parser.InfixExpression)

        testing.expectf(t, infix_ok,
            "stmt.expr is not an 'InfixExpression', got=%v",
            typeid_of(type_of(program.statements[0])))

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

    testing.expectf(t, len(program.statements) == 1,
        "program.statements does not contain 1 statement, got %d",
        len(program.statements))

    stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, stmt_ok,
        "Statement is not of type 'ExpressionStatement'!, got %v",
        typeid_of(type_of(program.statements[0])))

    if_expr, if_ok := stmt.expr.(^parser.IfExpression)

    testing.expectf(t, if_ok,
        "Statement is not of type 'IfExpression'!, got %v",
        typeid_of(type_of(stmt.expr)))

    test_infix_expression(t, if_expr.condition, "x", "<", "y")

    testing.expectf(t, len(if_expr.consequence.statements) == 1,
        "Consequence is not 1 statement, got %d",
        len(if_expr.consequence.statements))

    consequence, cons_ok := if_expr.consequence.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, if_ok,
        "Consequence statement is not of type 'ExpressionStatement'!, got %v",
        typeid_of(type_of(if_expr.consequence.statements[0])))

    testing.expect(t, test_identifier(t, consequence.expr, "x"))

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

    testing.expectf(t, len(program.statements) == 1,
        "program.statements does not contain 1 statement, got %d",
        len(program.statements))

    stmt, stmt_ok := program.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, stmt_ok,
        "Statement is not of type 'ExpressionStatement'!, got %v",
        typeid_of(type_of(program.statements[0])))

    if_expr, if_ok := stmt.expr.(^parser.IfExpression)

    testing.expectf(t, if_ok,
        "Statement is not of type 'IfExpression'!, got %v",
        typeid_of(type_of(stmt.expr)))

    test_infix_expression(t, if_expr.condition, "x", "<", "y")

    // Testing 'if'
    testing.expectf(t, len(if_expr.consequence.statements) == 1,
        "Consequence is not 1 statement, got %d",
        len(if_expr.consequence.statements))

    consequence, cons_ok := if_expr.consequence.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, if_ok,
        "Consequence statement is not of type 'ExpressionStatement'!, got %v",
        typeid_of(type_of(if_expr.consequence.statements[0])))

    testing.expect(t, test_identifier(t, consequence.expr, "x"))
    testing.expect(t, if_expr.alternative != nil, "Alternative was nil!")

    // Testing 'else'
    testing.expectf(t, len(if_expr.alternative.statements) == 1,
        "Alternative is not 1 statement, got %d",
        len(if_expr.alternative.statements))

    alt, alt_ok := if_expr.alternative.statements[0].(^parser.ExpressionStatement)

    testing.expectf(t, if_ok,
        "Alternative statement is not of type 'ExpressionStatement'!, got %v",
        typeid_of(type_of(if_expr.alternative.statements[0])))

    testing.expect(t, test_identifier(t, alt.expr, "y"))
}
