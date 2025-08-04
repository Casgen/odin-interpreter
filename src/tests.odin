#+feature dynamic-literals
package main

import "core:testing"
import "core:fmt"
import "lexer"
import "token"
import "parser"
import "core:log"
import "core:strconv"
import "core:mem/virtual"

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
	tests := [?]token.Token {
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "five"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "ten"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "add"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Function, "fn"},
		{token.TokenType.Left_Paren, "("},
		{token.TokenType.Identifier, "x"},
		{token.TokenType.Comma, ","},
		{token.TokenType.Identifier, "y"},
		{token.TokenType.Right_Paren, ")"},
		{token.TokenType.Left_Brace, "{"},
		{token.TokenType.Identifier, "x"},
		{token.TokenType.Plus, "+"},
		{token.TokenType.Identifier, "y"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Right_Brace, "}"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "result"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Identifier, "add"},
		{token.TokenType.Left_Paren, "("},
		{token.TokenType.Identifier, "five"},
		{token.TokenType.Comma, ","},
		{token.TokenType.Identifier, "ten"},
		{token.TokenType.Right_Paren, ")"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Bang, "!"},
		{token.TokenType.Minus, "-"},
		{token.TokenType.Slash, "/"},
		{token.TokenType.Asterisk, "*"},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Lt, "<"},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Gt, ">"},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.If, "if"},
		{token.TokenType.Left_Paren, "("},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Lt, "<"},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Right_Paren, ")"},
		{token.TokenType.Left_Brace, "{"},
		{token.TokenType.Return, "return"},
		{token.TokenType.True, "true"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Right_Brace, "}"},
		{token.TokenType.Else, "else"},
		{token.TokenType.Left_Brace, "{"},
		{token.TokenType.Return, "return"},
		{token.TokenType.False, "false"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Right_Brace, "}"},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Equal, "=="},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Not_Equal, "!="},
		{token.TokenType.Integer, "9"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.EOF, ""},
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
    defer { parser.destroy_parser(par); par = nil }

    program := parser.parse_program(par)
    defer { parser.free_program(program); program = nil }
    
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
    
    program.statements = {
        parser.Statement{
            stmt = parser.LetStatement{
                token = {literal = "let", type = .Let},
                ident = parser.create_identifier(token.Token{
                    literal = "myVar",
                    type = .Identifier
                }),
                value = parser.create_identifier(
                        token.Token{
                            literal = "anotherVar",
                            type = .Identifier
                })
            }
        }
    }

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

    lex := lexer.new_lexer(input)
    par := parser.new_parser(lex)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    check_parser_errors(t, par)

    testing.expectf(t, len(program.statements) == 1,
        "Program has not enough statements. got=%d", len(program.statements))

    stmt: parser.ExpressionStatement

    #partial switch s in program.statements[0].stmt {
    case parser.ExpressionStatement: stmt = s
    case:
        log.errorf("Statement is not of type 'ExpressionStatement'!, got %v",
            typeid_of(type_of(s)))
        testing.fail(t)
    }

    ident: ^parser.Identifier

    #partial switch expr in stmt.expr {
    case ^parser.Identifier: ident = expr
    case:
        log.errorf("Expression is not of type '^parser.Identifier'!, got %v",
            typeid_of(type_of(expr)))
        testing.fail(t)
    }

    testing.expectf(t, ident.token.literal == "foobar",
        `ident.token.literal not '%s', got '%s'`, "foobar", ident.token.literal)
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

    stmt, ok := program.statements[0].stmt.(parser.ExpressionStatement)
    
    testing.expectf(t, ok, "Statement is not of type 'ExpressionStatement'!, got %v",
        typeid_of(type_of(program.statements[0].stmt)))

    ident, ident_ok := stmt.expr.(parser.IntegerLiteral)

    testing.expectf(t, ident_ok, "Expression is not of type 'IntegerLiteral'!, got %v",
        typeid_of(type_of(stmt.expr)))

    testing.expectf(t, ident.token.literal == "5",
        `ident.token.literal not '5', got '%s'`, ident.token.literal)

    testing.expectf(t, ident.token.type == .Integer,
        "ident.token.type not .Integer, got %v", ident.token.literal)

    testing.expectf(t, ident.value == 5,
        `ident.value is not equal to 5, got=%d`, ident.value)
}

@(test)
test_parsing_prefix_expressions :: proc(t: ^testing.T) {
    PrefixTests :: struct {
        input:          string,
        operator:       string,
        integer_value:  i64
    }

    prefix_tests := []PrefixTests{
        { input = "!5;", operator = "!", integer_value = 5},
        { input = "-15;", operator = "-", integer_value = 15},
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

        stmt, ok := program.statements[0].stmt.(parser.ExpressionStatement)

        testing.expectf(t, ok, "Statement is not an 'ExpressionStatement', got %v",
            typeid_of(type_of(program.statements[0].stmt)))

        expr, prefix_ok := stmt.expr.(parser.PrefixExpression)

        testing.expectf(t, prefix_ok, "variant is not 'PrefixExpression'!, got %v",
            typeid_of(type_of(stmt.expr)))

        testing.expectf(t, expr.operator == test.operator,
            "expr.operator is not '%s', got '%s'", test.operator, expr.operator)

        test_integer_literal(t, expr.right, test.integer_value)
    }
}

test_integer_literal :: proc(t: ^testing.T, il: ^parser.Expression,
    value: i64) -> bool {

    integer, ok := il.(parser.IntegerLiteral)

    testing.expectf(t, ok, "il not 'IntegerLiteral', got %v")

    testing.expectf(t, integer.value == value, "integer.value not %d, got %d",
        value, integer.value)

    testing.expectf(t, integer.token.literal == fmt.tprintf("%d", value),
        "integer.token.literal is not equal to '%s', got '%s'", value,
        integer.token.literal)
}
