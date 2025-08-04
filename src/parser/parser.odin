package parser

import "../lexer"
import "../token"
import "core:fmt"
import "core:mem/virtual"
import "core:strconv"
import "../utils"
import "../arena_utils"

PrefixParseProc :: proc(parser: ^Parser) -> (Expression, bool)
InfixParseProc :: proc(parser: ^Parser, expr: Expression) -> (Expression, bool)

Parser :: struct {
	lex:        ^lexer.Lexer,
	curr_token: token.Token,
	peek_token: token.Token,
	errors:     [dynamic]string,
    arena:      virtual.Arena,
    identifiers: [dynamic]Identifier,

    prefix_parser_procs: map[token.TokenType]PrefixParseProc,
    infix_parser_procs: map[token.TokenType]InfixParseProc,
}

Precedence :: enum u8 {
    LOWEST      = 1,
    EQUALS      = 2,
    LESSGREATER = 3,
    SUM         = 4,
    PRODUCT     = 5,
    PREFIX      = 6,
    CALL        = 7,
}

alloc_token :: proc(arena: ^virtual.Arena, tok: token.Token) -> ^token.Token {
    result_tok := arena_utils.push_struct(arena, token.Token)
    result_str := arena_utils.push_string(arena, tok.literal)

    result_tok.literal = result_str
    result_tok.type = tok.type

    return result_tok
}

new_parser :: proc{
    new_parser_lexer,
    new_parser_input,
}

new_parser_input :: proc(input: string, clone_input: bool = true) -> ^Parser {
    lex := lexer.new_lexer(input, clone_input) 
	return new_parser_lexer(lex)
}

new_parser_lexer :: proc(lex: ^lexer.Lexer) -> ^Parser {

	p := new(Parser)

	p.lex = lex
	p.curr_token = lexer.next_token(lex)
	p.peek_token = lexer.next_token(lex)
	p.errors = make([dynamic]string, 0, 1)

    p.prefix_parser_procs = make(map[token.TokenType]PrefixParseProc)

    p.prefix_parser_procs[token.TokenType.Identifier] = parse_identifier
    p.prefix_parser_procs[token.TokenType.Integer] = parse_integer_literal

    p.infix_parser_procs = make(map[token.TokenType]InfixParseProc) 

    err := virtual.arena_init_growing(&p.arena)
    assert(err == virtual.Allocator_Error.None, "Failed to allocate an arena!")

	return p
}

// TODO: Identifiers could be part of a different container like map.
// After that, don't release it like explicitly or directly!
free_program :: proc(program: ^Program) {

    virtual.arena_destroy(&program.arena)

    for &ident in program.identifiers {
        delete(ident.token.literal)
    }

    delete(program.identifiers)
    delete(program.statements)
    free(program)
}

// Arena and identifiers are passed onto the Program.
// They are not deleted here!
destroy_parser :: proc(parser: ^Parser) {
    lexer.destroy_lexer(parser.lex)
    parser.lex = nil

    delete(parser.errors)
    parser.errors = nil

    delete(parser.prefix_parser_procs)
    delete(parser.infix_parser_procs)

    free(parser)
}

next_token :: proc(using parser: ^Parser) {
	curr_token = peek_token
	peek_token = lexer.next_token(parser.lex)
}

// Be aware that this procedure advances to the next token!
expect_peek_token :: proc(using parser: ^Parser, expect_type: token.TokenType) -> bool {

    if peek_token.type == expect_type {
        next_token(parser)
        return true
    }

    err := fmt.tprintf("Expected peek token: '%v', got '%s' instead", 
        token.token_strings[expect_type], parser.peek_token.literal)
    append(&parser.errors, err)


    return false
}

expect_token :: proc(using parser: ^Parser, expect_type: token.TokenType) -> bool {

    if curr_token.type == expect_type do return true

    err := fmt.tprintf("Expected token: '%v', got '%s' instead",
        token.token_strings[expect_type], parser.curr_token.literal)

    append(&parser.errors, err)

    return false
}


parse_program :: proc(using parser: ^Parser) -> ^Program {
	program := new(Program)
	program.statements = {}

	for curr_token.type != token.TokenType.EOF {
		stmt, ok := parse_statement(parser)

		if ok {
		    append(&program.statements, stmt)
		}

		next_token(parser)
	}
    
    program.identifiers = parser.identifiers
    program.arena = parser.arena

	return program
}

parse_statement :: proc(parser: ^Parser) -> (stmt: Statement, ok: bool) {
    #partial switch parser.curr_token.type {
	case .Let:
		return parse_let_statement(parser)
    case .Return:
        return parse_return_statement(parser)
	case:
        return parse_expression_statement(parser)
    }
}

parse_expression_statement :: proc(parser: ^Parser) ->
    (result: Statement, ok: bool) {

    result.stmt = arena_utils.push_struct(&parser.arena, ExpressionStatement)

    expr_stmt := result.stmt.(^ExpressionStatement)
    expr_stmt.token = alloc_token(&parser.arena, parser.curr_token)

    expr_ok: bool
    expr_stmt.expr, expr_ok = parse_expression(parser, .LOWEST)

    if !ok {
        return Statement{stmt = nil}, false
    }

    if expect_peek_token(parser, token.TokenType.Semicolon) {
        next_token(parser)
    }

    return Statement{stmt = expr_stmt}, true
}

parse_expression :: proc(parser: ^Parser, precedence: Precedence) -> (expr: Expression, ok: bool) {
    // TODO: Could it be converted to just a switch statement?
    prefix_fn, map_ok := parser.prefix_parser_procs[parser.curr_token.type]

    if !map_ok {
        err_msg := fmt.tprintf("Prefix function based on token '%v' was not found!",
            parser.curr_token.type)
        append(&parser.errors, err_msg)
        return nil, false
    }

    return prefix_fn(parser)
}


parse_return_statement :: proc(parser: ^Parser) -> (result: Statement, ok: bool) {

    result.stmt = arena_utils.push_struct(&parser.arena, ReturnStatement)

    ret_stmt := result.stmt.(^ReturnStatement)
    ret_stmt.token = alloc_token(&parser.arena, parser.curr_token)

    for parser.curr_token.type != .Semicolon {
        next_token(parser)
    }

    // TODO: Add Expression support
    return result, true
}

// TODO: Probably sooner or later has to be modified
new_identifier :: proc(token: token.Token) -> ^Identifier {
    ident := new(Identifier)
    ident.token = token

    return ident
}

parse_let_statement :: proc(parser: ^Parser) -> (result: Statement, ok: bool) {

    if !expect_peek_token(parser, .Identifier) {
        return result, false
    }

    append(&parser.identifiers, Identifier{token = parser.curr_token})

    let_stmt := arena_utils.push_struct(&parser.arena, LetStatement{
        ident = &parser.identifiers[len(parser.identifiers) - 1],
        token = alloc_token(&parser.arena, parser.curr_token)
    })

    if !expect_peek_token(parser, .Assign) {
        return result, false
    }

    for parser.curr_token.type != .Semicolon {
        next_token(parser)
    }

    result.stmt = let_stmt

    return result, true
}

parse_integer_literal :: proc(parser: ^Parser) -> (Expression, bool) {
    if !expect_token(parser, .Integer) {
        return nil, false
    }

    parsed_value, ok := strconv.parse_i64(parser.curr_token.literal)

    if !ok {
        append(&parser.errors, fmt.tprintf("Failed to parse an integer! got '%s'",
            parser.curr_token.literal))
        return nil, false
    }

    int_literal := arena_utils.push_struct(&parser.arena, IntegerLiteral{
        token = alloc_token(&parser.arena, parser.curr_token),
        value = parsed_value,
    })

    return int_literal, true
}

parse_identifier :: proc(parser: ^Parser) -> (Expression, bool) {
    append(&parser.identifiers, Identifier{token = parser.curr_token})
    ident := &parser.identifiers[len(parser.identifiers) - 1]

    return ident, true
}
