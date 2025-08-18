#+feature dynamic-literals
package parser

import "../lexer"
import "../token"
import "core:fmt"
import "core:mem/virtual"
import "core:strconv"
import "../utils"
import "../arena_utils"
import "core:strings"

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
    Lowest      = 1,
    Equals      = 2,
    Less_Greater = 3,
    Sum         = 4,
    Product     = 5,
    Prefix      = 6,
    Call        = 7,
}

TokenToPrecedence := map[token.TokenType]Precedence{
    .Equal      = .Equals,
    .Not_Equal  = .Equals,
    .Gt         = .Less_Greater,
    .Lt         = .Less_Greater,
    .Plus       = .Sum,
    .Minus      = .Sum,
    .Slash      = .Product,
    .Asterisk   = .Product,
}

alloc_token :: proc(arena: ^virtual.Arena, tok: token.Token) -> ^token.Token {
    result_tok, struct_err := arena_utils.push_struct(arena, token.Token)

    if struct_err != .None {
        return nil
    }

    result_str, str_err := arena_utils.push_string(arena, tok.literal)

    if str_err != .None {
        return nil
    }

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
    p.prefix_parser_procs[token.TokenType.Bang] = parse_prefix_expression
    p.prefix_parser_procs[token.TokenType.Minus] = parse_prefix_expression
    p.prefix_parser_procs[token.TokenType.True] = parse_boolean
    p.prefix_parser_procs[token.TokenType.False] = parse_boolean

    p.infix_parser_procs = make(map[token.TokenType]InfixParseProc) 

    p.infix_parser_procs[token.TokenType.Plus] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Minus] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Slash] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Asterisk] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Equal] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Not_Equal] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Lt] = parse_infix_expression
    p.infix_parser_procs[token.TokenType.Gt] = parse_infix_expression

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

add_error :: proc(parser: ^Parser, expected: any, actual: any) {

    err := fmt.tprintf("Expected token: '%v', got '%s' instead",
        expected, actual)

    append(&parser.errors, err)
}

expect_token :: proc(parser: ^Parser, expect_type: token.TokenType) -> bool {

    if parser.curr_token.type == expect_type do return true

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
    (Statement, bool) {

    expr_stmt, err := arena_utils.push_struct(&parser.arena, ExpressionStatement)

    if err != .None {
        return nil, false
    }

    expr_stmt.token = alloc_token(&parser.arena, parser.curr_token)

    expr_ok: bool
    expr_stmt.expr, expr_ok = parse_expression(parser, .Lowest)

    if !expr_ok {
        return nil, false
    }

    if parser.peek_token.type == token.TokenType.Semicolon {
        next_token(parser)
    }

    return expr_stmt, true
}

get_prefix_proc_by_token_type :: proc( tok_type: token.TokenType) ->
    PrefixParseProc {
    
    #partial switch tok_type {
    case .Identifier: return parse_identifier
    case .Integer: return parse_integer_literal
    case .Bang: fallthrough
    case .Minus: return parse_prefix_expression
    case .True: fallthrough
    case .False:
        return parse_boolean
    }

    return nil
}

get_infix_proc_by_token_type :: proc(tok_type: token.TokenType) -> 
    InfixParseProc {

    #partial switch tok_type {
    case token.TokenType.Plus: fallthrough
    case token.TokenType.Minus: fallthrough
    case token.TokenType.Slash: fallthrough
    case token.TokenType.Asterisk: fallthrough
    case token.TokenType.Equal: fallthrough
    case token.TokenType.Not_Equal: fallthrough
    case token.TokenType.Lt: fallthrough
    case token.TokenType.Gt: return parse_infix_expression
    }

    return nil
}

parse_expression :: proc(parser: ^Parser, precedence: Precedence) -> (Expression, bool) {
    // TODO: Could it be converted to just a switch statement?
    prefix_fn := get_prefix_proc_by_token_type(parser.curr_token.type)

    if prefix_fn == nil {
        err_msg := fmt.tprintf("Prefix function based on token '%v' was not found!",
            parser.curr_token.type)
        append(&parser.errors, err_msg)
        return nil, false
    }

    left_expr, prefix_ok := prefix_fn(parser)

    if !prefix_ok {
        return nil, false
    }


    if parser.peek_token.type == .Semicolon || precedence >= peek_precedence(parser) {
        return left_expr, true
    }

    ok: bool = false
    for parser.peek_token.type != .Semicolon && precedence < peek_precedence(parser) {
        infix_fn := get_infix_proc_by_token_type(parser.peek_token.type)

        if infix_fn == nil {
            return left_expr, true
        }

        next_token(parser)

        left_expr, ok = infix_fn(parser, left_expr)
    }

    return left_expr, ok
}

parse_prefix_expression :: proc(parser: ^Parser) -> (Expression, bool) {
    
    result, err := arena_utils.push_struct(&parser.arena, PrefixExpression)

    if err != .None {
        return nil, false
    }

    result.token = alloc_token(&parser.arena, parser.curr_token)
    result.operator = result.token.literal

    next_token(parser)

    ok: bool
    result.right, ok = parse_expression(parser, .Prefix)

    return result, true
}

parse_infix_expression :: proc(parser: ^Parser, left_expr: Expression) ->
    (Expression, bool) {
    
    expr, err := arena_utils.push_struct(&parser.arena, InfixExpression)

    if err != .None {
        return nil, false
    }

    expr.token = alloc_token(&parser.arena, parser.curr_token)
    expr.operator = expr.token.literal
    expr.left = left_expr

    precedence := curr_precedence(parser)
    next_token(parser)

    ok: bool
    expr.right, ok = parse_expression(parser, precedence)

    return expr, ok
}

peek_precedence :: proc(parser: ^Parser) -> Precedence {
    if prec, ok := TokenToPrecedence[parser.peek_token.type]; ok {
        return prec
    }

    return .Lowest
}

curr_precedence :: proc(parser: ^Parser) -> Precedence {
    if prec, ok := TokenToPrecedence[parser.curr_token.type]; ok {
        return prec
    }

    return .Lowest
}


parse_return_statement :: proc(parser: ^Parser) -> (Statement, bool) {

    result, err := arena_utils.push_struct(&parser.arena, ReturnStatement)

    if err != .None {
        return nil, false
    }

    result.token = alloc_token(&parser.arena, parser.curr_token)

    for parser.curr_token.type != .Semicolon {
        next_token(parser)
    }

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

    let_stmt, err := arena_utils.push_struct(&parser.arena, LetStatement{
        ident = &parser.identifiers[len(parser.identifiers) - 1],
        token = alloc_token(&parser.arena, parser.curr_token)
    })

    if err != .None {
        return nil, false
    }

    if !expect_peek_token(parser, .Assign) {
        return result, false
    }

    for parser.curr_token.type != .Semicolon {
        next_token(parser)
    }

    result = let_stmt

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

    int_literal, err := arena_utils.push_struct(&parser.arena, IntegerLiteral{
        token = alloc_token(&parser.arena, parser.curr_token),
        value = parsed_value,
    })

    if err != .None {
        return nil, false
    }

    return int_literal, true
}

is_boolean_type :: proc(token_type: token.TokenType) -> bool {
    return token_type == .True || token_type == .False
}

parse_boolean :: proc(parser: ^Parser) -> (Expression, bool) {
    
    if !is_boolean_type(parser.curr_token.type) {
        err_msg := fmt.tprintf(
            "Expected token 'True' or 'False', got '%s' instead",
            parser.curr_token.type
        )

        append(&parser.errors, err_msg)
        return nil, false
    }
    
    bool_expr, err := arena_utils.push_struct(&parser.arena, Boolean)

    if err != .None {
        return nil, false
    }
    
    bool_expr.token = alloc_token(&parser.arena, parser.curr_token)
    bool_expr.value = parser.curr_token.type == .True ? true : false

    return bool_expr, true
}

parse_identifier :: proc(parser: ^Parser) -> (Expression, bool) {
    tok := token.Token{
        type = parser.curr_token.type,
        literal = strings.clone(parser.curr_token.literal)
    }
    append(&parser.identifiers, Identifier{token = tok})
    ident := &parser.identifiers[len(parser.identifiers) - 1]

    return ident, true
}
