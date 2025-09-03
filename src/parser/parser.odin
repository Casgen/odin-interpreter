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
import "core:slice"
import "core:mem"
import "base:runtime"

PrefixParseProc :: proc(parser: ^Parser) -> (Expression, bool)
InfixParseProc :: proc(parser: ^Parser, expr: Expression) -> (Expression, bool)

Parser :: struct {
	lex:        ^lexer.Lexer,
	curr_token: token.Token,
	peek_token: token.Token,
	errors:     [dynamic]string,
    arena:      virtual.Arena,

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
    .Left_Paren = .Call,
}

alloc_token :: proc(arena: ^virtual.Arena, tok: token.Token) -> ^token.Token {
    result_tok, struct_err := arena_utils.push_struct(arena, token.Token)
    if struct_err != .None do return nil

    result_str, str_err := arena_utils.push_string(arena, tok.literal)
    if str_err != .None do return nil

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

    err := virtual.arena_init_growing(&p.arena)
    assert(err == virtual.Allocator_Error.None, "Failed to allocate an arena!")

	return p
}

free_program :: proc(program: ^Program) {
    virtual.arena_destroy(&program.arena)

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

    token_literal: string
    if parser.peek_token.type != .EOF {
        token_literal = token.token_strings[parser.peek_token.type]
    } else {
        token_literal = "EOF"
    }

    err := fmt.tprintf("Expected peek token: '%v', got '%s' instead", 
        token.token_strings[expect_type], token_literal)
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
    if err != .None do return nil, false

    expr_stmt.token = alloc_token(&parser.arena, parser.curr_token)

    expr_ok: bool
    expr_stmt.expr, expr_ok = parse_expression(parser, .Lowest)

    if !expr_ok do return nil, false

    if parser.peek_token.type == token.TokenType.Semicolon {
        next_token(parser)
    }

    return expr_stmt, true
}

get_prefix_proc_by_token_type :: proc(tok_type: token.TokenType) ->
    PrefixParseProc {
    
    #partial switch tok_type {
    case .Identifier: return parse_identifier
    case .Integer: return parse_integer_literal
    case .Bang: fallthrough
    case .Minus: return parse_prefix_expression
    case .True: fallthrough
    case .False: return parse_boolean
    case .Left_Paren: return parse_grouped_expression
    case .If: return parse_if_expression
    case .Function: return parse_function_literal
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
    case token.TokenType.Left_Paren: return parse_call_expression
    }

    return nil
}

parse_expression :: proc(parser: ^Parser, precedence: Precedence) -> (Expression, bool) {
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
    if err != .None do return nil, false

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
    if err != .None do return nil, false

    expr.token = alloc_token(&parser.arena, parser.curr_token)
    expr.operator = expr.token.literal
    expr.left = left_expr

    precedence := curr_precedence(parser)
    next_token(parser)

    ok: bool
    expr.right, ok = parse_expression(parser, precedence)

    return expr, ok
}

parse_call_expression :: proc(parser: ^Parser, left_expr: Expression) ->
    (Expression, bool) {

    call_expr, err := arena_utils.push_struct_type(
        &parser.arena,
        CallExpression
    )

    if err != .None do return nil, false

   call_expr.expr = left_expr
    ok: bool
    call_expr.arguments, ok = parse_call_arguments(parser)
    
    return call_expr, ok
}

// TODO: The underlying allocation of identifiers is kinda sub-optimal
// and should be handled differently.
parse_call_arguments :: proc(parser: ^Parser) -> ([]Expression, bool) {
    if parser.peek_token.type == .Right_Paren {
        next_token(parser)
        return nil, false
    }

    next_token(parser)

    arguments: [dynamic]Expression
    defer delete(arguments)


    expr, expr_ok := parse_expression(parser, .Lowest)
    if !expr_ok do return nil, false

    append(&arguments, expr)

    for parser.peek_token.type == .Comma {
        next_token(parser)
        next_token(parser)

        expr, expr_ok = parse_expression(parser, .Lowest)
        if !expr_ok do return nil, false

        append(&arguments, expr)
    }

    if !expect_peek_token(parser, .Right_Paren) {
        return nil, false
    }

    /*
    WARN: The push_dynamic_array here makes a shallow copy. For now
    The token pointer is valid, because it is pre-allocated in the arena
    Watch out When modifying this to ensure that the identifiers are
    separate from the AST to ensure different lifetimes throughout the
    program
    */
    args_slice, err := arena_utils.push_dynamic_array(
        &parser.arena,
        arguments,
    )

    return args_slice, err == .None
}

parse_if_expression :: proc(parser: ^Parser) -> (Expression, bool) {

    if_expr, err := arena_utils.push_struct_type(&parser.arena, IfExpression)
    if err != .None do return nil, false

    if_expr.token = alloc_token(&parser.arena, parser.curr_token)

    // Parse if
    if !expect_peek_token(parser, .Left_Paren) do return nil, false

    next_token(parser)

    expr_ok: bool
    if_expr.condition, expr_ok = parse_expression(parser, .Lowest)

    if !expr_ok || !expect_peek_token(parser, .Right_Paren) {
        return nil, false
    }

    if !expect_peek_token(parser, .Left_Brace) do return nil, false
    
    cons_ok: bool
    if_expr.consequence, cons_ok = parse_block_statement(parser)

    if !cons_ok do return nil, false

    // if 'else' block is present, parse it.
    if parser.peek_token.type != .Else {
        return if_expr, true
    }

    next_token(parser)

    if !expect_peek_token(parser, .Left_Brace) {
        return if_expr, false
    }

    alt_ok: bool
    if_expr.alternative, alt_ok = parse_block_statement(parser)

    return if_expr, alt_ok
}

parse_function_literal :: proc(parser: ^Parser) -> (Expression, bool) {

    fn_literal, err := arena_utils.push_struct_type(
        &parser.arena,
        FunctionLiteral
    )
    if err != .None do return nil, false

    fn_literal.token = alloc_token(&parser.arena, parser.curr_token)

    if !expect_peek_token(parser, .Left_Paren) {
        return nil, false
    }

    params_ok: bool
    fn_literal.params, params_ok = parse_function_parameters(parser)

    if params_ok && !expect_peek_token(parser, .Left_Brace) {
        return fn_literal, false
    }

    body_ok: bool
    fn_literal.body, body_ok = parse_block_statement(parser)


    return fn_literal, body_ok
}
// TODO: The underlying allocation of identifiers is kinda sub-optimal
// and should be handled differently.
parse_function_parameters :: proc(parser: ^Parser) -> ([]Identifier, bool) {

    if parser.peek_token.type == .Right_Paren {
        next_token(parser)
        return nil, true
    }

    next_token(parser)

    // Relying on the fact that the token literal is valid, which should
    // probably be since parser owns the lexer and lexer owns the input.
    identifiers: [dynamic]Identifier = {
        Identifier{ token = alloc_token(&parser.arena, parser.curr_token) }
    }
    defer delete(identifiers)

    for parser.peek_token.type == .Comma {
        next_token(parser)
        next_token(parser)
        append(&identifiers, Identifier{
            token = alloc_token(&parser.arena, parser.curr_token)
        })
    }


    if !expect_peek_token(parser, .Right_Paren) {
        return nil, false
    }

    /*
    WARN: The push_dynamic_array here makes a shallow copy. For now
    The token pointer is valid, because it is pre-allocated in the arena
    Watch out When modifying this to ensure that the identifiers are
    separate from the AST to ensure different lifetimes throughout the
    program
    */
    ident_slice, err := arena_utils.push_dynamic_array(
        &parser.arena,
        identifiers,
    )

    return ident_slice, err == .None
}

parse_block_statement :: proc(parser: ^Parser) -> (^BlockStatement, bool) {

    block_stmt, err := arena_utils.push_struct(&parser.arena, BlockStatement)
    if err != .None do return nil, false

    block_stmt.token = alloc_token(&parser.arena, parser.curr_token)

    next_token(parser)

    // I don't know how many statements are going to be there to allocate
    // it in the arena. Have to parse them first and then allocate them
    // in the arena.
    statements: [dynamic]Statement = {}
    defer delete(statements)

    for parser.curr_token.type != .Right_Brace && parser.curr_token.type != .EOF {
        stmt, ok := parse_statement(parser)

        if ok {
            append(&statements, stmt)
        }

        next_token(parser)
    }

    slice_stmts, stmts_err := arena_utils.push_dynamic_array(
        &parser.arena,
        statements
    )

    if stmts_err != .None do return block_stmt, false

    block_stmt.statements = slice_stmts
    return block_stmt, true
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

    return_expr, err := arena_utils.push_struct(&parser.arena, ReturnStatement)

    if err != .None do return nil, false

    return_expr.token = alloc_token(&parser.arena, parser.curr_token)

    next_token(parser)

    expr_ok: bool
    return_expr.value, expr_ok = parse_expression(parser, .Lowest)

    if !expr_ok do return return_expr, false

    for parser.curr_token.type != .Semicolon {
        next_token(parser)
    }

    return return_expr, true
}


parse_let_statement :: proc(parser: ^Parser) -> (result: Statement, ok: bool) {

    let_stmt, err := arena_utils.push_struct_type(&parser.arena, LetStatement)
    if err != .None do return let_stmt, false

    let_stmt.token = alloc_token(&parser.arena, parser.curr_token)

    if !expect_peek_token(parser, .Identifier) {
        return result, false
    }

    ident, ident_ok := parse_identifier(parser)

    if !ident_ok {
        return nil, false
    }

    let_stmt.ident = ident.(^Identifier)

    if !expect_peek_token(parser, .Assign) {
        return result, false
    }

    next_token(parser)

    value_ok: bool
    let_stmt.value, value_ok = parse_expression(parser, .Lowest)

    if !value_ok do return nil, false

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

    if err != .None do return nil, false

    return int_literal, true
}

is_boolean_literal :: proc(token_type: token.TokenType) -> bool {
    return token_type == .True || token_type == .False
}

parse_boolean :: proc(parser: ^Parser) -> (Expression, bool) {
    
    if !is_boolean_literal(parser.curr_token.type) {
        err_msg := fmt.tprintf(
            "Expected token 'True' or 'False', got '%s' instead",
            parser.curr_token.type
        )

        append(&parser.errors, err_msg)
        return nil, false
    }
    
    bool_expr, err := arena_utils.push_struct(&parser.arena, Boolean)

    if err != .None do return nil, false
    
    bool_expr.token = alloc_token(&parser.arena, parser.curr_token)
    bool_expr.value = parser.curr_token.type == .True ? true : false

    return bool_expr, true
}

parse_grouped_expression :: proc(parser: ^Parser) -> (Expression, bool) {

    next_token(parser)

    expr, ok := parse_expression(parser, .Lowest)

    if ok && expect_peek_token(parser, .Right_Paren) {
        return expr, true
    }

    return nil, false
}

parse_identifier :: proc(parser: ^Parser) -> (Expression, bool) {
    ident, err := arena_utils.push_struct(&parser.arena, Identifier)

    if err != .None do return nil, false
    ident.token = alloc_token(&parser.arena, parser.curr_token)

    return ident, true
}
