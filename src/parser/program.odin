package parser

import "core:strings"
import "core:mem/virtual"
import "core:fmt"


Program :: struct {
    arena: virtual.Arena,
    identifiers: [dynamic]Identifier,
	statements: [dynamic]Statement,
}

get_program_string :: proc(program: ^Program) -> string {
    assert(program != nil)
    str_builder := strings.builder_make()

    // This can be optimized!
    for &statement in program.statements {
        out_stmt_str := get_statement_string(&statement) 
        strings.write_string(&str_builder, out_stmt_str)
        delete(out_stmt_str)
    }

    return strings.to_string(str_builder)
}

write_expr_statement_string :: proc(statement: ^ExpressionStatement,
    str_builder: ^strings.Builder) {

    assert(str_builder != nil)

    strings.write_string(str_builder, statement.token.literal)

    write_expression_string(statement.expr, str_builder)
    strings.write_byte(str_builder, ';')
}

write_expression_string :: proc(expr: ^Expression,
    str_builder: ^strings.Builder) {
    
    switch variant in expr {
    case ^Identifier:
        strings.write_string(str_builder, variant.token.literal)
    case ^IntegerLiteral:
        strings.write_string(str_builder, variant.token.literal)
    case ^PrefixExpression:
        strings.write_string(str_builder, variant.token.literal)
        write_expression_string(variant.right, str_builder)
    }
}


// TODO: Make sure the strings are being properly outputed.
get_statement_string :: proc(statement: ^Statement) -> string {

    if statement == nil {
        fmt.eprintfln(`Get statement couldn't output anything!
            Statement is nil!`)
        return ""
    }

    str_builder := strings.builder_make()

    switch &obj in statement.stmt {
    case ^ReturnStatement:
        strings.write_string(&str_builder, obj.token.literal)
        strings.write_byte(&str_builder, ' ')
        write_expression_string(obj.value, &str_builder)
    case ^LetStatement:
        strings.write_string(&str_builder, obj.token.literal)
        strings.write_byte(&str_builder, ' ')
        strings.write_string(&str_builder, obj.ident.token.literal)
        strings.write_string(&str_builder, " = ")
        write_expression_string(obj.value, &str_builder)
    case ^ExpressionStatement:
        write_expr_statement_string(obj, &str_builder)
    case ^PrefixExpression:
        strings.write_string(&str_builder, obj.operator^)
        write_expression_string(obj.right, &str_builder)
    case:
        fmt.eprintln("Failed to get statement string. Unhandled Statement type!")
    }

    strings.write_byte(&str_builder, ';')

    return strings.to_string(str_builder) // this only gives out the slice!
}
