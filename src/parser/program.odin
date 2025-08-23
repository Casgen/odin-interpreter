package parser

import "core:strings"
import "core:mem/virtual"
import "core:fmt"


Program :: struct {
    arena: virtual.Arena,
	statements: [dynamic]Statement,
}

get_program_string :: proc(program: ^Program) -> string {
    assert(program != nil)

    str_builder := strings.builder_make()

    for &statement in program.statements {
        write_statement_string(statement, &str_builder) 
    }

    return strings.to_string(str_builder)
}

write_block_statement :: proc(
    block_stmt: ^BlockStatement,
    str_builder: ^strings.Builder,
) {
    strings.write_string(str_builder, "{ ")

    for &stmt in block_stmt.statements {
        write_statement_string(stmt, str_builder)
    }

    strings.write_string(str_builder, "}")
}

write_expression_string :: proc(expr: Expression,
    str_builder: ^strings.Builder) {
    
    switch variant in expr {
    case ^Identifier:
        strings.write_string(str_builder, variant.token.literal)
    case ^IntegerLiteral:
        strings.write_string(str_builder, variant.token.literal)
    case ^PrefixExpression:
        strings.write_byte(str_builder, '(')
        strings.write_string(str_builder, variant.operator)
        write_expression_string(variant.right, str_builder)
        strings.write_byte(str_builder, ')')
    case ^InfixExpression:
        strings.write_byte(str_builder, '(')
        write_expression_string(variant.left, str_builder)
        strings.write_byte(str_builder, ' ')
        strings.write_string(str_builder, variant.operator)
        strings.write_byte(str_builder, ' ')
        write_expression_string(variant.right, str_builder)
        strings.write_byte(str_builder, ')')
    case ^Boolean:
        strings.write_string(str_builder, variant.value ? "true" : "false")
    case ^IfExpression:
        strings.write_string(str_builder, "if")
        write_expression_string(variant.condition, str_builder)
        strings.write_byte(str_builder, ' ')
        write_block_statement(variant.consequence, str_builder)

        if (variant.alternative != nil) {
            strings.write_string(str_builder, "else ")
            write_block_statement(variant.consequence, str_builder)
        }
    case ^FunctionLiteral:
        strings.write_string(str_builder, "fn(")
    
        param_count := len(variant.params)

        if param_count > 0 {
            for i in 0..<(len(variant.params)-1) {
                strings.write_string(
                    str_builder, 
                    variant.params[i].token.literal
                )
                strings.write_string(str_builder, ", ")
            }

            strings.write_string(
                str_builder,
                variant.params[param_count - 1].token.literal,
            )
        }

        strings.write_string(str_builder,") ")
        write_block_statement(variant.body, str_builder)
    }
}


// TODO: Make sure the strings are being properly outputed.
write_statement_string :: proc(
    statement: Statement,
    str_builder: ^strings.Builder
) {

    if statement == nil {
        fmt.eprintfln(`Get statement couldn't output anything!
            Statement is nil!`)
        return
    }

    switch obj in statement {
    case ^ReturnStatement:
        strings.write_string(str_builder, obj.token.literal)
        strings.write_byte(str_builder, ' ')
        write_expression_string(obj.value, str_builder)
    case ^LetStatement:
        strings.write_string(str_builder, obj.token.literal)
        strings.write_byte(str_builder, ' ')
        strings.write_string(str_builder, obj.ident.token.literal)
        strings.write_string(str_builder, " = ")
        write_expression_string(obj.value, str_builder)
        strings.write_byte(str_builder, ';')
    case ^ExpressionStatement:
        write_expression_string(obj.expr, str_builder)
    case:
        fmt.eprintln("Failed to get statement string. Unhandled Statement type!")
    }
}
