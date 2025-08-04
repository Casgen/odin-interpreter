package main

import "core:fmt"
import "core:testing"
import "lexer"
import "token"
import "parser"

main :: proc() {
    input := `
    let x = 5;
    return 0;
    let foobar = 838383;
    `

    lex := lexer.new_lexer(input)
    par := parser.new_parser(lex)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)

    fmt.println(parser.get_program_string(program))
    defer parser.free_program(program)
}
