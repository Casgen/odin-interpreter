package main

import "core:fmt"
import "core:testing"
import "lexer"
import "token"
import "parser"

main :: proc() {
    input := "if (x < y) { x } else { y }"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

   fmt.println(parser.get_program_string(program))
}
