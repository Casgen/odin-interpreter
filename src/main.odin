package main

import "core:fmt"
import "core:testing"
import "core:io"
import "core:os"
import "base:runtime"
import "core:reflect"
import "parser"
import "evaluator"

// main :: proc() {
//     reader: io.Reader
//     reader.data = nil
//     reader.procedure = evaluator.stdin_and_out_procedure
//
//     writer: io.Writer
//     writer.data = nil
//     writer.procedure = evaluator.stdin_and_out_procedure
//
//     evaluator.start(reader, writer)
// }

main :: proc() {
    input := "!!5"

    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)
    
    eval_ctx := evaluator.create_evaluator_ctx()
    defer evaluator.destroy_evaluator_ctx(&eval_ctx)

    result := evaluator.eval(&eval_ctx, program)
    fmt.print(result)
}
