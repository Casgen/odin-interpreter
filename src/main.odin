package main

import "core:fmt"
import "core:testing"
import "core:io"
import "core:os"
import "base:runtime"
import "core:reflect"
import "core:mem"

import "parser"
import "evaluator"
import "object"

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

test_eval :: proc(input: string, ctx: ^object.EvaluatorCtx) -> object.Object {
    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    return evaluator.eval_program(ctx, program)
}

main :: proc() {
    input := `
    "foo bar"
    `

    track_allocator: mem.Tracking_Allocator
    mem.tracking_allocator_init(&track_allocator, context.allocator)
    context.allocator = mem.tracking_allocator(&track_allocator)

    defer {
        if len(track_allocator.allocation_map) > 0 {
            fmt.eprintf("=== %v allocations not freed: ===\n", len(track_allocator.allocation_map))
            for _, entry in track_allocator.allocation_map {
                fmt.eprintf("- %v bytes @ %v (Ptr: %#X)\n", entry.size, entry.location, entry.memory)
            }
        }
        mem.tracking_allocator_destroy(&track_allocator)
    }

    eval_ctx := object.create_evaluator_ctx()
    defer object.destroy_evaluator_ctx(&eval_ctx)

    evaluated := test_eval(input, &eval_ctx)
    fmt.print(evaluated)
}
