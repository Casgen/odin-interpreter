package main

import "core:fmt"
import "core:testing"
import "repl"
import "core:io"
import "core:os"
import "base:runtime"
import "core:reflect"

stdin_and_out_procedure ::proc(
    stream_data: rawptr,
    mode: io.Stream_Mode,
    p: []byte,
    offset: i64,
    whence: io.Seek_From
) -> (n: i64, err: io.Error)  {

    switch mode {
    case .Read:
        n_bytes, read_err := os.read(os.stdin, p)

        if read_err == nil {
            return i64(n_bytes), .None
        }

        if _, ok := read_err.(io.Error); !ok {
            fmt.eprintfln("Failed to read! %v",
                reflect.union_variant_typeid(read_err))
            return -1, .Unknown
        } 

        return -1, read_err.(io.Error)
    case .Query:
        return i64(io.Stream_Mode_Set{.Read, .Read_At, .Size, .Write}), .None
    case .Read_At:
        n_bytes, read_err := os.read_at(os.stdin, p, offset)

        if read_err == nil {
            return i64(n_bytes), .None
        }

        if _, ok := read_err.(io.Error); !ok {
            fmt.eprintfln("read_at has Failed! %v",
                reflect.union_variant_typeid(read_err))
            return -1, .Unknown
        } 

        return -1, read_err.(io.Error)
    case .Size:
        result, file_size_err := os.file_size(os.stdin)
        if file_size_err == nil {
            return i64(result), .None
        }

        if _, ok := file_size_err.(io.Error); !ok {
            fmt.eprintfln("Getting file size has Failed! %v",
                reflect.union_variant_typeid(file_size_err))
            return -1, .Unknown
        }

        return -1, file_size_err.(io.Error)
    case .Write:
        result, write_err := os.write(os.stdout, p)
        if write_err == nil {
            return i64(result), .None
        }

        if _, ok := write_err.(io.Error); !ok {
            fmt.eprintfln("Writing to Stdout has failed! %v",
                reflect.union_variant_typeid(write_err))
            return -1, .Unknown
        }

        return -1, write_err.(io.Error)
    case .Write_At: fallthrough
    case .Close: fallthrough
    case .Flush: fallthrough
    case .Seek: fallthrough
    case .Destroy:
        return 0, .Empty
    }

    return -1,  .Unknown
}

main :: proc() {
    reader: io.Reader
    reader.data = nil
    reader.procedure = stdin_and_out_procedure

    writer: io.Writer
    writer.data = nil
    writer.procedure = stdin_and_out_procedure

    repl.start(reader, writer)
}

// main :: proc() {
//     input := "add(1, 2 * 3, 4 + 5)"
//
//     par := parser.new_parser(input)
//     defer parser.destroy_parser(par)
//
//     program := parser.parse_program(par)
//     defer parser.free_program(program)
//
//    fmt.println(parser.get_program_string(program))
// }
