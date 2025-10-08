package evaluator

import "core:os"
import "core:fmt"
import "core:strings"
import "core:slice"
import "core:bufio"
import "core:io"
import "core:reflect"
import "../evaluator"

import "../parser"
import "../object"

PROMPT :: ">> "

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

start :: proc(reader: io.Reader, writer: io.Writer) {

    bufio_reader: bufio.Reader

    bufio.reader_init(&bufio_reader, reader)

    fmt.println("Welcome to the Monkey programming language!")

    for {
        fmt.print(PROMPT)

        input, err := bufio.reader_read_bytes(&bufio_reader, '\n')
        defer delete(input)
        if err != .None {
            fmt.eprintfln("Failed to read terminal input! %v. Quitting...", err)
            return
        }

        if err != nil {
            fmt.eprintfln("Failed to read input! %v", err)
            return
        }

        input_str := transmute(string)input

        if input_str == "exit\n" {
            fmt.println("Bye bye!")
            // No need to delete the input, the defer above already does
            // that for us.
            break
        }

        par := parser.new_parser(transmute(string)input)
        defer parser.destroy_parser(par)

        program := parser.parse_program(par)
        defer parser.free_program(program)

        if len(par.errors) != 0 {
            print_parser_errors(par.errors[:])
            continue
        }

        eval_ctx := evaluator.create_evaluator_ctx()
        defer evaluator.destroy_evaluator_ctx(&eval_ctx)

        evaluated := eval_program(&eval_ctx, program)
        if evaluated != nil {
            _, _ = io.write_string(
                writer,
                object.inspect_string(evaluated)
            )
            _ = io.write_byte(writer, '\n')
        }

    }

}

print_parser_errors :: proc(error_msgs: []string) {
    for &msg in error_msgs {
        fmt.eprintfln("\t%s\n", msg)
    }
}
