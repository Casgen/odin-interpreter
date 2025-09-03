package repl

import "core:os"
import "core:fmt"
import "core:strings"
import "core:slice"
import "core:bufio"
import "core:io"

import "../parser"

PROMPT :: ">> "

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

        program_str := parser.get_program_string(program)
        defer delete(program_str)

        fmt.println(program_str)
    }

}

print_parser_errors :: proc(error_msgs: []string) {
    for &msg in error_msgs {
        fmt.eprintfln("\t%s\n", msg)
    }
}

