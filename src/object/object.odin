package object

import "core:fmt"
import "core:mem/virtual"
import "core:strings"

import "../parser"
import "../arena_utils"
import "../utils"

ObjectType :: string

INTEGER_OBJ         :: "INTEGER"
STRING_OBJ          :: "STRING"
BOOLEAN_OBJ         :: "BOOLEAN"
NULL_OBJ            :: "NULL"
RETURN_VALUE_OBJ    :: "RETURN_VALUE"
ERROR_OBJ           :: "ERROR"
FUNCTION_OBJ        :: "FUNCTION_OBJ"

TRUE  := Boolean{value = true}
FALSE := Boolean{value = false}
NULL := Null{}

Integer :: struct { value: i64 }
String :: struct { value: string }
Boolean :: struct { value: bool }
ReturnValue :: struct { value: Object }
Null :: struct {}
Error :: struct { message: string }
Function :: struct {
    params: []parser.Identifier,    // Has to be copied from AST!
    body: ^parser.BlockStatement,   // Has to be copied from AST!
    eval_ctx: EvaluatorCtx,
}

Object :: union {
    ^Integer,
    ^String,
    ^Boolean,
    ^Null,
    ^ReturnValue,
    ^Error,
    ^Function
}

BuiltIn :: struct {

}


new_integer :: proc(value: i64, arena: ^virtual.Arena = nil) -> ^Integer {
    if arena != nil {
        obj, err := virtual.new(arena, Integer)
        obj.value = value

        fmt.assertf(err == .None,
            "Failed to allocated an Integer object '%v'!", err)

        return obj
    }

    obj := new(Integer)
    obj.value = value

    return obj
}

new_string :: proc(value: string, arena: ^virtual.Arena = nil) -> ^String {
    if arena != nil {
        obj, err := virtual.new(arena, String)
        fmt.assertf(err == .None, "Failed to allocate a String object! %v",
            err)

        value_clone, value_ok := arena_utils.push_string(arena, value)
        fmt.assertf(value_ok == .None,
            "Failed to allocate a String literal value to a String object! %v",
            value_ok)
        obj.value = value_clone

        return obj
    }
    obj := new(String)
    obj.value = strings.clone(value)

    return obj
}

new_boolean :: proc(value: bool, arena: ^virtual.Arena = nil) -> ^Boolean {
    if arena != nil {
        obj, err := arena_utils.push_struct(arena, Boolean{value = value})
        fmt.assertf(err == .None,
            "Failed to allocated an Boolean object '%v'!", err)

        return obj
    }

    obj := new(Boolean)
    obj.value = value

    return obj
}

new_return_value :: proc(
    value: Object,
    arena: ^virtual.Arena = nil
) -> ^ReturnValue {
    if arena != nil {
        obj, err := arena_utils.push_struct(arena, ReturnValue{value = value})
        fmt.assertf(err == .None,
            "Failed to allocated an ReturnValue object '%v'!", err)

        return obj
    }

    obj := new(ReturnValue)
    obj.value = value

    return obj
}

new_error :: proc(msg: string, args: ..any, arena: ^virtual.Arena = nil) -> ^Error {
    if arena != nil {
        // First create the formatted message
        fmt_str := fmt.aprintf(msg, args)
        defer delete(fmt_str)
        // Then allocate
        message, msg_ok := arena_utils.push_string(arena, fmt_str)
        fmt.assertf(msg_ok == .None,
            "Failed to allocate an error string message! %v", msg_ok)

        obj, err := arena_utils.push_struct(arena, Error{ message = message })
        fmt.assertf(err == .None,
            "Failed to allocate an Error object! %v", err)

        return obj
    }

    obj := new(Error)
    obj.message = fmt.aprintf(msg, args)

    return obj
}

new_function :: proc(
    fn_literal: ^parser.FunctionLiteral,
    eval_ctx: ^EvaluatorCtx,
) -> Object {
    fn, fn_err := virtual.new(&eval_ctx.arena, Function) 
    fmt.assertf(fn_err == .None, "Failed to allocate a new function!")

    fn.eval_ctx = create_evaluator_ctx(eval_ctx)    
    {
        // TODO: Should params and body really live outside of the function?
        context.allocator = virtual.arena_allocator(&fn.eval_ctx.arena)
        fn.params = parser.copy_identifiers(fn_literal.params)
        fn.body = parser.copy_block_statement(fn_literal.body)
    }

    return fn
}

type :: proc(obj: Object) -> ObjectType {
    switch obj_type in obj {
    case ^Integer: return INTEGER_OBJ
    case ^String: return STRING_OBJ
    case ^Boolean: return BOOLEAN_OBJ
    case ^Null: return NULL_OBJ
    case ^ReturnValue: return RETURN_VALUE_OBJ
    case ^Error: return ERROR_OBJ
    case ^Function: return FUNCTION_OBJ
    case: return "UNKNOWN"
    }
}

inspect_string :: proc(obj: Object) -> string {
    switch obj_type in obj {
    case ^Integer: return fmt.aprint(obj_type.value)
    case ^String: return fmt.aprint(obj_type.value)
    case ^Boolean: return fmt.aprint(obj_type.value)
    case ^Null: return fmt.aprint("null")
    case ^ReturnValue: return inspect_string(obj_type.value)
    case ^Error: return obj_type.message
    case ^Function:
        str_builder := strings.builder_make()
        strings.write_string(&str_builder, "fn(")

        for &par, i in obj_type.params {
            strings.write_string(&str_builder, par.token.literal)

            if i < len(obj_type.params) - 1 {
                strings.write_string(&str_builder, ", ")
            }
        }
        strings.write_string(&str_builder, ") {\n")
        parser.write_block_statement(&str_builder, obj_type.body, false)
        strings.write_string(&str_builder, "\n}")
        return strings.to_string(str_builder)
    case: return "UNKNOWN"
    }
}

free_object :: proc(obj: Object) {
    switch variant in obj {
    case ^Function:
        // Params and body are part of the outer contexts arena. No need to free them
        delete(variant.eval_ctx.identifier_map)
        virtual.arena_destroy(&variant.eval_ctx.arena)
        free(variant)
    case ^Integer:
        free(variant)
    case ^String:
        delete(variant.value)
        free(variant)
    case ^ReturnValue:
        free_object(variant.value)
        free(variant)
    case ^Error:
        delete(variant.message)
        free(variant)
    // Below cases are left blank because they point to a static global
    // variable. They don't need to be freed.
    case ^Boolean:
    case ^Null:
    }
}

copy_object :: proc(obj: Object) -> Object {
    switch variant in obj {
    case ^Integer:
        integer := new(Integer)
        integer.value = variant.value

        return integer
    case ^String:
        string_obj := new(String)
        string_obj.value = strings.clone(variant.value)

        return string_obj
    case ^ReturnValue:
        ret_value := new(ReturnValue)
        ret_value.value = copy_object(variant.value)

        return ret_value
    case ^Error:
        err_msg, err := strings.clone(variant.message)
        fmt.assertf(err == .None,
            "Failed to copy an error message '%v'!", err)

        err_obj := new(Error)
        err_obj.message = err_msg

        return err_obj
    case ^Function:
        function := new(Function)
        function.body = parser.copy_block_statement(variant.body)
        function.params = parser.copy_identifiers(variant.params)
        function.eval_ctx = create_evaluator_ctx(variant.eval_ctx.outer)
        arena_allocator := virtual.arena_allocator(&function.eval_ctx.arena)

        for key, entry in variant.eval_ctx.identifier_map {
            // Clone the environments identifiers and objects.
            // This is useful for closures.
            saved_allocator := context.allocator
            context.allocator = arena_allocator
            copied_entry := copy_object(entry)
            context.allocator = saved_allocator

            function.eval_ctx.identifier_map[key] = copied_entry
        }

        return function
    // Just return the object back, since a boolean and null SHOULD
    // point to the global constant values
    case ^Boolean: return obj
    case ^Null: return obj
    }

    return nil
}
