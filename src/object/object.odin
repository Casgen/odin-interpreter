package object

import "core:fmt"
import "core:mem/virtual"
import "../arena_utils"

ObjectType :: string

INTEGER_OBJ :: "INTEGER"
BOOLEAN_OBJ :: "BOOLEAN"
NULL_OBJ :: "NULL"
RETURN_VALUE_OBJ :: "RETURN_VALUE"
ERROR_OBJ :: "ERROR"

TRUE  := Boolean{value = true}
FALSE := Boolean{value = false}
NULL := Null{}

Integer :: struct { value: i64 }
Boolean :: struct { value: bool }
ReturnValue :: struct { value: Object }
Null :: struct {}
Error :: struct { message: string }

Object :: union {
    ^Integer,
    ^Boolean,
    ^Null,
    ^ReturnValue,
    ^Error
}


new_integer :: proc(value: i64, arena: ^virtual.Arena = nil) -> ^Integer {
    if arena != nil {
        obj, err := arena_utils.push_struct(arena, Integer{value = value})
        return err == .None ? obj : nil
    }

    obj := new(Integer)
    obj.value = value

    return obj
}

new_boolean :: proc(value: bool, arena: ^virtual.Arena = nil) -> ^Boolean {
    if arena != nil {
        obj, err := arena_utils.push_struct(arena, Boolean{value = value})
        return err == .None ? obj : nil
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
        return err == .None ? obj : nil
    }

    obj := new(ReturnValue)
    obj.value = value

    return obj
}

type :: proc(obj: Object) -> ObjectType {
    switch obj_type in obj {
    case ^Integer: return INTEGER_OBJ
    case ^Boolean: return BOOLEAN_OBJ
    case ^Null: return NULL_OBJ
    case ^ReturnValue: return RETURN_VALUE_OBJ
    case ^Error: return ERROR_OBJ
    case: return fmt.aprintf("Unrecognized object type!")
    }
}

inspect_string :: proc(obj: Object) -> string {
    switch obj_type in obj {
    case ^Integer: return fmt.aprint(obj_type.value)
    case ^Boolean: return fmt.aprint(obj_type.value)
    case ^Null: return fmt.aprint("null")
    case ^ReturnValue: return inspect_string(obj_type.value)
    case ^Error: return obj_type.message
    case: return fmt.aprintf("Unrecognized object type!")
    }
}

free_object :: proc(obj: Object) {
    switch variant in obj {
    case ^Integer:
        free(variant)
    case ^Boolean:
        // Left blank. This is because we are using global references
        // to true/false constants in objects.
    case ^Null:
        // Left blank. This is because we are using global constant reference
        // to null
    case ^ReturnValue:
        free_object(variant.value)
        free(variant)
    case ^Error:
        delete(variant.message)
        free(variant)
    }
}
