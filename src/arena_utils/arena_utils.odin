package arena_utils

import "core:mem/virtual"
import "core:mem"

push_struct :: proc {
    push_struct_type,
    push_struct_literal,
}

push_struct_type :: proc(arena: ^virtual.Arena, $T: typeid) -> ^T {

    data, err := virtual.arena_alloc(arena, size_of(T), mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None: return transmute(T)data 
    case .Invalid_Pointer: panic("Failed to push struct! Invalid_Pointer!")
    case .Out_Of_Memory: panic("Failed to push struct! Out Of Memory!")
    case .Mode_Not_Implemented: panic("Failed to push struct! Out Of Memory!")
    case .Invalid_Argument: panic("Failed to push struct! Invalid Argument!")
    }

    return nil
}

push_struct_literal :: proc(arena: ^virtual.Arena, arg: $E) -> ^E {

    data, err := virtual.arena_alloc(arena, size_of(E), mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None: return transmute(E)data 
    case .Invalid_Pointer: panic("Failed to push struct! Invalid_Pointer!")
    case .Out_Of_Memory: panic("Failed to push struct! Out Of Memory!")
    case .Mode_Not_Implemented: panic("Failed to push struct! Out Of Memory!")
    case .Invalid_Argument: panic("Failed to push struct! Invalid Argument!")
    }

    return nil
}

push_array :: proc(arena: ^virtual.Arena, obj: $T,
    length: u32) -> []T {

    if length == 0 do return nil

    data, err := virtual.arena_alloc(arena, size_of(E) * length,
        mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None: return transmute(T)data 
    case .Invalid_Pointer: panic("Failed to push array! Invalid_Pointer!")
    case .Out_Of_Memory: panic("Failed to push array! Out Of Memory!")
    case .Mode_Not_Implemented: panic("Failed to push array! Out Of Memory!")
    case .Invalid_Argument: panic("Failed to push array! Invalid Argument!")
    }

    return nil

}

push_string :: proc(arena: ^virtual.Arena, str: string) -> string {
    
    str_data := transmute([]u8)str
    new_data, err := virtual.arena_alloc(arena, len(str_data), mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None: return transmute(string)new_data 
    case .Invalid_Pointer: panic("Failed to push string! Invalid_Pointer!")
    case .Out_Of_Memory: panic("Failed to push string! Out Of Memory!")
    case .Mode_Not_Implemented: panic("Failed to push string! Out Of Memory!")
    case .Invalid_Argument: panic("Failed to push string! Invalid Argument!")
    }

    return ""
}
