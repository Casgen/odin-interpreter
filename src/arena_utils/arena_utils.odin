package arena_utils

import "core:mem/virtual"
import "core:mem"
import "core:strings"
import "core:fmt"
import "core:slice"

push_struct :: proc {
    push_struct_type,
    push_struct_literal,
}

// Allocates a struct on the arena based on the given type
push_struct_type :: proc(
    arena: ^virtual.Arena,
    $T: typeid
) -> (^T, mem.Allocator_Error)  {

    data, err := virtual.arena_alloc(arena, size_of(T), mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None:
        return cast(^T)&data[0], err
    case .Invalid_Pointer:
        fmt.eprintfln("Failed to push struct! Invalid_Pointer!")
    case .Out_Of_Memory:
        fmt.eprintfln("Failed to push struct! Out Of Memory!")
    case .Mode_Not_Implemented:
        fmt.eprintfln("Failed to push struct! Mode not implemented!")
    case .Invalid_Argument:
        fmt.eprintfln("Failed to push struct! Invalid Argument!")
    }

    return nil, err
}

// Allocates a struct on the arena and sets the structs values.
// WARN: Note that this makes a shallow copy from the literal!
push_struct_literal :: proc(
    arena: ^virtual.Arena,
    arg: $E
) -> (^E, mem.Allocator_Error) {
    arg := arg

    data, err := virtual.arena_alloc(arena, size_of(E), mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None:
        // &data[0] makes pointer to the first element
        // &data would be just a pointer to the slice object!
        mem.copy(&data[0], &arg, size_of(E))
        return cast(^E)&data[0], err
    case .Invalid_Pointer:
        fmt.eprintfln("Failed to push struct! Invalid_Pointer!")
    case .Out_Of_Memory:
        fmt.eprintfln("Failed to push struct! Out Of Memory!")
    case .Mode_Not_Implemented:
        fmt.eprintfln("Failed to push struct! Mode not implemented!")
    case .Invalid_Argument:
        fmt.eprintfln("Failed to push struct! Invalid Argument!")
    }

    return nil, err
}

/*
Allocates a static array on the Arena. The array is not resizable!

NOTE: Should nil be returned or not? In C it is implementation defined.
Either it will return nullptr or not, but it shouldn't be dereferenced.
If it can't be dereferenced, than nullptr would be more predictable,
because that can't be dereferenced too.

en.cppreference.com/w/c/memory/malloc
*/
push_array :: proc(arena: ^virtual.Arena, obj: $T,
    length: u32) -> ([]T, mem.Allocator_Error) {

    if length == 0 do return nil

    data, err := virtual.arena_alloc(arena, size_of(E) * length,
        mem.DEFAULT_ALIGNMENT)

    switch err {
    case .None:
        return slice.reinterpret([]T, data), err
    case .Invalid_Pointer:
        fmt.eprintfln("Failed to push array! Invalid_Pointer!")
    case .Out_Of_Memory:
        fmt.eprintfln("Failed to push array! Out Of Memory!")
    case .Mode_Not_Implemented:
        fmt.eprintfln("Failed to push array! Mode not implemented!")
    case .Invalid_Argument:
        fmt.eprintfln("Failed to push array! Invalid Argument!")
    }

    return nil, err
}

/*
Accepts a dynamic array, allocates it in the arena, copies the data
and returns an array.

WARN: The result makes a shallow copy and the array is no longer resizable!

NOTE: Should nil be returned or not? In C it is implementation defined.
Either it will return nullptr or not, but it shouldn't be dereferenced.
If it can't be dereferenced, than nullptr would be more predictable,
because that can't be dereferenced too.

en.cppreference.com/w/c/memory/malloc
*/
push_dynamic_array :: proc(
    arena: ^virtual.Arena,
    dynamic_arr: [dynamic]$E
) -> ([]E, mem.Allocator_Error) {

    if len(dynamic_arr) == 0 {
        fmt.eprintfln("Warning! Pushed an array with 0 length!")
        return nil, .Invalid_Pointer
    }

    num_bytes: uint = size_of(E) * len(dynamic_arr)
    data, err := virtual.arena_alloc(arena, num_bytes, mem.DEFAULT_ALIGNMENT)

    switch err {
    case .Invalid_Pointer:
        fmt.eprintfln("Failed to push array! Invalid_Pointer!")
    case .Out_Of_Memory:
        fmt.eprintfln("Failed to push array! Out Of Memory!")
    case .Mode_Not_Implemented:
        fmt.eprintfln("Failed to push array! Mode not implemented!")
    case .Invalid_Argument:
        fmt.eprintfln("Failed to push array! Invalid Argument!")
    case .None:
        // Make a copy
        mem.copy(mem.raw_data(data), mem.raw_data(dynamic_arr),
            size_of(E) * len(dynamic_arr))

        // Cast to the slice with appropriate type
        return slice.reinterpret([]E, data), err
    }


    return nil, err
}

// Allocates a constant string on the arena.
push_string :: proc(
    arena: ^virtual.Arena,
    str: string
) -> (string, mem.Allocator_Error) {
    
    str_data := transmute([]u8)str
    new_data, err := virtual.arena_alloc(arena, len(str_data), mem.DEFAULT_ALIGNMENT)
    
    // Data from the string has to be copied!
    for i in 0..<len(str_data) {
        new_data[i] = str_data[i]
    }

    switch err {
    case .None:
        return transmute(string)new_data, err
    case .Invalid_Pointer:
        fmt.eprintfln("Failed to push string! Invalid_Pointer!")
    case .Out_Of_Memory:
        fmt.eprintfln("Failed to push string! Out Of Memory!")
    case .Mode_Not_Implemented:
        fmt.eprintfln("Failed to push string! Mode not implemented!")
    case .Invalid_Argument:
        fmt.eprintfln("Failed to push string! Invalid Argument!")
    }

    return "", err
}
