package object

import "core:mem/virtual"

EvaluatorCtx :: struct {
    arena: virtual.Arena,
    outer: ^EvaluatorCtx,
    identifier_map: map[string]Object,
}

// Creates an evaluator for evaluating the code. Serves as a context.
// Create this first and then pass it to the eval procedures!
create_evaluator_ctx :: proc(outer_ctx: ^EvaluatorCtx = nil) -> EvaluatorCtx {
    eval_context := EvaluatorCtx{
        arena = virtual.Arena{},
        identifier_map = map[string]Object{},
        outer = outer_ctx
    } 

    if err := virtual.arena_init_growing(&eval_context.arena); err != nil {
        panic("Failed to create an arena for Evaluator context!")
    }

    return eval_context
}

destroy_evaluator_ctx :: proc(eval_ctx: ^EvaluatorCtx) {
    for _, entry in eval_ctx.identifier_map {
        if fn, ok := entry.(^Function); ok {
            destroy_evaluator_ctx(&fn.eval_ctx)
        }
    }
    virtual.arena_destroy(&eval_ctx.arena)
    delete(eval_ctx.identifier_map)
}

ctx_get :: proc(eval_ctx: ^EvaluatorCtx, name: string) -> (Object, bool) {
    value, ok := eval_ctx.identifier_map[name]

    if !ok && eval_ctx.outer != nil {
        ctx_get(eval_ctx.outer, name)
    }

    return value, ok
}

ctx_set :: #force_inline proc(eval_ctx: ^EvaluatorCtx, name: string, obj: Object) -> Object {
    eval_ctx.identifier_map[name] = obj
    return obj
}
