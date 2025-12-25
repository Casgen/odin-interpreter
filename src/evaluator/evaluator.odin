package evaluator

import "core:reflect"
import "core:mem/virtual"
import "core:mem"
import sa "core:container/small_array"
import "core:fmt"
import "core:strings"

import "../object"
import "../parser"
import "../token"
import "../arena_utils"

eval_program :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    program: ^parser.Program
) -> object.Object {

    result: object.Object

    for &stmt in program.statements {
        result = eval_statement(eval_ctx, stmt)

        if result != nil {
            #partial switch variant in result {
            case ^object.Error:
                return result
            case ^object.ReturnValue:
                return variant.value
            }
        }
    }

    return result
}

eval_block_statement :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    block_stmt: ^parser.BlockStatement
) -> object.Object {
    result: object.Object

    for &stmt in block_stmt.statements {
        result = eval_statement(eval_ctx, stmt)

        if result != nil {
            #partial switch variant in result {
            case ^object.Error:
                return variant
            case ^object.ReturnValue:
                return variant
            }
        }
    }

    return result
}

eval_expression :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    expr_node: parser.Expression
) -> object.Object {

    switch expr in expr_node {
    case ^parser.IntegerLiteral:
        return object.new_integer(expr.value, &eval_ctx.arena)
    case ^parser.StringLiteral:
        return object.new_string(expr.value, &eval_ctx.arena)
    case ^parser.Boolean:
        return expr.value ? &object.TRUE : &object.FALSE
    case ^parser.IfExpression:
        condition := eval_expression(eval_ctx, expr.condition)

        if (is_error(condition)) {
            return condition
        }

        if is_truthy(condition) {
            return eval_block_statement(eval_ctx, expr.consequence)
        } else if expr.alternative != nil {
            return eval_block_statement(eval_ctx, expr.alternative)
        }

        // Has to return NULL, if `alternative` is not available.
        return &object.NULL
    case ^parser.Identifier:
        obj, ok := eval_ctx.identifier_map[expr.token.literal]

        if !ok {
            return object.new_error(
                "identifier not found: %s",
                expr.token.literal,
                arena=&eval_ctx.arena
            )
        }
        
        return obj
    case ^parser.PrefixExpression:
        right_expr := eval_expression(eval_ctx, expr.right)
        if (is_error(right_expr)) {
            return right_expr
        }
        return eval_prefix_expression(eval_ctx, expr.operator, right_expr)
    case ^parser.InfixExpression:
        left_obj := eval_expression(eval_ctx, expr.left)
        if (is_error(left_obj)) {
            return left_obj
        }

        right_obj := eval_expression(eval_ctx, expr.right)
        if (is_error(right_obj)) {
            return right_obj
        }
        return eval_infix_expression(eval_ctx, expr.operator, left_obj, right_obj)
    case ^parser.FunctionLiteral:
        return object.new_function(expr, eval_ctx)
    case ^parser.CallExpression:
        obj := eval_expression(eval_ctx, expr.function)

        if _, ok := obj.(^object.Error); ok {
            return obj
        }

        function, ok := obj.(^object.Function)
        if !ok {
            return object.new_error("not a function: %s", object.type(obj))
        }

        // Evaluate arguments
        args_results := make([]object.Object, len(expr.arguments))
        defer delete(args_results)
        
        for i in 0..<len(expr.arguments) {
            evaluated := eval_expression(eval_ctx, expr.arguments[i])
            if _, ok := evaluated.(^object.Error); ok {
                return evaluated
            }
            args_results[i] = evaluated
        }


        result := apply_function(eval_ctx, function, args_results)

        // We're destroying the function here immediately in case there is
        // a literal function defined and then a call right away.
        // Meaning something like this `fn(x) { x; }(5)`. Function doesn't
        // have any identifier associated and can not be tracked, therefore
        // its context resources have to be destroyed after the call.
        _, is_literal := expr.function.(^parser.FunctionLiteral);
        if is_literal {
            object.destroy_evaluator_ctx(&function.eval_ctx)
        }

        return result
    }

    return object.new_error("Unknown expression type!", arena=&eval_ctx.arena)
}

apply_function :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    fn: ^object.Function,
    args: []object.Object
) -> object.Object {
    // Get the parameter results and put them into the function EvaluatorCtx
    for i in 0..<len(args) {
        object.ctx_set(
            &fn.eval_ctx,
            fn.params[i].token.literal,
            args[i]
        )
    }

    // Begin a 'stack frame'. After the function is evaluated, reset it.
    temp_arena := virtual.arena_temp_begin(&fn.eval_ctx.arena)
    defer virtual.arena_temp_end(temp_arena)

    evaluated := eval_block_statement(&fn.eval_ctx, fn.body)

    if return_value, ok := evaluated.(^object.ReturnValue); ok {
        evaluated = return_value.value
    }

    // The object has to be copied from the evaluated function's environment
    // to the outer one to not lose the reference.
    context.allocator = virtual.arena_allocator(&eval_ctx.arena)
    evaluated = object.copy_object(evaluated)

    // Clone the environments identifiers and objects in case of a function.
    // This is useful for closures.
    if result_fn, is_result_fn := evaluated.(^object.Function); is_result_fn {
        arena_allocator := virtual.arena_allocator(&result_fn.eval_ctx.arena)
        for key, entry in fn.eval_ctx.identifier_map {
            saved_allocator := context.allocator
            context.allocator = arena_allocator
            copied_entry := object.copy_object(entry)
            context.allocator = saved_allocator

            result_fn.eval_ctx.identifier_map[key] = copied_entry
        }
    }

    return evaluated
}

is_truthy :: proc(obj: object.Object) -> bool {
    switch variant in obj {
    case ^object.Null:
        return false
    case ^object.Boolean:
        return variant.value
    case ^object.Integer:
        return variant.value > 0
    case ^object.String:
        panic("isTruthy not allowed on String!")
    case ^object.ReturnValue:
        panic("isTruthy not allowed on ReturnValue!")
    case ^object.Function:
        panic("isTruthy not allowed on a Function!")
    case ^object.Error:
        assert(variant != nil)
        return true;
    }

    return false
}

eval_prefix_expression :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    operator: string,
    right_obj: object.Object
) -> object.Object {
    switch operator {
    case "!":
        return eval_bang_operator_expression(eval_ctx, right_obj)
    case "-":
        if obj, ok := right_obj.(^object.Integer); ok {
            return object.new_integer(-obj.value, &eval_ctx.arena)
        }
    }

    return object.new_error(
        "unknown operator: %s%s",
        operator,
        object.type(right_obj),
        arena=&eval_ctx.arena
    )
}

eval_infix_expression :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    operator: string,
    left_obj, right_obj: object.Object
) -> object.Object {
    if (reflect.union_variant_typeid(left_obj) == ^object.Integer) &&
       (reflect.union_variant_typeid(right_obj) == ^object.Integer) {
        return eval_infix_integer_expression(eval_ctx, operator, left_obj, right_obj)
    }

    if (reflect.union_variant_typeid(left_obj) == ^object.Boolean) &&
       (reflect.union_variant_typeid(right_obj) == ^object.Boolean) {
        return eval_infix_bool_expression(eval_ctx, operator, left_obj, right_obj)
    }

    if (reflect.union_variant_typeid(left_obj) == ^object.String) &&
       (reflect.union_variant_typeid(right_obj) == ^object.String) {
        return eval_infix_string_expression(eval_ctx, operator, left_obj, right_obj)
    }

    return object.new_error(
        "type mismatch: %s %s %s",
        object.type(left_obj),
        operator,
        object.type(right_obj),
        arena=&eval_ctx.arena,
    )
}

// Expects that the left and right objects are of type object.Boolean!
// Otherwise it will panic!
eval_infix_bool_expression :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    operator: string,
    left_obj, right_obj: object.Object
) -> object.Object {
    left_value := left_obj.(^object.Boolean).value
    right_value := right_obj.(^object.Boolean).value
    switch operator {
        case "==": 
            return left_value == right_value ? &object.TRUE : &object.FALSE
        case "!=":
            return left_value != right_value ? &object.TRUE : &object.FALSE
    }

    return object.new_error(
        "unknown operator:  BOOLEAN %s BOOLEAN",
        operator,
        arena=&eval_ctx.arena
    )
}

// Expects that the left and right objects are of type object.Integer.
// Otherwise it will panic!
eval_infix_integer_expression :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    operator: string,
    left_obj, right_obj: object.Object
) -> object.Object {
    left_value := left_obj.(^object.Integer).value
    right_value := right_obj.(^object.Integer).value

    switch operator {
    // Number Operators
    case "+":
        return object.new_integer(left_value + right_value, &eval_ctx.arena)
    case "-":
        return object.new_integer(left_value - right_value, &eval_ctx.arena)
    case "*":
        return object.new_integer(left_value * right_value, &eval_ctx.arena)
    case "/":
        return object.new_integer(left_value / right_value, &eval_ctx.arena)
    // Boolean Operators
    case "==": 
        return left_value == right_value ? &object.TRUE : &object.FALSE
    case "!=":
        return left_value != right_value ? &object.TRUE : &object.FALSE
    case "<":
        return left_value < right_value ? &object.TRUE : &object.FALSE
    case ">":
        return left_value > right_value ? &object.TRUE : &object.FALSE
    }

    return object.new_error(
        "unknown operator: INTEGER %s INTEGER",
        operator,
        arena=&eval_ctx.arena,
    )
}

eval_infix_string_expression :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    operator: string,
    left_obj, right_obj: object.Object
) -> object.Object {
    left_value := left_obj.(^object.String).value
    right_value := right_obj.(^object.String).value

    switch operator {
    case "+":
        saved_allocator := context.allocator
        context.allocator = virtual.arena_allocator(&eval_ctx.arena)
        result := strings.concatenate({left_value, right_value})
        context.allocator = saved_allocator

        return object.new_string(result, &eval_ctx.arena)
    case "==":
        return left_value == right_value ? &object.TRUE : &object.FALSE
    }

    return object.new_error(
        "unknown operator: STRING %s STRING",
        operator,
        arena=&eval_ctx.arena,
    )
}

eval_bang_operator_expression :: proc(eval_ctx: ^object.EvaluatorCtx, obj: object.Object) -> object.Object {
    switch variant in obj {
    case ^object.Null: return &object.TRUE
    case ^object.Integer:
        return variant.value > 0 ? &object.FALSE : &object.TRUE
    case ^object.Boolean:
        return variant.value ? &object.FALSE : &object.TRUE
    case ^object.ReturnValue:
        panic("Bang operator (!) not allowed on ReturnValue!")
    case ^object.Function:
        panic("Bang operator (!) not allowed on Function!")
    case ^object.String:
        panic("Bang operator (!) not allowed on String!")
    case ^object.Error:
        assert(variant != nil)
        return &object.FALSE;
    }

    return &object.NULL
}

eval_statement :: proc(
    eval_ctx: ^object.EvaluatorCtx,
    stmt_node: parser.Statement
) -> object.Object {
    switch stmt in stmt_node {
    case ^parser.LetStatement:
        expr_obj := eval_expression(eval_ctx, stmt.value)
        if (is_error(expr_obj)) {
            return expr_obj
        }

        exists := stmt.ident.token.literal in eval_ctx.identifier_map
        if exists {
            return object.new_error(
                "Redifinition of identifier \"%s\"",
                stmt.ident.token.literal,
                arena=&eval_ctx.arena
            )
        }
        return object.ctx_set(eval_ctx, stmt.ident.token.literal, expr_obj) 

    case ^parser.ExpressionStatement:
        return eval_expression(eval_ctx, stmt.expr)
    case ^parser.ReturnStatement:
        return_result := eval_expression(eval_ctx, stmt.value)
        if (is_error(return_result)) {
            return return_result
        }
        return object.new_return_value(return_result, &eval_ctx.arena)
 
    }

    return object.new_error("Unknown statement!", arena=&eval_ctx.arena)
}

is_error :: proc(obj: object.Object) -> bool {
    if obj != nil {
        return object.type(obj) == object.ERROR_OBJ
    }

    return false
}
