package evaluator

import "core:reflect"
import "core:mem/virtual"

import "../object"
import "../parser"
import "../token"

EvaluatorCtx :: struct {
    arena: virtual.Arena,
}

eval :: proc {
    eval_expression,
    eval_statement,
    eval_program,
}

// Creates an evaluator for evaluating the code. Serves as a context.
// Create this first and then pass it to the eval procedures!
create_evaluator_ctx :: proc() -> EvaluatorCtx {
    eval_context := EvaluatorCtx{ arena = virtual.Arena{} } 

    if err := virtual.arena_init_growing(&eval_context.arena); err != nil {
        panic("Failed to create an arena for Evaluator context!")
    }

    return eval_context
}

destroy_evaluator_ctx :: proc(eval_ctx: ^EvaluatorCtx) {
    virtual.arena_destroy(&eval_ctx.arena)
}

eval_program :: proc(
    eval_ctx: ^EvaluatorCtx,
    program: ^parser.Program
) -> object.Object {

    result: object.Object

    for &stmt in program.statements {
        result = eval_statement(eval_ctx, stmt)

        if ret_value, ok := result.(^object.ReturnValue); ok {
            return ret_value.value
        }
    }

    return result
}

eval_block_statement :: proc(
    eval_ctx: ^EvaluatorCtx,
    block_stmt: ^parser.BlockStatement
) -> object.Object {
    result: object.Object

    for &stmt in block_stmt.statements {
        result = eval_statement(eval_ctx, stmt)
        
        if result != nil && object.type(result) == object.RETURN_VALUE_OBJ {
            return result
        }
    }

    return result
}

eval_expression :: proc(
    eval_ctx: ^EvaluatorCtx,
    expr_node: parser.Expression
) -> object.Object {
    switch expr in expr_node {
    case ^parser.IntegerLiteral:
        return object.new_integer(expr.value, &eval_ctx.arena)
    case ^parser.Boolean:
        return expr.value ? &object.TRUE : &object.FALSE
    case ^parser.IfExpression:
        condition := eval_expression(eval_ctx, expr.condition)

        if is_truthy(condition) {
            return eval_block_statement(eval_ctx, expr.consequence)
        } else if expr.alternative != nil {
            return eval_block_statement(eval_ctx, expr.alternative)
        }
        
        return &object.NULL
    case ^parser.Identifier: panic("Unimplemented!")
    case ^parser.PrefixExpression:
        right_expr := eval_expression(eval_ctx, expr.right)
        return eval_prefix_expression(eval_ctx, expr.operator, right_expr)
    case ^parser.InfixExpression:
        left_obj := eval_expression(eval_ctx, expr.left)
        right_obj := eval_expression(eval_ctx, expr.right)
        return eval_infix_expression(eval_ctx, expr.operator, left_obj, right_obj)
    case ^parser.FunctionLiteral: panic("Unimplemented!")
    case ^parser.CallExpression: panic("Unimplemented!")
    }

    return nil
}

is_truthy :: proc(obj: object.Object) -> bool {
    switch variant in obj {
    case ^object.Null:
    case ^object.Boolean:
        return variant.value
    case ^object.Integer:
        return variant.value > 0
    case ^object.ReturnValue:
        panic("isTruthy not allowed on ReturnValue!")
    }

    return false
}

eval_prefix_expression :: proc(
    eval_ctx: ^EvaluatorCtx,
    operator: string,
    right_obj: object.Object
) -> object.Object {
    switch operator {
    case "!": return eval_bang_operator_expression(right_obj)
    case "-":
        if obj, ok := right_obj.(^object.Integer); ok {
            return object.new_integer(-obj.value, &eval_ctx.arena)
        }
    }

    return &object.NULL
}

eval_infix_expression :: proc(
    eval_ctx: ^EvaluatorCtx,
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
    return &object.NULL
}

// Expects that the left and right objects are of type object.Boolean!
// Otherwise it will panic!
eval_infix_bool_expression :: proc(
    eval_ctx: ^EvaluatorCtx,
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

    return &object.NULL
}

// Expects that the left and right objects are of type object.Integer.
// Otherwise it will panic!
eval_infix_integer_expression :: proc(
    eval_ctx: ^EvaluatorCtx,
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
        return object.new_boolean(left_value == right_value, &eval_ctx.arena)
    case "!=":
        return object.new_boolean(left_value != right_value, &eval_ctx.arena)
    case "<":
        return object.new_boolean(left_value < right_value, &eval_ctx.arena)
    case ">":
        return object.new_boolean(left_value > right_value, &eval_ctx.arena)
    }

    return &object.NULL
}

eval_bang_operator_expression :: proc(obj: object.Object) -> object.Object {
    switch variant in obj {
    case ^object.Null: return &object.TRUE
    case ^object.Integer:
        return variant.value > 0 ? &object.FALSE : &object.TRUE
    case ^object.Boolean:
        return variant.value ? &object.FALSE : &object.TRUE
    case ^object.ReturnValue:
        panic("Bang operator (!) not allowed on ReturnValue!")
    }

    return &object.NULL
}

eval_statement :: proc(
    eval_ctx: ^EvaluatorCtx,
    stmt_node: parser.Statement
) -> object.Object {
    switch stmt in stmt_node {
    case ^parser.LetStatement:
    case ^parser.ExpressionStatement:
        return eval_expression(eval_ctx, stmt.expr)
    case ^parser.ReturnStatement:
        return_result := eval_expression(eval_ctx, stmt.value)
        return_value_obj := object.new_return_value(
            return_result,
            &eval_ctx.arena
        )
        return return_value_obj
    }

    return nil
}

// TODO: create error handling
