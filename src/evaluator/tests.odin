package evaluator

import "core:testing"
import "core:reflect"

import "../object"
import "../parser"
import "../evaluator"


test_eval :: proc(eval_ctx: ^EvaluatorCtx, input: string) -> object.Object {
    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    return evaluator.eval(eval_ctx, program)
}

test_integer_object :: proc(
    t: ^testing.T,
    obj: object.Object,
    expected: i64,
) -> bool {
    testing.expectf(t, obj != nil, "Object is nil! Expected Integer")

    result, ok := obj.(^object.Integer)

    testing.expectf(t, ok, "Unexpected object type! Expected Integer, got %v",
        reflect.union_variant_typeid(obj)) or_return

    testing.expectf(t, result.value == expected,
        "Unexpected object type! Expected %d, got %d",
        expected, result.value) or_return

    return true
}

test_bool_object :: proc(
    t: ^testing.T,
    obj: object.Object,
    expected: bool,
) -> bool {
    testing.expectf(t, obj != nil, "Object is nil! Expected Boolean")
    result, ok := obj.(^object.Boolean)

    testing.expectf(t, ok, "Unexpected object type! Expected Boolean, got %v",
        reflect.union_variant_typeid(obj)) or_return

    testing.expectf(t, result.value == expected,
        "Unexpected object type! Expected %v, got %v",
        expected, result.value) or_return

    return true
}

test_null_object :: proc(t: ^testing.T, obj: object.Object) -> bool {
    testing.expectf(t, obj != nil, "Object is nil! Expected Null Object")
    result, ok := obj.(^object.Null)

    testing.expectf(t, ok, "Unexpected object type! Expected Null, got %v",
        reflect.union_variant_typeid(obj)) or_return

    testing.expect(t, result == &object.NULL,
        "Null isn't referencing the constant NULL!") or_return

    return true
}

@(test)
test_eval_integer_expression :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: i64,
    }

    tests := [?]Tests{
        { "5", 5 },
        { "10", 10 },
        { "-5", -5 },
        { "-10", -10 },
        {"5 + 5 + 5 + 5 - 10", 10},
        {"2 * 2 * 2 * 2 * 2", 32},
        {"-50 + 100 + -50", 0},
        {"5 * 2 + 10", 20},
        {"5 + 2 * 10", 25},
        {"20 + 2 * -10", 0},
        {"50 / 2 * 2 + 10", 60},
        {"2 * (5 + 10)", 30},
        {"3 * 3 * 3 + 10", 37},
        {"3 * (3 * 3) + 10", 37},
        {"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50},
    }

    for &tt in tests {
        eval_ctx := evaluator.create_evaluator_ctx()
        defer evaluator.destroy_evaluator_ctx(&eval_ctx)

        result_obj := test_eval(&eval_ctx, tt.input)
        test_integer_object(t, result_obj, tt.expected)
    }
}

@(test)
test_eval_bool_expression :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: bool,
    }

    tests := [?]Tests{
        { "true", true },
        { "false", false },
        { "1 < 2", true },
        { "1 > 2", false },
        { "1 < 1", false },
        { "1 > 1", false },
        { "1 == 1", true },
        { "1 != 1", false },
        { "1 == 2", false },
        { "1 != 2", true },
        { "true == true", true },
        { "false == false", true },
        { "true == false", false },
        { "true != false", true },
        { "false != true", true },
        { "(1 < 2) == true", true },
        { "(1 < 2) == false", false },
        { "(1 > 2) == true", false },
        { "(1 > 2) == false", true },
    }

    for &tt in tests {
        eval_ctx := evaluator.create_evaluator_ctx()
        defer evaluator.destroy_evaluator_ctx(&eval_ctx)

        result_obj := test_eval(&eval_ctx, tt.input)
        test_bool_object(t, result_obj, tt.expected)
    }
}

@(test)
test_bang_operator :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: bool,
    }

    tests := [?]Tests{
        {"!true", false},
        {"!false", true},
        {"!5", false},
        {"!!true", true},
        {"!!false", false},
        {"!!5", true},
    }

    for &tt in tests {
        eval_ctx := evaluator.create_evaluator_ctx()
        defer evaluator.destroy_evaluator_ctx(&eval_ctx)

        result_obj := test_eval(&eval_ctx, tt.input)
        test_bool_object(t, result_obj, tt.expected)
    }
}

@(test)
test_if_else_expression :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: union {i64}
    }

    tests := [?]Tests{
        {"if (true) { 10 }", 10},
        {"if (false) { 10 }", nil},
        {"if (1) { 10 }", 10},
        {"if (1 < 2) { 10 }", 10},
        {"if (1 > 2) { 10 }", nil},
        {"if (1 > 2) { 10 } else { 20 }", 20},
        {"if (1 < 2) { 10 } else { 20 }", 10},
    }

    for &tt in tests {
        eval_ctx := evaluator.create_evaluator_ctx()
        defer evaluator.destroy_evaluator_ctx(&eval_ctx)

        result_obj := test_eval(&eval_ctx, tt.input)

        exp_value, ok := tt.expected.(i64)

        if ok {
            test_integer_object(t, result_obj, tt.expected.(i64))
        } else {
            test_null_object(t, result_obj)
        }
    }
}

@(test)
test_return_statements :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: i64
    }

    tests := [?]Tests{
        {"return 10;", 10},
        {"return 10; 9;", 10},
        {"return 2 * 5; 9;", 10},
        {"9; return 2 * 5; 9;", 10},
        {
        `
        if (10 > 1) {
            if (10 > 1) {
                return 10;
            }

            return 1;
        }
        `, 10
        }
    }

    for &test in tests {
        eval_ctx := evaluator.create_evaluator_ctx()
        defer evaluator.destroy_evaluator_ctx(&eval_ctx)

        evaluated := test_eval(&eval_ctx, test.input)
        test_integer_object(t, evaluated, test.expected)
    }
}

@(test)
test_error_handling :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected_message: string
    }

    tests := [?]Tests{
        {
            "5 + true;",
            "type mismatch: INTEGER + BOOLEAN",
        },
        {
            "5 + true; 5;",
            "type mismatch: INTEGER + BOOLEAN",
        },
        {
            "-true",
            "unknown operator: -BOOLEAN",
        },
        {
            "true + false;",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            "5; true + false; 5",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
        `
        if (10 > 1) {
            if (10 > 1) {
                return true + false;
            }
        return 1;
        }
        `, "unknown operator: BOOLEAN + BOOLEAN",
        },
    }

    for &test in tests {
        eval_ctx := create_evaluator_ctx()
        defer destroy_evaluator_ctx(&eval_ctx)

        evaluated := test_eval(&eval_ctx, test.input)
        err_obj, ok := evaluated.(^object.Error)

        if testing.expectf(t, ok, "No error object returned, got=%v",
            reflect.union_variant_typeid(evaluated)) {
            continue
        }

        testing.expectf(t, err_obj.message == test.expected_message,
            "wrong error message. expected=%s, got=%s",
            test.expected_message, err_obj.message)
    }
}
