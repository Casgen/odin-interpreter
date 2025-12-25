package evaluator

import "core:testing"
import "core:reflect"
import "core:strings"

import "../object"
import "../parser"
import "../evaluator"


test_eval :: proc(input: string) -> object.Object {
    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    eval_ctx := object.create_evaluator_ctx()
    defer object.destroy_evaluator_ctx(&eval_ctx)

    result := evaluator.eval_program(&eval_ctx, program)
    return object.copy_object(result)
}

test_eval_with_ctx :: proc(input: string, eval_ctx: ^object.EvaluatorCtx) -> object.Object {
    par := parser.new_parser(input)
    defer parser.destroy_parser(par)

    program := parser.parse_program(par)
    defer parser.free_program(program)

    return evaluator.eval_program(eval_ctx, program)
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

    for &test in tests {
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

        test_integer_object(t, evaluated, test.expected)
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

    for &test in tests {
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

        test_bool_object(t, evaluated, test.expected)
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

    for &test in tests {
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

        test_bool_object(t, evaluated, test.expected)
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

    for &test in tests {
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

        exp_value, ok := test.expected.(i64)

        if ok {
            test_integer_object(t, evaluated, test.expected.(i64))
        } else {
            test_null_object(t, evaluated)
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
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

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
            "foobar",
            "identifier not found: foobar",
        },
        {
            `
            if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
            return 1;
            }
            `,
            "unknown operator: BOOLEAN + BOOLEAN",
        },
        {
            `"Hello" - "World"`,
            "unknown operator: STRING - STRING",
        }
    }

    for &test in tests {
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

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

@(test)
test_let_statements :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: i64,
    }

    tests := [?]Tests{
        {"let a = 5; a;", 5},
        {"let a = 5 * 5; a;", 25},
        {"let a = 5; let b = a; b;", 5},
        {"let a = 5; let b = a; let c = a + b + 5; c;", 15},
    }

    for &test in tests {
        evaluated := test_eval(test.input)
        defer object.free_object(evaluated)

        test_integer_object(t, evaluated, test.expected)
    }
}

@(test)
test_function_object :: proc(t: ^testing.T) {
    input := "fn(x) { x + 2; }"

    eval_ctx := object.create_evaluator_ctx()
    defer object.destroy_evaluator_ctx(&eval_ctx)

    evaluated := test_eval_with_ctx(input, &eval_ctx)

    fn, ok := evaluated.(^object.Function)

    if testing.expectf(t, ok, "Object is not a function! got %v",
        reflect.union_variant_typeid(evaluated)) {
        return
    }

    if testing.expectf(t, len(fn.params) == 1,
        "Wrong number of parameters! expected 1, got %d", len(fn.params)) {
        return
    }

    if testing.expectf(t, fn.params[0].token.literal == "x",
        "Parameter is not 'x', got '%s'", fn.params[0].token.literal) {
        return
    }

    expected_body := "(x + 2)"

    str_builder: strings.Builder
    parser.write_block_statement(&str_builder, fn.body, false)
    actual_body := strings.to_string(str_builder)
    defer delete(actual_body)

    if testing.expectf(t, expected_body == actual_body,
        "Body is not '%s', got '%s'", expected_body, actual_body) {
        return
    }

}

@(test)
test_function_application :: proc(t: ^testing.T) {
    Tests :: struct {
        input: string,
        expected: i64
    }

    tests := [?]Tests{
        {"let identity = fn(x) { x; }; identity(5);", 5},
        {"let identity = fn(x) { return x; }; identity(5);", 5},
        {"let double = fn(x) { x * 2; }; double(5);", 10},
        {"let add = fn(x, y) { x + y; }; add(5, 5);", 10},
        {"let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20},
        {"fn(x) { x; }(5)", 5},
        {
            `
            let add = fn(x, y) { x + y; };
            let sub = fn(x, y) { x - y; };
            let multiply = fn(x, y) { x * y; };
            let divide = fn(x, y) {x / y;};

            let count = 10;
            let result = add(multiply(count + 10, 20), divide(20, 10));
            sub(result, -10);
            `,
            412
        },
        {
            `
            let newAdder = fn(x) { fn(n) { x + n } };
            let addTwo = newAdder(2);
            addTwo(2);
            `,
            4
        }
    }
    for test in tests {
        eval_ctx := object.create_evaluator_ctx();
        defer object.destroy_evaluator_ctx(&eval_ctx);

        evaluated := test_eval_with_ctx(test.input, &eval_ctx)

        test_integer_object(t, evaluated, test.expected)
    }
}

@(test)
// evaluator/evaluator_test.go
test_string_concatenation :: proc(t: ^testing.T) {
    input := `"Hello" + " " + "World!"`

    eval_ctx := object.create_evaluator_ctx();
    defer object.destroy_evaluator_ctx(&eval_ctx);

    evaluated := test_eval_with_ctx(input, &eval_ctx)
    str, ok := evaluated.(^object.String)

    if !testing.expectf(t, ok, "Object is not of Type String! got %v",
        reflect.union_variant_typeid(evaluated)) { return }

    testing.expectf(t, str.value == "Hello World!",
        "String has wrong value. got=%s", str.value)
}

@(test)
// evaluator/evaluator_test.go
test_string_comparison :: proc(t: ^testing.T) {

    Tests :: struct {
        input: string,
        expected: bool,
    }

    tests := [?]Tests{
        {
            `"Hello, World!" == "Hello, World!"`,
            true
        },
        {
            `"foo" == "bar"`,
            false
        }
    }

    for test in tests {
        eval_ctx := object.create_evaluator_ctx();
        defer object.destroy_evaluator_ctx(&eval_ctx);

        evaluated := test_eval_with_ctx(test.input, &eval_ctx)
        result, ok := evaluated.(^object.Boolean)

        if !testing.expectf(t, ok, "Result is not of type Boolean! got %v",
            reflect.union_variant_typeid(evaluated)) { return }

        testing.expectf(t, result.value == test.expected,
            "Unexpected string comparison result!. Expected %b,got=%b",
            test.expected ,result.value)

    }
}
