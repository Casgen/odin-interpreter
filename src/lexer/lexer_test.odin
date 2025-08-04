package lexer

import "core:testing"
import "../token"
import "core:fmt"


@test
test_next_token :: proc(t: ^testing.T) {
	input := `let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y;
    };

    let result = add(five, ten);
    !-/*5;

    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;`
	tests := [?]token.Token{
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "five"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "ten"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "add"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Function, "fn"},
		{token.TokenType.Left_Paren, "("},
		{token.TokenType.Identifier, "x"},
		{token.TokenType.Comma, ","},
		{token.TokenType.Identifier, "y"},
		{token.TokenType.Right_Paren, ")"},
		{token.TokenType.Left_Brace, "{"},
		{token.TokenType.Identifier, "x"},
		{token.TokenType.Plus, "+"},
		{token.TokenType.Identifier, "y"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Right_Brace, "}"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Let, "let"},
		{token.TokenType.Identifier, "result"},
		{token.TokenType.Assign, "="},
		{token.TokenType.Identifier, "add"},
		{token.TokenType.Left_Paren, "("},
		{token.TokenType.Identifier, "five"},
		{token.TokenType.Comma, ","},
		{token.TokenType.Identifier, "ten"},
		{token.TokenType.Right_Paren, ")"},
		{token.TokenType.Semicolon, ";"},
		{token.TokenType.Bang, "!"},
		{token.TokenType.Minus, "-"},
		{token.TokenType.Slash, "/"},
		{token.TokenType.Asterisk, "*"},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Semicolon, ";"},

		{token.TokenType.Integer, "5"},
		{token.TokenType.Lt, "<"},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Gt, ">"},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Semicolon, ";"},

		{token.TokenType.If, "if"},
		{token.TokenType.Left_Paren, "("},
		{token.TokenType.Integer, "5"},
		{token.TokenType.Lt, "<"},
		{token.TokenType.Integer, "10"},
		{token.TokenType.Right_Paren, ")"},
		{token.TokenType.Left_Brace, "{"},

		{token.TokenType.Return, "return"},
		{token.TokenType.True, "true"},
		{token.TokenType.Semicolon, ";"},

		{token.TokenType.Right_Brace, "}"},
		{token.TokenType.Else, "else"},
		{token.TokenType.Left_Brace, "{"},

		{token.TokenType.Return, "return"},
		{token.TokenType.False, "false"},
		{token.TokenType.Semicolon, ";"},

		{token.TokenType.Right_Brace, "}"},

        {token.TokenType.Integer, "10"},
        {token.TokenType.Equal, "=="},
        {token.TokenType.Integer, "10"},
		{token.TokenType.Semicolon, ";"},

        {token.TokenType.Integer, "10"},
        {token.TokenType.Not_Equal, "!="},
        {token.TokenType.Integer, "9"},
		{token.TokenType.Semicolon, ";"},

		{token.TokenType.EOF, ""},
	}

	l := new_lexer(input)

	for test in tests {
		tok := next_token(l)

        testing.expect(t,test.type == tok.type, fmt.tprintf("Token types do not match! Expected %q, got %q", test.type, tok.type ))
        testing.expect(t,test.literal == tok.literal, fmt.tprintf("Token literals do not match! Expected %s, got %s", test.literal, tok.literal ))
	}

}
