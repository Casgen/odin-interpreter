package lexer

import "../token"
import "core:fmt"
import "core:mem"
import "core:testing"
import "core:strings"


Lexer :: struct {
    is_input_cloned:  bool,
	input:            string,
    position:         u32,
    read_position:    u32,
	char:             u8,
}

PROMPT :: ">> "

new_lexer :: proc(input: string, clone_input: bool = true) -> ^Lexer {
	lex := new(Lexer)
	lex.input = strings.clone(input) if clone_input else input
    lex.is_input_cloned = clone_input
	lex.read_position = 0

	read_char(lex)

	return lex
}

destroy_lexer :: proc(lex: ^Lexer) {
    if lex.is_input_cloned do delete(lex.input)
    free(lex)
}

init_lexer :: proc() -> ^Lexer {
	lex := new(Lexer)

	lex.input = ""
	lex.char = 0
	lex.position = 0
	lex.read_position = 1

	return lex
}

set_new_input :: proc(using lex: ^Lexer, new_input: string) {
	input = new_input
	char = input[0]
	position = 0
	read_position = 1
}

read_char :: proc(using lex: ^Lexer) {
	if read_position >= u32(len(input)) {
		char = 0
	} else {
		char = input[read_position]
	}

	position = read_position
	read_position += 1
}

next_token :: proc(using lex: ^Lexer) -> token.Token {

	tok := token.Token{token.TokenType.Illegal, token.token_strings[.Illegal]}

	skip_whitespace(lex)

	switch char {
	case '=':
		tok = make_two_char_token(lex, token.TokenType.Assign)
	case '+':
		tok = {token.TokenType.Plus, token.token_strings[.Plus]}
	case '-':
		tok = {token.TokenType.Minus, token.token_strings[.Minus]}
	case '/':
		tok = {token.TokenType.Slash, token.token_strings[.Slash]}
	case '*':
		tok = {token.TokenType.Asterisk, token.token_strings[.Asterisk]}
	case '<':
		tok = {token.TokenType.Lt, token.token_strings[.Lt]}
	case '>':
		tok = {token.TokenType.Gt, token.token_strings[.Gt]}
	case ';':
		tok = {token.TokenType.Semicolon, token.token_strings[.Semicolon]}
	case ',':
		tok = {token.TokenType.Comma, token.token_strings[.Comma]}
	case '(':
		tok = {token.TokenType.Left_Paren, token.token_strings[.Left_Paren]}
	case ')':
		tok = {token.TokenType.Right_Paren, token.token_strings[.Right_Paren]}
	case '{':
		tok = {token.TokenType.Left_Brace, token.token_strings[.Left_Brace]}
	case '}':
		tok = {token.TokenType.Right_Brace, token.token_strings[.Right_Brace]}
	case '!':
		tok = make_two_char_token(lex, token.TokenType.Bang)
	case 0:
		tok = {token.TokenType.EOF, token.token_strings[.EOF]}
	case:
		if is_letter(char) {
			tok.literal = read_identifier(lex)
			tok.type = token.lookup_ident(tok.literal)
			return tok
		} else if is_digit(char) {
			tok.literal = read_number(lex)
			tok.type = token.TokenType.Integer
			return tok
		}
	}

	read_char(lex)
	return tok
}

make_two_char_token :: proc(using lex: ^Lexer, no_match_type: token.TokenType) -> token.Token {

	tokenType: token.TokenType
	peek := peek_char(lex)

	switch {
	case char == '!' && peek == '=':
		defer read_char(lex)
		return {token.TokenType.Not_Equal, "!="}
	case char == '=' && peek == '=':
		defer read_char(lex)
		return {token.TokenType.Equal, "=="}
	}

	return token.Token{no_match_type, fmt.tprintf("%c", char)}
}

is_letter :: proc(char: u8) -> bool {
	// This is a hack where we abuse the ascii tables organization of characters into columns
	// The uppercase characters begin at 65 and lowercase at 97.
	// If we abuse the fact that their offsetted by 32, we can use some bitmasking to mask
	// off the 4. bit (32) from the 97 and that bumps it down to 65. Therefore we should be
	// able to only do two comparisons. Not four. 0b00110000 & 11011111
	mask: u8 = char & (223)
	return (mask > 64 && mask < 91) || char == 95
}

is_digit :: proc(char: u8) -> bool {
	// 48 = '0'
	// 57 = '9'
	return (char >= 48) & (char <= 57)
}

read_number :: proc(using lex: ^Lexer) -> string {

	first_position := position

	for is_digit(char) {
		read_char(lex)
	}

	return input[first_position:position]
}

peek_char :: proc(using lex: ^Lexer) -> u8 {
	if read_position >= u32(len(input)) {
		return 0
	}

	return input[read_position]
}

read_identifier :: proc(using lex: ^Lexer) -> string {
	first_position := position

	for is_letter(char) {
		read_char(lex)
	}

    // Cloning the literal to make it independent from the input
    ident := strings.clone(input[first_position:position])
	return ident
}

skip_whitespace :: proc(using lex: ^Lexer) {
	for (char == ' ' || char == '\n' || char == '\t' || char == '\r') {
		read_char(lex)
	}
}
