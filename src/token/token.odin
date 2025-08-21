#+feature dynamic-literals
package token

Token :: struct {
	type:    TokenType,
	literal: string,
}

TokenType :: enum {
	Illegal,
	EOF,

	// Identifier + Literals
	Identifier,
	Integer,

	// Operators
	Assign,
	Plus,
	Minus,
	Bang,
	Asterisk,
	Slash,
	Equal,
	Not_Equal,
	Comma,
	Semicolon,
	Left_Paren,
	Right_Paren,
	Left_Brace,
	Right_Brace,

	// Keywords
	Let,
	True,
	False,
	If,
	Else,
	Return,
	Function,
	Gt,
	Lt,
}



keywords := map[string]TokenType {
    "fn" = .Function,
    "let" = .Let,
    "return" = .Return,
    "true" = .True,
    "false" = .False,
    "if" = .If,
    "else" = .Else,
}

lookup_ident :: proc(ident: string) -> TokenType {
    if type, ok := keywords[ident]; ok {
        return type
    }

    return .Identifier
}

Token_Strings :: [TokenType]string {
	.Illegal     = "ILLEGAL",
	.EOF         = "",
	.Identifier  = "IDENT",
	.Integer     = "INTEGER",
	.Assign      = "=",
	.Plus        = "+",
	.Minus       = "-",
	.Bang        = "!",
	.Asterisk    = "*",
	.Slash       = "/",
	.Equal       = "==",
	.Not_Equal   = "!=",
	.Comma       = ",",
	.Semicolon   = ";",
	.Left_Paren  = "(",
	.Right_Paren = ")",
	.Left_Brace  = "{",
	.Right_Brace = "}",
	.Let         = "let",
	.True        = "true",
	.False       = "false",
	.If          = "if",
	.Else        = "else",
	.Return      = "return",
	.Function    = "fn",
	.Gt          = ">",
	.Lt          = "<",
}

token_strings := Token_Strings
