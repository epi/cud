/**
Define tokens and operations on sequences of thereof.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.token;

import std.range : isInputRange, ElementType, empty, popFront, front;

///
struct Line
{
	uint num; ///
	string content; ///
	string file; ///
	immutable(size_t)[] mergedOffsets;
}

///
enum TokenKind : int
{
	reserved,

	eof,
	space,
	newline,

	lbracket,
	rbracket,
	lparen,
	rparen,
	lcurly,
	rcurly,
	dot,
	ptr,

	plusplus,
	minusminus,
	and,
	mul,
	plus,
	minus,
	tilde,
	not,

	div,
	mod,
	shl,
	shr,
	lt,
	gt,
	le,
	ge,
	equal,
	notequal,
	xor,
	or,
	andand,
	oror,

	question,
	colon,
	semicolon,
	ellipsis,

	assign,
	mulassign,
	divassign,
	modassign,
	plusassign,
	minusassign,
	shlassign,
	shrassign,
	andassign,
	xorassign,
	orassign,

	comma,
	hash,
	hashhash,

	headername,
	identifier,
	ppnumber,
	charconstant,
	stringliteral,
}

///
struct Location
{
	string file; ///
	uint line; ///
	uint column; ///
}

/// Represents a token or preprocessing token (pp-token)
struct Token
{
	TokenKind kind = TokenKind.reserved; ///
	Location location; ///
	string spelling; ///

	/// Allow testing for null token
	bool opCast(T)() if (is(T == bool))
	{
		return kind != TokenKind.reserved;
	}

	///
	unittest
	{
		static assert(!Token.init);
		static assert(!!Token(TokenKind.plus));
	}
}

/// Remove tokens from the front of input range `tr` up to the first non-whitespace token.
package void popSpaces(R)(ref R tr) pure nothrow
	if (isInputRange!R && is(ElementType!R : const(Token)))
{
	while (!tr.empty && tr.front.kind == TokenKind.space)
		tr.popFront;
}

package Token expect(R)(ref R tr, TokenKind[] token_kinds...) pure
{
	Token result;
	foreach (tk; token_kinds) {
		if (tk != TokenKind.space)
			tr.popSpaces;
		if (!((!tr.empty && tr.front.kind == tk) || (tr.empty && tk == TokenKind.eof))) {
			import std.conv : to;
			throw new Exception("expected: " ~ to!string(tk));
		}
		if (result.kind == TokenKind.reserved)
			result = tr.front;
		if (!tr.empty)
			tr.popFront;
	}
	return result;
}