/**
Define tokens and operations on sequences of thereof.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.token;

import std.range : isInputRange, ElementType, empty, popFront, front;

version(unittest) import cud.test;

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
	bool opCast(T)() const pure nothrow @safe
		if (is(T == bool))
	{
		return kind != TokenKind.reserved;
	}

	///
	unittest
	{
		crtest!("opCast!bool is true for Token.init",
			() {
				assert(!Token.init);
				assert(!!Token(TokenKind.plus));
			});
	}
}

/// Remove tokens from the front of input range `tr` up to the first non-whitespace token.
/// Return: the first non-whitespace token or `Token.init`, if not found.
package Token popSpaces(R)(ref R tr) pure nothrow
	if (isInputRange!R && is(ElementType!R : const(Token)))
{
	while (!tr.empty) {
		immutable tok = tr.front;
		if (tok.kind != TokenKind.space)
			return tok;
		tr.popFront;
	}
	return Token.init;
}


/**
Returns: the first token of matched sequence, or a null array if
the `input` sequence does not match the expected `token_kinds`.
*/
package Token match(bool expect = false)(ref const(Token)[] input, TokenKind[] token_kinds...) pure @safe
{
	const(Token)[] tr = input;
	Token result;
	foreach (i, kind; token_kinds) {
		assert(kind != TokenKind.eof);
		assert(kind != TokenKind.space);
		const tok = tr.popSpaces;
		if (tok.kind != kind) {
			static if (expect) {
				import std.string : format;
				throw new Exception(format("%s(%d,%d): found '%s' when expecting %s",
					tok.location.file, tok.location.line + 1, tok.location.column + 1,
					tok.spelling, kind));
			} else {
				return Token.init;
			}
		}
		if (!result)
			result = tr.front;
		tr.popFront;
	}
	input = tr;
	return result;
}

package Token expect(ref const(Token)[] input, TokenKind[] token_kinds...) pure @safe
{
	return match!true(input, token_kinds);
}

unittest
{
	import cud.test;
	import std.algorithm : map, equal;
	import std.range : array;

	with(TokenKind) {
		crtest!("match() returns first token of matched sequence",
			() {
				auto toks = [ space, space, plus, space, identifier ]
					.map!(k => const(Token)(k)).array;
				assert(toks.match(plus, identifier).kind == plus);
				assert(toks.empty);
			});

		crtest!("match() does not accept partial matches: returns null token and does not advance input",
			() {
				auto toks = [ space, space, plus, space, identifier, newline ]
					.map!(k => const(Token)(k)).array;
				assert(!toks.match(plus, plus));
				assert(toks.map!(t => t.kind).equal([space, space, plus, space, identifier, newline]));
			});

	}
}
