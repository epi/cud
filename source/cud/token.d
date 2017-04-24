/**
Define tokens and operations on sequences of thereof.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.token;

import std.range : isInputRange, ElementType, empty, popFront, front;
import std.ascii;

version(unittest) {
	import std.algorithm : map;
	import std.exception : assertThrown;
	import cud.test;
}

///
struct Line
{
	uint num; ///
	string content; ///
	string file; ///
	immutable(size_t)[] mergedOffsets;
}

private enum maxMacroParams = 65536;

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

	intconstant,
	uintconstant,
	longconstant,
	ulongconstant,

	auto_,
	break_,
	case_,
	char_,
	const_,
	continue_,
	default_,
	do_,
	double_,
	else_,
	enum_,
	extern_,
	float_,
	for_,
	goto_,
	if_,
	inline_,
	int_,
	long_,
	register_,
	restrict_,
	return_,
	short_,
	signed_,
	sizeof_,
	static_,
	struct_,
	switch_,
	typedef_,
	union_,
	unsigned_,
	void_,
	volatile_,
	while_,
	_Alignas_,
	_Alignof_,
	_Atomic_,
	_Bool_,
	_Complex_,
	_Generic_,
	_Imaginary_,
	_Noreturn_,
	_Static_assert_,
	_Thread_local_,

	hash,
	hashhash,

	headername,
	identifier,
	ppnumber,
	charconstant,
	stringliteral,

	notreplacedidentifier,
	placemarker,
	macroparam,
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
	union {
		long signedInt64Value;
		ulong unsignedInt64Value;
	}

	static Token makeConstant(T)(T value, Location loc, string spelling)
	{
		with(TokenKind) {
			import std.meta : AliasSeq, staticIndexOf;
			import std.traits : isSigned, isUnsigned;
			alias Types = AliasSeq!(int, uint, long, ulong);
			enum kinds = [intconstant, uintconstant, longconstant, ulongconstant];
			enum kind = kinds[staticIndexOf!(T, Types)];
			auto result = Token(kind, loc, spelling);
			static if (isSigned!T)
				result.signedInt64Value = value;
			else static if (isUnsigned!T)
				result.unsignedInt64Value = value;
			else
				static assert(0);
			return result;
		}
	}

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

	Token toMacroParam(int index) const pure @safe
	{
		import std.exception : enforce;
		enforce(index >= 0 && index < maxMacroParams);
		return Token(
			cast(TokenKind) (TokenKind.macroparam + index),
			this.location, this.spelling);
	}

	@property bool isMacroParam() const pure nothrow @safe
	{
		return kind >= TokenKind.macroparam && kind < (TokenKind.macroparam + maxMacroParams);
	}

	@property int macroParamIndex() const pure nothrow @safe
	{
		assert(this.isMacroParam);
		return kind - TokenKind.macroparam;
	}

	bool opEquals(Token rhs) const pure @safe nothrow
	{
		return this.kind == rhs.kind && this.spelling == rhs.spelling;
	}
}

///
void error(A...)(Token tok, const(char)[] format, auto ref A args)
{
	import std.format : formattedWrite;
	import std.range : appender;
	auto app = appender!string;
	app.formattedWrite(
		"%s(%d,%d): Error: ",
		tok.location.file,
		tok.location.line + 1,
		tok.location.column + 1);
	app.formattedWrite(format, args);
	throw new Exception(app.data);
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
			static if (expect)
				error(tok, "found '%s' when expecting %s", tok.spelling, kind);
			else
				return Token.init;
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

Token parseNumber(Token tok) pure @safe
{
	auto spelling = tok.spelling;
	ulong value = spelling.front - '0';
	spelling.popFront;
	if (spelling.empty)
		return Token.makeConstant!int(cast(int) value, tok.location, tok.spelling);
	const base = () {
		if (!value) {
			if (spelling.length >= 2
				&& (spelling[0] == 'x' || spelling[0] == 'X')
				&& spelling[1].isHexDigit)
			{
				spelling = spelling[1 .. $];
				return 16;
			}
			return 8;
		}
		return 10;
	}();
	while (!spelling.empty && spelling.front.isHexDigit) {
		uint digit = () {
			const c = spelling.front;
			if (c >= '0' && c <= '9')
				return c - '0';
			if (c >= 'A' && c <= 'F')
				return c - 'A' + 10;
			if (c >= 'a' && c <= 'f')
				return c - 'a' + 10;
			return uint.max;
		}();
		if (digit >= base)
			break;
		const oldvalue = value;
		value = value * base + digit;
		if (oldvalue && value <= oldvalue)
			error(tok,
				"integer literal is too large to be represented in any integer type");
		spelling.popFront;
	}

	switch (spelling) {
		case "l": case "L":
		case "ll": case "LL":
			if (value <= long.max)
				return Token.makeConstant!long(value, tok.location, tok.spelling);
			// TODO: warn if base == 10
			goto case;
		case "ul": case "uL": case "Ul": case "UL":
		case "lu": case "Lu": case "lU": case "LU":
		case "ull": case "uLL": case "Ull": case "ULL":
		case "llu": case "LLu": case "llU": case "LLU":
			return Token.makeConstant!ulong(value, tok.location, tok.spelling);
		case "":
			if (value <= int.max)
				return Token.makeConstant!int(cast(int) value, tok.location, tok.spelling);
			if ((base == 8 || base == 16) && value <= uint.max)
				return Token.makeConstant!uint(cast(uint) value, tok.location, tok.spelling);
			if (value <= long.max)
				return Token.makeConstant!long(value, tok.location, tok.spelling);
			// TODO: warn if base == 10
			return Token.makeConstant!ulong(value, tok.location, tok.spelling);
		case "u": case "U":
			if (value <= uint.max)
				return Token.makeConstant!uint(cast(uint) value, tok.location, tok.spelling);
			return Token.makeConstant!ulong(value, tok.location, tok.spelling);
		default:
			error(tok, "invalid suffix '%s' on integer constant", spelling);
			assert(0);
	}
}

Token ppTokenToToken(Token tok) pure @safe
{
	switch (tok.kind) with(TokenKind) {
	case ppnumber:
		return parseNumber(tok);
	case identifier:
		foreach (tk; __traits(allMembers, TokenKind)) {
			static if (tk[$ - 1] == '_') {
				if (tok.spelling == tk[0 .. $ - 1])
					return Token(mixin(tk), tok.location, tok.spelling);
			}
		}
		goto default;
	default:
		return tok;
	}
}

unittest
{
	import std.conv : octal;

	static auto ppnum(string spelling)
	{
		return Token(TokenKind.ppnumber, Location.init, spelling).ppTokenToToken;
	}

	static auto testppnum(T)(string spelling, TokenKind kind, T value)
	{
		auto token = ppnum(spelling);
		assert(token.kind == kind);
		if (kind == TokenKind.intconstant || kind == TokenKind.longconstant)
			assert(token.signedInt64Value == value);
		else if (kind == TokenKind.uintconstant || kind == TokenKind.ulongconstant)
			assert(token.unsignedInt64Value == value);
	}

	crtest!("convert pp tokens to integer constants",
		() {
			with(TokenKind) {
				testppnum("0", intconstant, 0);
				testppnum("2", intconstant, 2);
				testppnum("00017", intconstant, octal!17);
				testppnum("0l", longconstant, 0);
				testppnum("0ll", longconstant, 0);
				testppnum("0x123", intconstant, 0x123);
				testppnum("0x89aBcDef", uintconstant, 0x89abcdef);
				testppnum("0x123u", uintconstant, 0x123);
				testppnum("123", intconstant, 123);
				testppnum("123u", uintconstant, 123);
				testppnum("2147483653", longconstant, 0x80000005);
				testppnum("2147483653U", uintconstant, 0x80000005);
				testppnum("2147483653uL", ulongconstant, 0x80000005);
				testppnum("4294967301", longconstant, 0x100000005);
				testppnum("4294967301u", ulongconstant, 0x100000005);
				testppnum("0xffffffffffffffff", ulongconstant, 0xffffffffffffffff);
				testppnum("0xffffffffffffffffll", ulongconstant, 0xffffffffffffffff);
				testppnum("0xffffffffffffffffU", ulongconstant, 0xffffffffffffffff);
			}
		});

	crtest!("error on too large integer constant",
		() {
			assertThrown(ppnum("18446744073709551616"));
			assertThrown(ppnum("0x10000000000000000"));
		});

	crtest!("error on invalid suffix on integer constant",
		() {
			assertThrown(ppnum("0x2ffgl"));
		});
}

unittest
{
	with (TokenKind) {
		[
			Token(identifier, Location.init, "auto"),
			Token(identifier, Location.init, "if"),
			Token(identifier, Location.init, "static"),
			Token(identifier, Location.init, "_Thread_local"),
		]
			.map!(t => t.ppTokenToToken.kind)
			.assertEqual([auto_, if_, static_, _Thread_local_]);
	}
}
