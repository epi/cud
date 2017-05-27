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

enum TokenKind : int {
	eof,
}

template Tokens()
{
	import std.string : split;
	import std.algorithm : countUntil;

	enum punctuators =
		(`[ ] ( ) { } >>= >> >= > <<= << <= < ` ~
		`&& &= & || |= | ^= ^ *= * ++ += + -- -= -> - == = != ! /= / ` ~
		`%= % : ... . ~ ## # ; , ?`).split;

	enum alternative = `<: :> <% %> %: %:%:`.split;
	enum equivalents = `[  ]  {  }  #  ##`.split;

	enum keywords =
		(`auto break case char const continue default do double else enum ` ~
		`extern float for goto if inline int long register restrict return ` ~
		`short signed sizeof static struct switch typedef union unsigned ` ~
		`void volatile while _Alignas _Alignof _Atomic _Bool _Complex ` ~
		`_Generic _Imaginary _Noreturn _Static_assert _Thread_local`).split;

	enum dynamicTokens =
		(`space newline intconst uintconst longconst ` ~
		`ulongconst headername identifier ppnumber charconst ` ~
		`stringliteral notreplacedidentifier placemarker ` ~
		`post++ post-- macroparam`).split;

	enum allTokens = [`eof`, ``] ~ punctuators ~ keywords ~ dynamicTokens;

	template tk(string s)
	{
		enum i = allTokens.countUntil(s);
		static if (i >= 0)
			enum tk = cast(TokenKind) i;
		else {
			enum j = alternative.countUntil(s);
			static if (j >= 0)
				enum tk = .Tokens!().tk!(equivalents[j]);
			else
				static assert(0, "Invalid token kind: " ~ s);
		}
	}
}

alias allTokens = Tokens!().allTokens;
alias tk = Tokens!().tk;

template tks(string s)
{
	import std.string : split;
	import std.meta : aliasSeqOf, staticMap;
	alias tks = staticMap!(tk, aliasSeqOf!(s.split));
}

unittest
{
	static assert(tk!`<%` == tk!`{`);
	static assert(tk!`:>` == tk!`]`);
	static assert(tk!`%:%:` == tk!`##`);
	static assert(tks!`<% :>` == tks!`{ ]`);
}

///
struct Location
{
	string file; ///
	uint line; ///
	uint column; ///

	void toString(scope void delegate(const(char)[]) dg)
	{
		import std.format : formattedWrite;
		dg.formattedWrite("%s:%d,%d", file, line + 1, column + 1);
	}
}

/// Represents a token or preprocessing token (pp-token)
struct Token
{
	TokenKind kind = tk!``;
	Location location; ///
	string spelling; ///
	union {
		long signedInt64Value;
		ulong unsignedInt64Value;
	}

	static Token makeConstant(T)(T value, Location loc, string spelling)
	{
		import std.meta : AliasSeq, staticIndexOf;
		import std.traits : isSigned, isUnsigned;
		alias Types = AliasSeq!(int, uint, long, ulong);
		enum kinds = [tk!`intconst`, tk!`uintconst`, tk!`longconst`, tk!`ulongconst`];
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

	void toString(scope void delegate(const(char)[]) dg) const
	{
		import std.format : formattedWrite;
		if (!spelling.length)
			dg.formattedWrite("[%s]", kind);
		dg(spelling);
	}

	/// Allow testing for null token
	bool opCast(T)() const pure nothrow @safe
		if (is(T == bool))
	{
		return kind != tk!``;
	}

	///
	unittest
	{
		crtest!("opCast!bool is true for Token.init",
			() {
				assert(!Token.init);
				assert(!!Token(tk!`+`));
			});
	}

	Token toMacroParam(int index) const pure @safe
	{
		import std.exception : enforce;
		enforce(index >= 0 && index < maxMacroParams);
		return Token(
			cast(TokenKind) (tk!`macroparam` + index),
			this.location, this.spelling);
	}

	@property bool isMacroParam() const pure nothrow @safe
	{
		return kind >= tk!`macroparam` && kind < (tk!`macroparam` + maxMacroParams);
	}

	@property int macroParamIndex() const pure nothrow @safe
	{
		assert(this.isMacroParam);
		return kind - tk!`macroparam`;
	}

	bool opEquals(Token rhs) const pure @safe nothrow
	{
		return this.kind == rhs.kind && this.spelling == rhs.spelling;
	}
}

///
void error(A...)(Token tok, const(char)[] format, auto ref A args)
{
	error(tok.location, format, args);
}

/// ditto
void error(A...)(Location loc, const(char)[] format, auto ref A args)
{
	import std.format : formattedWrite;
	import std.range : appender;
	auto app = appender!string;
	app.formattedWrite(
		"%s(%d,%d): Error: ",
		loc.file,
		loc.line + 1,
		loc.column + 1);
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
		if (tok.kind != tk!`space`)
			return tok;
		tr.popFront;
	}
	return Token.init;
}

/**
Returns: the first token of matched sequence, or a null array if
the `input` sequence does not match the expected `token_kinds`.
*/
package Token match(bool expect = false)(
	ref const(Token)[] input, TokenKind[] token_kinds...) pure @safe
{
	const(Token)[] tr = input;
	Token result;
	foreach (i, kind; token_kinds) {
		assert(kind != tk!`eof`);
		assert(kind != tk!`space`);
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
	import std.algorithm : map;
	import std.range : array;

	crtest!("match() returns first token of matched sequence",
		() {
			auto toks = [ tk!`space`, tk!`space`, tk!`+`, tk!`space`,
				tk!`identifier` ].map!(k => const(Token)(k)).array;
			assert(toks.match(tk!`+`, tk!`identifier`).kind == tk!`+`);
			assert(toks.empty);
		});

	crtest!("match() does not accept partial matches: returns null token and does not advance input",
		() {
			auto toks = [ tk!`space`, tk!`space`, tk!`+`, tk!`space`,
				tk!`identifier`, tk!`newline` ]
				.map!(k => const(Token)(k)).array;
			assert(!toks.match(tk!`+`, tk!`+`));
			assertEqual(
				toks.map!(t => t.kind),
				[
					tk!`space`, tk!`space`, tk!`+`, tk!`space`,
					tk!`identifier`, tk!`newline`
				]);
		});
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
	import std.meta : aliasSeqOf;
	switch (tok.kind) {
	case tk!`ppnumber`:
		return parseNumber(tok);
	case tk!`identifier`:
		foreach (kw; aliasSeqOf!(Tokens!().keywords)) {
			if (tok.spelling == kw)
				return Token(tk!kw, tok.location, tok.spelling);
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
		return Token(tk!`ppnumber`, Location.init, spelling).ppTokenToToken;
	}

	static auto testppnum(T)(string spelling, TokenKind kind, T value)
	{
		auto token = ppnum(spelling);
		assert(token.kind == kind);
		if (kind == tk!`intconst` || kind == tk!`longconst`)
			assert(token.signedInt64Value == value);
		else if (kind == tk!`uintconst` || kind == tk!`ulongconst`)
			assert(token.unsignedInt64Value == value);
	}

	crtest!("convert pp tokens to integer constants",
		() {
			testppnum("0", tk!`intconst`, 0);
			testppnum("2", tk!`intconst`, 2);
			testppnum("00017", tk!`intconst`, octal!17);
			testppnum("0l", tk!`longconst`, 0);
			testppnum("0ll", tk!`longconst`, 0);
			testppnum("0x123", tk!`intconst`, 0x123);
			testppnum("0x89aBcDef", tk!`uintconst`, 0x89abcdef);
			testppnum("0x123u", tk!`uintconst`, 0x123);
			testppnum("123", tk!`intconst`, 123);
			testppnum("123u", tk!`uintconst`, 123);
			testppnum("2147483653", tk!`longconst`, 0x80000005);
			testppnum("2147483653U", tk!`uintconst`, 0x80000005);
			testppnum("2147483653uL", tk!`ulongconst`, 0x80000005);
			testppnum("4294967301", tk!`longconst`, 0x100000005);
			testppnum("4294967301u", tk!`ulongconst`, 0x100000005);
			testppnum("0xffffffffffffffff", tk!`ulongconst`, 0xffffffffffffffff);
			testppnum("0xffffffffffffffffll", tk!`ulongconst`, 0xffffffffffffffff);
			testppnum("0xffffffffffffffffU", tk!`ulongconst`, 0xffffffffffffffff);
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
	assertEqual(
		[
			Token(tk!`identifier`, Location.init, "auto"),
			Token(tk!`identifier`, Location.init, "if"),
			Token(tk!`identifier`, Location.init, "static"),
			Token(tk!`identifier`, Location.init, "_Thread_local"),
		].map!(t => t.ppTokenToToken.kind),
		[ tks!`auto if static _Thread_local` ]);
}
