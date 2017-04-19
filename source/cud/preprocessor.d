/**
CTFEable C language preprocessor.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/

module cud.preprocessor;

import std.range : isInputRange, ElementType, empty, popFront, front;

import cud.token : Token, TokenKind, match, expect, popSpaces;
import cud.lexer;

debug import std.stdio, std.algorithm : map, filter, equal;

version(unittest) {
	import std.algorithm : map, filter, equal;
	import std.range : array;
	import std.exception : assertThrown, assertNotThrown;
	import std.stdio;
	import cud.test;
}

/**
Defines a simple abstraction of a read-only file system that returns
contents of a file given its name.
*/
enum isFS(T) = is(typeof((T.init)[""]) : string);

///
unittest
{
	import cud.preprocessor : isFS;

	string[string] aa;
	struct FS {
		auto opIndex(string name) { return ""; }
	}

	static assert(!isFS!(string[]));
	static assert(isFS!(typeof(aa)));
	static assert(isFS!FS);
}

private void error(A...)(Token tok, const(char)[] format, auto ref A args)
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

struct Preprocessor(FS)
	if (isFS!FS)
{
	private {
		FS m_fs;
		const(Token)[] m_input;
		Token[] m_output;
	}

	this(FS fs, string file_name)
	{
		m_fs = fs;
		m_input = readFile(file_name);
		m_output ~= Token(TokenKind.space);
	}

	Token[] preprocess()
	{
		while (!m_input.empty) {
			while (m_input.match(TokenKind.hash))
				directive(m_input);
			while (!m_input.empty) {
				const tok = m_input.front;
				m_input.popFront;
				if (tok.kind == TokenKind.newline) {
					put(Token(TokenKind.space, tok.location, ""));
					break;
				} else {
					put(tok);
				}
			}
		}
		return m_output;
	}

	private void put(in Token tok) pure
	{
		if (tok.kind == TokenKind.space
		 && m_output[$ - 1].kind == TokenKind.space)
			return;
		m_output ~= tok;
	}

	private void includeFile(ref const(Token)[] input) pure
	{
		auto name = input.expect(TokenKind.headername, TokenKind.newline).spelling;
		assert(name.length >= 2);
		name = name[1 .. $ - 1];
		input = readFile(name) ~ input;
	}

	private const(Token)[] readFile(string file_name) pure
	{
		return m_fs[file_name]
			.split(file_name)
			.merge
			.tokenize
			.array;
	}

	private void directive(ref const(Token)[] input) pure
	{
		const tok = input.popSpaces;
		input.popFront;
		if (tok.kind == TokenKind.identifier) {
			switch (tok.spelling) {
				case "include":
					includeFile(input);
					break;
				default:
					error(tok, "invalid preprocessing directive '%s'", tok.spelling);
			}
		}
	}

	debug private string format(const(Token)[] tokens) pure
	{
		enum color_space      = "\x1b[38;2;255;255;255;48;2;48;48;48m";
		enum color_hash       = "\x1b[38;2;255;0;0;48;2;32;32;32m";
		enum color_hashhash   = "\x1b[38;2;255;128;0;48;2;32;32;32m";
		enum color_identifier = "\x1b[38;2;128;255;128;48;2;32;32;32m";
		enum color_literal    = "\x1b[38;2;255;255;0;48;2;32;32;32m";
		enum color_other      = "\x1b[38;2;192;192;192;48;2;32;32;32m";
		enum color_reset      = "\x1b[0m";
		import std.array : appender;
		if (tokens.length == 0)
			return "";
		auto app = appender!string();
		while (!tokens.empty) {
			const tok = tokens.front;
			tokens.popFront;
			if (tok.kind == TokenKind.newline) {
				app.put(color_space);
				app.put("↵");
				app.put(color_reset);
			} else if (tok.kind == TokenKind.space) {
				app.put(color_space);
				app.put(" ");
				app.put(color_reset);
			} else {
				switch (tok.kind) with (TokenKind) {
					case identifier:
						app.put(color_identifier);
						break;
					case hash:
						app.put(color_hash);
						break;
					case hashhash:
						app.put(color_hashhash);
						break;
					case charconstant:
					case stringliteral:
					case ppnumber:
						app.put(color_literal);
						break;
					default:
						app.put(color_other);
				}
				app.put(tok.spelling);
				app.put(color_reset);
			}
		}
		return app.data;
	}
}

/**
Preprocess a file: execute preprocessor directives,
convert pp-tokens into tokens.

Params:
 fs = source file system
 file_name = name of the file to preprocess

Returns:
 Array of tokens after preprocessing

See_Also:
 `cud.preprocessor.isFS`, `cud.token.Token`

Standards:
According to ISO C standard draft:

$(I 4. Preprocessing directives are executed, macro invocations are expanded, and
`_Pragma` unary operator expressions are executed. If a character sequence that
matches the syntax of a universal character name is produced by token
concatenation (6.10.3.3), the behavior is undefined. A `#include` preprocessing
directive causes the named header or source file to be processed from phase 1
through phase 4, recursively. All preprocessing directives are then deleted.)
*/
Token[] preprocess(FS)(FS fs, string file_name) pure // @safe
	if (isFS!FS)
{
	return Preprocessor!FS(fs, file_name).preprocess;
}

version(unittest) {
	auto preprocess(string source)
	{
		return preprocess([ "test.h" : source ], "test.h");
	}

	Preprocessor!FS testPP(FS)(FS fs, string file_name, string expected = "")
	{
		auto pp = Preprocessor!FS(fs, file_name);
		auto result = pp.preprocess()
			.array;
		auto expected_tokens = expected.split.merge.tokenize
			.array;
		if (!__ctfe) {
			debug writeln(" EXP: ", pp.format(expected_tokens));
			debug writeln(" ACT: ", pp.format(result));
		}
		assertEqual(
			result.filter!(t => t.kind != TokenKind.space && t.kind != TokenKind.newline),
			expected_tokens
				.filter!(t => t.kind != TokenKind.space && t.kind != TokenKind.newline));
		return pp;
	}

	Preprocessor!(string[string]) testPP(string source, string expected = "")
	{
		return testPP(
			[ "test.h" : source ],
			"test.h",
			expected);
	}
}

unittest
{
	crtest!("invalid preprocessor directive",
		() {
			assertThrown(testPP("#invalid"));
		});
}

unittest
{
	crtest!("nested #include works",
		() {
			enum fs = [
				"foo.h": " foo1 \n#include \"bar.h\"\n foo2",
				"bar.h": " bar1 \n#include \"baz.h\"\n bar2",
				"baz.h": "baz1\n #include \"quux.h\"\n baz2",
				"quux.h": "quux"
			];
			testPP(fs, "foo.h", "foo1 bar1 baz1 quux baz2 bar2 foo2");
		});
}
