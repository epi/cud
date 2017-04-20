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

interface MacroVisitor
{
	void visit(const FunctionLikeMacro m) pure;
	void visit(const ObjectLikeMacro m) pure;
	void visit(const BuiltinMacro m) pure;
}

class NoOpMacroVisitor : MacroVisitor
{
	void visit(const FunctionLikeMacro m) pure {}
	void visit(const ObjectLikeMacro m) pure {}
	void visit(const BuiltinMacro m) pure {}
}

abstract class Macro
{
	string name;

	abstract void accept(MacroVisitor) const pure;
}

abstract class BuiltinMacro : Macro
{
	override void accept(MacroVisitor mv) const pure { mv.visit(this); }
}

abstract class UserDefinedMacro : Macro
{
	const(Token)[] replacement;

	this(string name, const(Token)[] replacement) pure @safe
	{
		this.name = name;
		this.replacement = replacement;
	}
}

final class FunctionLikeMacro : UserDefinedMacro
{
	const uint paramCount;

	this(string name, const(Token)[] replacement, uint paramCount) pure @safe
	{
		super(name, replacement);
		this.paramCount = paramCount;
	}

	override void accept(MacroVisitor mv) const { mv.visit(this); }
}

final class ObjectLikeMacro : UserDefinedMacro
{
	this(string name, const(Token)[] replacement) pure @safe
	{
		super(name, replacement);
	}

	override void accept(MacroVisitor mv) const { mv.visit(this); }
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
		Macro[string] m_macros;
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

	private uint[string] parseMacroParams(ref const(Token)[] input) pure @safe
	{
		if (input.empty || input.front.kind != TokenKind.lparen)
			return null;

		input.popFront;
		uint[string] params;
		uint index;
		params[""] = -1;
		params.remove("");
		assert(params !is null);

		if (input.popSpaces.kind == TokenKind.rparen) {
			input.popFront;
			return params;
		}

		for (;;) {
			if (immutable tok = input.match(TokenKind.identifier)) {
				if (tok.spelling in params)
					error(tok, "duplicate macro parameter '%s'",
						tok.spelling);
				params[tok.spelling] = index++;
				if (input.match(TokenKind.rparen))
					break;
				input.expect(TokenKind.comma);
			} else {
				input.expect(TokenKind.ellipsis, TokenKind.rparen);
				params["__VA_ARGS__"] = index;
				break;
			}
		}
		return params;
	}

	private Token[] parseMacroReplacement(
		ref const(Token)[] input,
		in uint[string] params) pure @safe
	{
		immutable is_function_like = params !is null;
		Token[] replacement;
		while (!input.empty) {
			Token rep_token = input.front;
			input.popFront;
			if (rep_token.kind == TokenKind.newline)
				break;
			if (rep_token.kind == TokenKind.space) {
				if (!(replacement.length
					&& (replacement[$ - 1].kind == TokenKind.hash
					 || replacement[$ - 1].kind == TokenKind.hashhash)))
				{
					replacement ~= Token(TokenKind.space, rep_token.location, " ");
				}
				rep_token = input.popSpaces;
				input.popFront;
			}
			switch (rep_token.kind) {
				case TokenKind.identifier:
					if (auto pident = rep_token.spelling in params) {
						replacement ~= rep_token.toMacroParam(*pident);
						break;
					} else
						goto default;
				case TokenKind.hashhash:
					while (replacement.length && replacement[$ - 1].kind == TokenKind.space)
						replacement = replacement[0 .. $ - 1];
					if (replacement.length == 0)
						error(rep_token, "'##' cannot appear at start of a macro replacement list");
					goto default;
				default:
					if (is_function_like
						&& replacement.length
						&& replacement[$ - 1].kind == TokenKind.hash)
					{
						error(replacement[$ - 1], "'#' is not followed by a macro parameter");
					}
					replacement ~= rep_token;
			}
		}
		if (replacement.length) {
			const last = replacement[$ - 1];
			if (is_function_like && last.kind == TokenKind.hash)
				error(last, "'#' is not followed by a macro parameter");
			if (last.kind == TokenKind.hashhash)
				error(last, "'##' cannot appear at end of a macro replacement list");
		}
		return replacement;
	}

	private void defineMacro(ref const(Token)[] input) pure
	{
		immutable name_tok = input.expect(TokenKind.identifier);
		immutable name = name_tok.spelling;
		const params = parseMacroParams(input);
		immutable is_function_like = params !is null;

		// silently ignoring 6.10.3, constraint 3: required space after object-like macro name
		// TODO: 6.10.3, constraint 2: macro redefinition
		if (const pm = name in m_macros) {
			(*pm).accept(new class NoOpMacroVisitor {
					override void visit(const BuiltinMacro bm) const {
						error(name_tok, "cannot redefine macro '%s'", name);
					}
				});
		}
		input.popSpaces();
		const replacement = parseMacroReplacement(input, params);
		m_macros[name] = is_function_like
			? new FunctionLikeMacro(name, replacement, cast(uint) params.length)
			: new ObjectLikeMacro(name, replacement);
	}

	private void undefMacro(ref const(Token)[] input) pure
	{
		auto name = input.expect(TokenKind.identifier, TokenKind.newline).spelling;
		m_macros.remove(name);
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
				case "define":
					defineMacro(input);
					break;
				case "undef":
					undefMacro(input);
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

	version(unittest) private @property macros() const pure nothrow @safe { return m_macros; }
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
		auto result = pp.preprocess().array;
		scope(failure) if (!__ctfe) { debug writeln(" ACT: ", pp.format(result)); }
		auto expected_tokens = expected.split.merge.tokenize.array;
		scope(failure) if (!__ctfe) { debug writeln(" EXP: ", pp.format(expected_tokens)); }
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

unittest // defineMacro
{
	crtest!("PP rejects #define without macro name",
		() {
			assertThrown(testPP("#define"));
		});

	crtest!("PP parses and stores object-like macro definitions",
		() {
			if (auto mac = testPP("#define FOO").macros["FOO"].as!ObjectLikeMacro)
				assert(mac.replacement.length == 0);

			if (auto mac = testPP("#define FOO 42 + bar").macros["FOO"].as!ObjectLikeMacro)
				assertEqual(
					mac.replacement.map!(t => t.spelling),
					["42", " ", "+", " ", "bar"]);

			if (auto mac = testPP("#define FOO __VA_ARGS__").macros["FOO"].as!ObjectLikeMacro)
				assertEqual(mac.replacement.map!(t => t.spelling), ["__VA_ARGS__"]);
		});

	crtest!("PP rejects unbalanced parens in macro definition",
		() {
			assertThrown(testPP("#define FOO(  "));
		});

	crtest!("PP parses and stores function-like macro definitions",
		() {
			if (auto mac = testPP("#define FOO(  )").macros["FOO"].as!FunctionLikeMacro) {
				assert(mac.paramCount == 0);
				assert(mac.replacement.length == 0);
			}

			if (auto mac = testPP("#define FOO(x)").macros["FOO"].as!FunctionLikeMacro) {
				assert(mac.paramCount == 1);
				assert(mac.replacement.length == 0);
			}

			if (auto mac = testPP("#define FOO(x, y)\n").macros["FOO"].as!FunctionLikeMacro) {
				assert(mac.paramCount == 2);
				assert(mac.replacement.length == 0);
			}

			if (auto mac = testPP("#define FOO(...)\n").macros["FOO"].as!FunctionLikeMacro) {
				assert(mac.paramCount == 1);
				assert(mac.replacement.length == 0);
			}

			if (auto mac = testPP("#define FOO(x, y, ...) (x - y) / baz(__VA_ARGS__)\n")
				.macros["FOO"].as!FunctionLikeMacro)
			{
				assert(mac.paramCount == 3);
				assertEqual(
					mac.replacement.map!(t => t.spelling),
					[ "(", "x", " ", "-", " ", "y", ")", " ", "/", " ",
						"baz", "(", "__VA_ARGS__", ")" ]);
			}

			if (auto mac = testPP("#define FOO(x, y) __VA_ARGS__")
				.macros["FOO"].as!FunctionLikeMacro)
			{
				assert(mac.paramCount == 2);
				assertEqual(mac.replacement.map!(t => t.spelling), ["__VA_ARGS__"]);
			}
		});

	crtest!("PP rejects '...' at non-terminal position in macro parameter list",
		() {
			assertThrown(testPP("#define FOO(a, ..., b)"));
		});

	crtest!("# is accepted and stored literally in object-like macro definition",
		() {
			if (auto mac = testPP("#define FOO #abc").macros["FOO"].as!ObjectLikeMacro)
				assertEqual(mac.replacement.map!(t => t.spelling), ["#", "abc"]);

			if (auto mac = testPP("#define FOO #").macros["FOO"].as!ObjectLikeMacro)
				assertEqual(mac.replacement.map!(t => t.spelling), ["#" ]);

			if (auto mac = testPP("#define FOO #1").macros["FOO"].as!ObjectLikeMacro)
				assertEqual(mac.replacement.map!(t => t.spelling), ["#", "1"]);
		});

	crtest!("# fails without a parameter name in function-like macro definition",
		() {
			assertThrown(testPP("#define FOO(p) #"));

			assertThrown(testPP("#define FOO(p) #1"));

			assertThrown(testPP("#define FOO(p) #abc"));

			assertThrown(testPP("#define FOO(p) #p#"));

			assertThrown(testPP("#define FOO(p) #p#1"));
		});

	crtest!("# is accepted when followed by parameter name in function-like macro definition",
		() {
			if (auto mac = testPP("#define FOO(abc, def) # \tdef#abc")
				.macros["FOO"].as!FunctionLikeMacro)
			{
				assert(mac.replacement.length == 4);
				assert(mac.replacement[0].kind == TokenKind.hash);
				assert(mac.replacement[1].isMacroParam);
				assert(mac.replacement[1].macroParamIndex == 1);
				assert(mac.replacement[2].kind == TokenKind.hash);
				assert(mac.replacement[3].isMacroParam);
				assert(mac.replacement[3].macroParamIndex == 0);
			}
		});

	crtest!("## is rejected at initial or terminal position in replacement list",
		() {
			assertThrown(testPP("#define FOO ##"));
			assertThrown(testPP("#define FOO a ##"));
			assertThrown(testPP("#define FOO ## b"));
			assertNotThrown(testPP("#define FOO a ## b"));
			assertThrown(testPP("#define FOO() ##"));
			assertThrown(testPP("#define FOO() a ##"));
			assertThrown(testPP("#define FOO() ## b"));
			assertNotThrown(testPP("#define FOO() a ## b"));
		});

	crtest!("## is accepted between two other tokens in replacement list",
		() {
			if (auto mac = testPP("#define FOO(abc) abc ## def")
				.macros["FOO"].as!FunctionLikeMacro)
			{
				assertEqual(
					mac.replacement.map!(t => t.spelling),
					["abc", "##", "def"]);
			}

			if (auto mac = testPP("#define FOO(abc) abc ## ## def")
				.macros["FOO"].as!FunctionLikeMacro)
			{
				assertEqual(
					mac.replacement.map!(t => t.spelling),
					["abc", "##", "##", "def"]);
			}

		});
}
