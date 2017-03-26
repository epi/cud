/**
CTFEable C language preprocessor.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/

module cud.preprocessor;

import std.range : isInputRange, ElementType, empty, popFront, front;

import cud.token;
import cud.lexer;

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

private struct Preprocessor(FS)
	if (isFS!FS)
{
	alias Lexer = typeof(lexFile(""));

	static struct Macro
	{
		string name;
		const(Token)[] params;
		const(Token)[] replacement;
		bool isFunctionMacro;
	}

	private {
		FS fs;
		Lexer[] lexerStack;
		const(Token)[] line;
		Macro[string] macros;
	}

	this(FS fs, string file_name)
	{
		this.fs = fs;
		stackPush(lexFile(file_name));
		nextLine();
	}

	@property bool empty() const pure nothrow
	{
		return line.length == 0 && stackEmpty;
	}

	@property Token front() const pure nothrow
	{
		return line[0];
	}

	void popFront()
	{
		if ((line = line[1 .. $]).length == 0)
			nextLine();
	}

	private auto lexFile(string name)
	{
		return split(fs[name], name).merge.tokenize;
	}

	private @property bool stackEmpty() pure nothrow const
	{
		return lexerStack.length == 0;
	}

	private void stackPop()
	{
		lexerStack = lexerStack[0 .. $ - 1];
	}

	private @property Lexer* stackTop() pure nothrow
	{
		return &lexerStack[$ - 1];
	}

	private void stackPush(Lexer i)
	{
		import std.algorithm : move;
		lexerStack ~= move(i);
	}

	private void defineMacro(const(Token)[] tl)
	{
		auto m = Macro(tl.expect(TokenKind.identifier).spelling);
		if (!tl.empty && tl.front.kind == TokenKind.lparen) {
			m.isFunctionMacro = true;
			tl.popFront;
			if (!tl.match(TokenKind.rparen)) {
				for (;;) {
					if (Token tok = tl.match(TokenKind.identifier)) {
						m.params ~= tok;
						if (tl.match(TokenKind.rparen))
							break;
						tl.expect(TokenKind.comma);
					} else {
						m.params ~= tl.expect(TokenKind.ellipsis, TokenKind.rparen);
						break;
					}
				}
			}
		}
		// 6.10.3.3 requires space after macro name, but we'll silently ignore this rule
		substituteMacros(tl, m.replacement);
		macros[m.name] = m;
	}

	unittest
	{
		import std.algorithm : map, equal;
		import cud.test;
		crtest!("All forms of macro definitions are accepted",
			() {
				auto fs = [ "foo.h" :
					"#define FOO \n" ~
					"#define BAR 42 + bar\n" ~
					"#define BAQ(  )\n" ~
					"#define BAT(x)\n" ~
					"#define BAZ(x, y)\n" ~
					"#define BAL(...)\n" ~
					"#define BAU(x, y, ...) (x - y) / baz(__VA_ARGS__)\n" ~
					"#define BAN ( x ## y )" ];

				auto pp = fs.preprocess("foo.h");
				while (!pp.empty)
					pp.popFront;

				assert(!pp.macros["FOO"].isFunctionMacro);
				assert(pp.macros["FOO"].params.length == 0);
				assert(pp.macros["FOO"].replacement.length == 0);

				assert(!pp.macros["BAR"].isFunctionMacro);
				assert(pp.macros["BAR"].params.length == 0);
				assert(pp.macros["BAR"].replacement.map!(t => t.spelling).equal(["42", "+", "bar"]));

				assert(pp.macros["BAQ"].isFunctionMacro);
				assert(pp.macros["BAQ"].params.length == 0);
				assert(pp.macros["BAQ"].replacement.length == 0);

				assert(pp.macros["BAT"].isFunctionMacro);
				assert(pp.macros["BAT"].params.map!(t => t.spelling).equal(["x"]));
				assert(pp.macros["BAT"].replacement.length == 0);

				assert(pp.macros["BAZ"].isFunctionMacro);
				assert(pp.macros["BAZ"].params.map!(t => t.spelling).equal(["x", "y"]));
				assert(pp.macros["BAZ"].replacement.length == 0);

				assert(pp.macros["BAL"].isFunctionMacro);
				assert(pp.macros["BAL"].params.map!(t => t.spelling).equal(["..."]));
				assert(pp.macros["BAL"].replacement.length == 0);

				assert(pp.macros["BAU"].isFunctionMacro);
				assert(pp.macros["BAU"].params.map!(t => t.spelling).equal(["x", "y", "..."]));
				assert(pp.macros["BAU"].replacement.map!(t => t.spelling).equal([
							"(", "x", "-", "y", ")", "/", "baz", "(", "__VA_ARGS__", ")"
						]));

				assert(!pp.macros["BAN"].isFunctionMacro);
				assert(pp.macros["BAN"].params.length == 0);
				assert(pp.macros["BAN"].replacement.map!(t => t.spelling).equal(["(", "x", "##", "y", ")"]));
				return true;
			});
	}

	private void substituteMacros(const(Token)[] from, ref const(Token)[] to)
	{
		foreach (t; from) {
			switch (t.kind) {
			case TokenKind.space: case TokenKind.newline:
				break;
			case TokenKind.identifier:
				if (auto pm = t.spelling in macros) {
					foreach (mt; pm.replacement)
						to ~= mt;
					break;
				}
				goto default;
			default:
				to ~= t;
			}
		}
	}

	private void includeFile(const(Token)[] tl)
	{
		auto name = tl.expect(TokenKind.headername, TokenKind.newline).spelling;
		assert(name.length >= 2);
		name = name[1 .. $ - 1];
		stackPush(lexFile(name));
	}

	private const(Token)[] executePreprocessorDirective(const(Token)[] tl)
	{
		tl.popSpaces;
		if (tl.empty)
			return tl;
		auto d = tl[0].spelling;
		tl = tl[1 .. $];
		switch (d) {
			case "define":
				defineMacro(tl);
				return [];
			case "include":
				includeFile(tl);
				return [];
			default:
				throw new Exception("invalid preprocessor directive #" ~ d);
		}
		//return tl;
	}

	private void nextLine()
	{
		const(Token)[] tl;
		for (;;) {
			while (!stackEmpty && stackTop.empty)
				stackPop();
			if (stackEmpty)
				return;
			auto lexer = stackTop;
			while (!lexer.empty && lexer.front.kind == TokenKind.space)
				lexer.popFront;
			while (!lexer.empty) {
				auto token = lexer.front;
				lexer.popFront;
				tl ~= token;
				if (token.kind == TokenKind.newline)
					break;
			}
			if (tl.empty)
				continue;
			if (tl.front.kind == TokenKind.hash) {
				tl.popFront;
				tl = executePreprocessorDirective(tl);
				if (tl.empty)
					continue;
			}
			substituteMacros(tl, line);
			if (line.length)
				return;
			tl.length = 0;
		}
	}
}

/**
Preprocess a file: execute preprocessor directives,
convert pp-tokens into tokens.

Params:
 fs = source file system
 file_name = name of the file to preprocess

Returns:
 Input range of tokens.

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
auto preprocess(FS)(FS fs, string file_name)
	if (isFS!FS)
{
	return Preprocessor!FS(fs, file_name);
}

unittest
{
	import std.algorithm : map, equal;

	enum fs = [
		"foo.h" : " #include <bar.h>\n\n\n  \t#  \t define BAZ BAR\nFOO BAZ FOO",
		"bar.h" : "#\t\t\tdefine BAR 42\n#define FOO\t \t\t\n"
	];
	static assert(fs.preprocess("foo.h").map!(t => t.spelling).equal(["42"]));
}
