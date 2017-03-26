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
	import std.stdio;
	static struct Preprocessor
	{
		alias Lexer = typeof(lexFile(""));

		static struct Macro
		{
			string name;
			const(Token)[] arguments;
			const(Token)[] tokens;
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
			auto name = tl.expect(TokenKind.identifier).spelling;
			tl.popSpaces;
			if (tl.empty) {
				macros[name] = Macro(name);
				return;
			}
			if (tl.front.kind == TokenKind.lparen)
				assert(0, "TODO");
			// C99 requires space after macro name, but we'll silently ignore this rule
			const(Token)[] tokens;
			substituteMacros(tl, tokens);
			macros[name] = Macro(name, [], tokens);
		}

		private void substituteMacro(ref const Macro m)
		{
			foreach (t; m.tokens)
				line ~= t;
		}

		private void substituteMacros(const(Token)[] from, ref const(Token)[] to)
		{
			foreach (t; from) {
				switch (t.kind) {
				case TokenKind.space: case TokenKind.newline:
					break;
				case TokenKind.identifier:
					if (auto pm = t.spelling in macros) {
						substituteMacro(*pm);
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

	return Preprocessor(fs, file_name);
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
