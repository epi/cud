/**
CTFEable C language preprocessor.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/

module cud.preprocessor;

import std.range : isInputRange, ElementType, empty, popFront, front;

import cud.token : Token, TokenKind, match, expect, popSpaces, error;
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

struct Preprocessor(FS, bool keepSpaces = false)
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
		static if (keepSpaces)
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
				} else if (tok.kind != TokenKind.identifier) {
					if (tok.kind == TokenKind.notreplacedidentifier)
						put(Token(TokenKind.identifier, tok.location, tok.spelling));
					else if (tok.kind != TokenKind.placemarker)
						put(tok);
				} else if (auto pm = tok.spelling in m_macros) {
					if (!replaceMacro(*pm, tok, m_input))
						put(tok);
				} else {
					put(tok);
				}
			}
		}
		return m_output;
	}

	private void put(in Token tok) pure
	{
		static if (!keepSpaces) {
			if (tok.kind == TokenKind.space || tok.kind == TokenKind.newline)
				return;
		} else {
			if (tok.kind == TokenKind.space
			 && m_output[$ - 1].kind == TokenKind.space)
			return;
		}
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

	private void errorDirective(in Token err_tok, ref const(Token)[] input) pure
	{
		string message;
		while (!input.empty) {
			const tok = input.front;
			input.popFront;
			if (tok.kind == TokenKind.newline)
				break;
			message ~= input.front.spelling;
		}
		error(err_tok, message);
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
		if (tok.kind == TokenKind.newline)
			return;
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
			case "line":
				goto ignore;
			case "error":
				errorDirective(tok, input);
				break;
			case "pragma":
				goto ignore;
			case "if":
				evaluateConstantExpression(input);
				break;
			default:
				error(tok, "invalid preprocessing directive '%s'", tok.spelling);
			}
		} else {
		ignore:
			while (!input.empty) {
				const discard = input.front;
				input.popFront;
				if (discard.kind == TokenKind.newline)
					break;
			}
		}
	}

	private const(Token)[][] parseArgs(uint param_count)
	{
		return parseArgs(m_input, param_count);
	}

	private const(Token)[][] parseArgs(
		ref const(Token)[] input,
		uint param_count)
	{
		Token popSpacesAcrossLines()
		{
			Token result;
			while (!input.empty) {
				const tok = input.front;
				if (tok.kind != TokenKind.newline && tok.kind != TokenKind.space)
					break;
				if (!result)
					result = Token(TokenKind.space, tok.location, " ");
				input.popFront;
				if (tok.kind == TokenKind.newline) {
					while (input.match(TokenKind.hash))
						directive(input);
				}
			}
			return result;
		}

		popSpacesAcrossLines();
		if (input.empty || input.front.kind != TokenKind.lparen)
			return null;

		immutable tok_lparen0 = input.front;
		input.popFront;

		auto args = new const(Token)[][](param_count == 0 ? 1 : param_count);
		int current = 0;
		int nest_level = 0;
		for (;;) {
			if (immutable space = popSpacesAcrossLines())
				args[current] ~= space;
			if (input.empty)
				error(tok_lparen0, "unterminated macro argument list");
			immutable tok = input.front;
			input.popFront;
			if (tok.kind == TokenKind.lparen) {
				nest_level++;
			} else if (tok.kind == TokenKind.rparen) {
				if (nest_level == 0)
					break;
				nest_level--;
			} else if (tok.kind == TokenKind.comma
				&& nest_level == 0
				&& current + 1 < args.length)
			{
				current++;
				continue;
			}
			args[current] ~= tok;
		}
		foreach (ref arg; args) {
			while (arg.length && arg[0].kind == TokenKind.space)
				arg = arg[1 .. $];
			while (arg.length && arg[$ - 1].kind == TokenKind.space)
				arg = arg[0 .. $ - 1];
		}
		return args;
	}

	// Returns true iff the macro has been inserted into input for rescanning.
	private bool replaceMacro(Macro m, Token macro_ref, ref const(Token)[] input) pure
	{
		bool insert = true;
		Token[] result;
		m.accept(new class NoOpMacroVisitor {
			override void visit(const ObjectLikeMacro m)
			{
				auto rep = m.replacement[];
				while (!rep.empty) {
					Token tok = rep.front;
					rep.popFront;
					if (tok.kind == TokenKind.hashhash) {
						while (rep.front.kind == TokenKind.hashhash)
							rep.popFront;
						assert(result.length > 0);
						assert(result[$ - 1].kind != TokenKind.space);
						assert(rep.front.kind != TokenKind.space);
						result[$ - 1] = pasteTokens(result[$ - 1], rep.front);
						rep.popFront;
					} else {
						result ~= tok;
					}
				}
			}

			override void visit(const FunctionLikeMacro m)
			{
				auto args = parseArgs(input, m.paramCount);
				if (!args) {
					insert = false;
					return;
				}

				auto replaced_args = new const(Token)[][](m.paramCount);
				auto stringified_args = new Token[](m.paramCount);

				const(Token)[] argOrPlacemarker(Token mac_tok)
				{
					const(Token)[] arg = args[mac_tok.macroParamIndex];
					if (arg.empty)
						return [Token(TokenKind.placemarker, mac_tok.location, "")];
					return arg;
				}

				const(Token)[] cachedReplacedArg(uint index)
				{
					if (!replaced_args[index]) {
						Token[] rarg;
						const(Token)[] arg = args[index];
						while (!arg.empty) {
							const tok = arg.front;
							arg.popFront;
							if (tok.kind != TokenKind.identifier)
								rarg ~= tok;
							else if (auto pm = tok.spelling in m_macros) {
								if (!replaceMacro(*pm, tok, arg))
									rarg ~= tok;
							} else {
								rarg ~= tok;
							}
						}
						replaced_args[index] = rarg;
					}
					return replaced_args[index];
				}

				Token cachedStringifiedArg(uint index)
				{
					if (!stringified_args[index]) {
						import std.array : appender;
						auto arg = args[index];
						auto strapp = appender!string;
						strapp.put('\"');
						foreach (tok; arg) {
							if (tok.kind == TokenKind.charconstant || tok.kind == TokenKind.stringliteral) {
								foreach (char c; tok.spelling) {
									if (c == '"' || c == '\\')
										strapp.put('\\');
									strapp.put(c);
								}
							} else {
								strapp.put(tok.spelling);
							}
						}
						strapp.put('\"');
						stringified_args[index] = Token(
							TokenKind.stringliteral,
							arg.length ? arg[0].location : macro_ref.location,
							strapp.data);
					}
					return stringified_args[index];
				}

				auto rep = m.replacement[];
				while (!rep.empty) {
					Token tok = rep.front;
					rep.popFront;
					if (tok.kind == TokenKind.hash) {
						assert(!rep.empty && rep.front.isMacroParam);
						result ~= cachedStringifiedArg(rep.front.macroParamIndex);
						rep.popFront;
					} else if (tok.isMacroParam) {
						if (!rep.empty && rep.front.kind == TokenKind.hashhash)
							result ~= argOrPlacemarker(tok);
						else
							result ~= cachedReplacedArg(tok.macroParamIndex);
					} else if (tok.kind == TokenKind.hashhash) {
						assert(!rep.empty);
						while (rep.front.kind == TokenKind.hashhash)
							rep.popFront;
						assert(!rep.empty);
						Token rhstok = rep.front;
						rep.popFront;
						if (rhstok.isMacroParam) {
							const(Token)[] rhsarg = argOrPlacemarker(rhstok);
							result[$ - 1] = pasteTokens(result[$ - 1], rhsarg[0]);
							result ~= rhsarg[1 .. $];
						} else {
							result[$ - 1] = pasteTokens(result[$ - 1], rhstok);
						}
					} else {
						result ~= tok;
					}
				}
			}
		});
		foreach (i, ref tok; result) {
			if (tok.kind == TokenKind.identifier && tok.spelling == m.name)
				tok.kind = TokenKind.notreplacedidentifier;
		}
		if (insert)
			input = result ~ input;
		return insert;
	}

	int evaluateConstantExpression(ref const(Token)[] tokens) pure
	{
		const(Token)[] expr_tokens;
		while (!tokens.empty) {
			const tok = tokens.popSpaces;
			tokens.popFront;
			if (tok.kind == TokenKind.newline)
				break;
			if (tok.kind == TokenKind.identifier) {
				if (tok.spelling == "defined") {
					// 6.10.1, Constraint 1
					bool paren = !!tokens.match(TokenKind.lparen);
					const name = tokens.expect(TokenKind.identifier).spelling;
					if (paren)
						tokens.expect(TokenKind.rparen);
					expr_tokens ~= Token(
						TokenKind.ppnumber, tok.location,
						name in m_macros ? "1" : "0");
					continue;
				} else if (auto pm = tok.spelling in m_macros) {
					// 6.10.1, 4
					if (replaceMacro(*pm, tok, tokens))
						continue;
				}
				expr_tokens ~= Token(TokenKind.ppnumber, tok.location, "0");
			} else if (tok.kind == TokenKind.stringliteral) {
				error(tok, "string literals are not valid tokens in preprocessor expressions");
			} else {
				expr_tokens ~= tok;
			}
		}

		debug if (!__ctfe) { writefln("Expr %s", format(expr_tokens)); }
		return 0;
	}

	debug private string format(const(Token)[] tokens) pure
	{
		enum color_space      = "\x1b[38;2;255;255;255;48;2;48;48;48m";
		enum color_hash       = "\x1b[38;2;255;0;0;48;2;32;32;32m";
		enum color_hashhash   = "\x1b[38;2;255;128;0;48;2;32;32;32m";
		enum color_macro      = "\x1b[38;2;128;192;255;48;2;32;32;32m";
		enum color_identifier = "\x1b[38;2;128;255;128;48;2;32;32;32m";
		enum color_param      = "\x1b[38;2;255;160;255;48;2;32;32;32m";
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
			} else if (tok.kind == TokenKind.placemarker) {
				app.put(color_space);
				app.put('¿');
				app.put(color_reset);
			} else {
				switch (tok.kind) with (TokenKind) {
					case notreplacedidentifier:
						if (tok.spelling in m_macros)
							app.put("\x1b[32;45;1m");
						else
							app.put("\x1b[34;45;1m");
						break;
					case identifier:
						if (tok.spelling in m_macros)
							app.put(color_macro);
						else
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
						if (tok.isMacroParam)
							app.put(color_param);
						else
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

unittest // parseArgs
{
	static auto makePP(string source)
	{
		return Preprocessor!(string[string])(
			[ "test.h" : source ],
			"test.h");
	}

	crtest!("parseArgs rejects unterminated argument list",
		() {
			assertThrown(makePP("(").parseArgs(0));
			assertThrown(makePP("(  \t\t    \t").parseArgs(0));
			assertThrown(makePP("   (\n  \t\t").parseArgs(0));
			assertThrown(makePP("(    \n\n\n\n  \t \n").parseArgs(0));
		});

	crtest!("parseArgs returns null and doesn't advance input (except spaces) on missing argument list",
		() {
			{
				auto pp = makePP("");
				auto args = pp.parseArgs(0);
				assert(pp.m_input.empty);
				assert(args is null);
			}

			{
				auto pp = makePP(" \n\n   \tfoo");
				auto args = pp.parseArgs(0);
				assert(pp.m_input.front.spelling == "foo");
				assert(args is null);
			}
		});

	crtest!("parseArgs parses empty argument list",
		() {
			auto args = makePP("   \n\n ( \t\t\n)").parseArgs(0);
			assert(args.length == 1);
			assert(args[0].length == 0);
		});
}

unittest {
	crtest!("#include + multi-level object-like macro substitution",
		() {
			enum fs = [
				"foo.h" : " #include <bar.h>\n\n\n  \t#  \t define BAZ BAR\nFOO BAZ FOO",
				"bar.h" : "#\t\t\tdefine BAR 42\n#define FOO\t \t\t\n"
			];
			testPP(fs, "foo.h", "42");
		});

	crtest!("simple stringification",
		() {
			testPP(
				"#define xstr(s) str(s)\n" ~
				"#define str(s) #s\n" ~
				"#define foo 4\n" ~
				"str (foo)\n" ~
				"xstr (foo)\n",
				`"foo" "4"`);
		});

	crtest!("6.10.3.4, 4. example: behave like GCC and clang with unspecified reexamination",
		() {
			testPP(
				"#define f(a) a*g\n" ~
				"#define g(a) f(a)\n" ~
				"f(2)(9)",
				"2*9*g");
		});

	crtest!("stringification of argument spanning multiple lines of source",
		() {
			testPP(
				"#define FOO(d) #d\n" ~
				"FOO(    a 42 b bar .\n\n" ~
				"\"baz\"->                  a  )\n",
				`"a 42 b bar . \"baz\"-> a"`);
		});

	crtest!("token pasting with rhs from included file",
		() {
			enum fs = [
				"baz.h" : "#define FOO(a, b) a ## b\nFOO(<,\n#include \"bar.h\"",
				"bar.h" : "=)"
			];
			testPP(fs, "baz.h", "<=");
		});

	crtest!("token pasting with rhs defined in place",
		() {
			testPP(
				"#define FOO(a, b) a ## b\n" ~
				"FOO(<,\n" ~
				"#define BAZ =\n" ~
				"=)\n",
				"<=");
		});

	crtest!("6.10.3.3, 4. example",
		() {
			testPP(
				"#define hash_hash # ## #\n" ~
				"#define mkstr(a) # a\n" ~
				"#define in_between(a) mkstr(a)\n" ~
				"#define join(c, d) in_between(c hash_hash d)\n" ~
				"char p[] = join(x, y);\n",
				`char p[] = "x ## y";`);
		});

	crtest!("token concatenation",
		() {
			testPP(
				"#define BOTH(a, b) a ## b\n" ~
				"#define LEFT(a) a ## b\n" ~
				"#define RIGHT(b) a ## b\n" ~
				"#define NONE() a ## b\n" ~
				"BOTH(x1 x2, y1 y2)\n" ~
				"LEFT(x1 x2)\n" ~
				"RIGHT(x1 x2)\n" ~
				"NONE()\n",
				"x1 x2y1 y2 x1 x2b ax1 x2 ab");
		});

	crtest!("multiple consecutive ##s ",
		() {
			testPP(
				"#define FOO(abc) abc ## ## def\n" ~
				"FOO(xyz)",
				"xyzdef");
		});

	crtest!("6.10.3.5, 5. example 3: redefinition and reexamination",
		() {
			enum defs =
				"#define x 3\n" ~
				"#define f(a) f(x * (a))\n" ~
				"#undef x\n" ~
				"#define x 2\n" ~
				"#define g f\n" ~
				"#define z z[0]\n" ~
				"#define h g(~\n" ~
				"#define m(a) a(w)\n" ~
				"#define w 0,1\n" ~
				"#define t(a) a\n" ~
				"#define p() int\n" ~
				"#define q(x) x\n" ~
				"#define r(x,y) x ## y\n" ~
				"#define str(x) # x\n";

			testPP(
				defs ~ "f(y+1) + f(f(z)) % t(t(g)(0) + t)(1);\n",
				"f(2 * (y+1)) + f(2 * (f(2 * (z[0])))) % f(2 * (0)) + t(1);");
			testPP(
				defs ~ "g(x+(3,4)-w) | h 5) & m\n    (f)^m(m);\n",
				"f(2 * (2+(3,4)-0,1)) | f(2 * (~ 5)) & f(2 * (0,1))^m(0,1);");
			testPP(
				defs ~ "p() i[q()] = { q(1), r(2,3), r(4,), r(,5), r(,) };\n",
				"int i[] = { 1, 23, 4, 5, };");
			testPP(
				defs ~ "char c[2][6] = { str(hello), str() };\n",
				`char c[2][6] = { "hello", "" };`);
		});

	crtest!("6.10.3.5, 6. example 4: creating character string literals and concatenating tokens",
		() {
			enum defs =
				"#define str(s) # s\n" ~
				"#define xstr(s) str(s)\n" ~
				`#define debug(s, t) printf("x" # s "= %d, x" # t "= %s", \` ~ "\n" ~
				"                        x ## s, x ## t)\n" ~
				"#define INCFILE(n) vers ## n\n" ~
				"#define glue(a, b) a ## b\n" ~
				"#define xglue(a, b) glue(a, b)\n" ~
				`#define HIGHLOW "hello"` ~ "\n" ~
				`#define LOW LOW ", world"` ~ "\n";

			testPP(
				defs ~ "debug(1, 2);\n",
				`printf("x" "1" "= %d, x" "2" "= %s", x1, x2); `);
			testPP(
				defs ~ `fputs(str(strncmp("abc\0d", "abc", '\4') // this goes away` ~ "\n" ~
				`      == 0) str(: @\n), s);` ~ "\n",
				`fputs( ` ~
				`  "strncmp(\"abc\\0d\", \"abc\", '\\4') == 0" ": @\n", ` ~
				`	s); `);

			// TODO:
			// not tested because cud's #include currently doesn't replace macros inside #include
			// "#include xstr(INCFILE(2).h)\n"

			testPP(
				defs ~
				"xstr(INCFILE(2).h)\n" ~
				"glue(HIGH, LOW);\n" ~
				"xglue(HIGH, LOW)\n",
				`"vers2.h" ` ~
				`"hello"; ` ~
				`"hello" ", world"`);
		});

	crtest!("6.10.3.5, 7. example 5: placemarker preprocessing tokens",
		() {
			testPP(
				"#define t(x,y,z) x ## y ## z\n" ~
				"int j[] = { t(1,2,3), t(,4,5), t(6,,7), t(8,9,),\n" ~
				"           t(10,,), t(,11,), t(,,12), t(,,) };\n",
				"int j[] = { 123, 45, 67, 89, 10, 11, 12, };");
		});

	crtest!("6.10.3.5, 9. example 7: variable argument list macro facilities",
		() {
			testPP(
				"#define debug(...) fprintf(stderr, __VA_ARGS__)\n" ~
				"#define showlist(...) puts(#__VA_ARGS__)\n" ~
				"#define report(test, ...) ((test)?puts(#test):\\\n" ~
				"printf(__VA_ARGS__))\n" ~
				`debug("Flag");` ~ "\n" ~
				`debug("X = %d\n", x);` ~ "\n" ~
				`showlist(The first, second, and third items.);` ~ "\n" ~
				`report(x>y, "x is %d but y is %d", x, y);`,
				`fprintf(stderr, "Flag" ); ` ~
				`fprintf(stderr, "X = %d\n", x ); ` ~
				`puts( "The first, second, and third items." ); ` ~
				`((x>y)?puts("x>y"): ` ~
				`            printf("x is %d but y is %d", x, y));`);
		});
}

unittest {
	// FIXME
	crtest!("#line is accepted, ignoring all pptokens up to newline",
		() {
			testPP(
				"#line\n" ~
				"#line bla bla bla\n" ~
				"#line 1\n" ~
				`#line 2147483647 "file.c"` ~ "\n" ~
				"foo",
				"foo");
		});

	crtest!("#pragma is accepted, ignoring all pptokens up to newline",
		() {
			testPP(
				"#pragma\n" ~
				"#pragma once\n" ~
				"#pragma warning\n" ~
				"#pragma STDC FENV_ACCESS DEFAULT\n" ~
				"#pragma foo bar\n" ~
				"#pragma STDC foo bar\n" ~
				"foo",
				"foo");
		});

	crtest!("null directive and non-directive are accepted and has no effect",
		() {
			testPP("#   \t \nfoo", "foo");
			testPP("#  ! 5 43 \t \nfoo", "foo");
		});
}

unittest {
	crtest!("#error throws with a message containing the spelling of all tokens",
		() {
			import std.exception : collectExceptionMsg;
			import std.string : endsWith;
			assert(collectExceptionMsg(testPP("#error 42 foo \"bar\"")).endsWith("42 foo \"bar\""));
		});
}

unittest {
	crtest!("#if reads tokens up to newline",
		() {
			testPP("#define foo\n#if dupa + defined(foo) (defined foo) && defined bar( bar)\nfoo");
		});
}
