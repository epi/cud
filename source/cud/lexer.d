/**
CTFEable C language lexer.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.lexer;

import std.range : isInputRange, ElementType, empty, front, popFront;

import cud.token;
version(unittest)
{
	import cud.test;
	import std.algorithm : map;
}

private bool isSpace(char x) pure nothrow @nogc
{
	return x == ' ' || x == '\t' || x == '\0';
}

private bool isDigit(char x) pure nothrow @nogc
{
	return x >= '0' && x <= '9';
}

private bool isNonDigit(char x) pure nothrow @nogc
{
	return (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || x == '_';
}

private bool isDigitOrNonDigit(char x) pure nothrow @nogc
{
	return x.isDigit || x.isNonDigit;
}

auto split(string source, string file_name = null)
{
	static struct Splitter
	{
		Line current;
		string source;

		this(string source, string file_name) pure nothrow
		{
			this.source = source;
			current.file = file_name;
			next();
		}

		@property bool empty() const pure nothrow
		{
			return !source.length && !current.content.length;
		}

		@property Line front() const pure nothrow
		{
			return current;
		}

		void popFront() pure nothrow
		{
			next();
			current.num++;
		}

		private void next() pure nothrow
		{
			foreach (i, c; source) {
				if (c == '\n') {
					current.content = source[0 .. i];
					source = source[i + 1 .. $];
					return;
				} else if (c == '\r') {
					current.content = source[0 .. i];
					if (i + 1 < source.length && source[i + 1] == '\n')
						source = source[i + 2 .. $];
					else
						source = source[i + 1 .. $];
					return;
				}
			}
			current.content = source;
			source = null;
		}
	}
	return Splitter(source, file_name);
}

unittest
{
	crtest!("any of CR, CR+LF, LF is accepted as single newline char",
		() {
			assertEqual(
				"foo\nbar\rbaz\r\n\nquux".split,
				[
					Line(0, "foo"),
					Line(1, "bar"),
					Line(2, "baz"),
					Line(3, ""),
					Line(4, "quux")
				]);
		});
}

auto merge(R)(R lines)
	if (isInputRange!R && is(ElementType!R : const(Line)))
{
	static struct Merger
	{
		R input;
		Line current;

		this(R)(R input) pure nothrow
		{
			this.input = input;
			next();
		}

		@property bool empty() const pure nothrow
		{
			return current.content is null;
		}

		@property Line front() const pure nothrow
		{
			return current;
		}

		void popFront() pure nothrow
		{
			next();
		}

		private void next() pure nothrow
		{
			string l;
			while (!input.empty) {
				if (!l) {
					current = input.front;
					l = current.content;
				} else {
					current.mergedOffsets ~= l.length;
					l ~= input.front.content;
				}
				input.popFront();
				while (l.length && l[$ - 1].isSpace)
					l = l[0 .. $ - 1];
				if (l.length < 1 || l[$ - 1] != '\\')
					break;
				l = l[0 .. $ - 1];
			}
			current.content = l;
		}
	}
	return Merger(lines);
}

unittest
{
	crtest!("merge lines on backslash",
		() {
			assertEqual(
				"foo  b\\ \nar\\\nbaz\nquux".split.merge,
				[
					Line(0, "foo  barbaz", null, [6, 8]),
					Line(3, "quux")
				]);
			assertEqual(
				"foo\\".split.merge,
				[Line(0, "foo")]);
		});
}

/**
Decompose an input range of source lines into an input range of preprocessing tokens.

Params:
 input = input range of lines.

Returns:
 Input range of preprocessing tokens.

See_Also:
 `cud.token.Line`, `cud.token.Token`

Standards:
According to ISO C standard draft:

$(I 3. The source file is decomposed into preprocessing tokens and sequences of
white-space characters (including comments). A source file shall not end in a
partial preprocessing token or in a partial comment. Each comment is replaced by
one space character. New-line characters are retained. Whether each nonempty
sequence of white-space characters other than new-line is retained or replaced by
one space character is implementation-defined.

preprocessing-token:
$(UL
 $(LI header-name)
 $(LI identifier)
 $(LI pp-number)
 $(LI character-constant)
 $(LI string-literal)
 $(LI punctuator)
 $(LI each non-white-space character that cannot be one of the above)
))
*/
auto tokenize(R)(R input)
	if (isInputRange!R && is(ElementType!R : const(Line)))
{
	static struct Lexer
	{
		private {
			enum IncludeState
			{
				none,
				hash,
				include,
			}
			R input;
			Token current;
			Location location;
			string line;
			IncludeState includeState;
		}

		this(R input)
		{
			this.input = input;
			next();
		}

		@property bool empty() const pure nothrow
		{
			return current.kind == tk!`eof`;
		}

		@property Token front() const pure nothrow
		{
			return current;
		}

		void popFront()
		{
			next();
		}

		private static struct Tok
		{
			TokenKind kind;
			size_t length;
		}

		private void error(string message)
		{
			//TODO: add Location.
			throw new Exception(message);
		}

		private bool nextLine()
		{
			if (input.empty)
				return false;
			auto nl = input.front;
			input.popFront();
			line = nl.content;
			location = Location(nl.file, nl.num, 0);
			return true;
		}

		private Tok lexMultiLineComment()
		{
			auto l = line;
			size_t n = 2;
			for (;;) {
				if (n + 2 <= l.length) {
					if (l[n .. n + 2] == "*/")
						return Tok(tk!`space`, n + 2);
					n++;
				} else {
					if (!nextLine())
						return Tok(tk!`eof`, 0);
					l = line;
					n = 0;
				}
			}
		}

		// Returns offset of the character immediately following the closing quote
		private size_t lexCharOrStringLiteral(size_t start, char term)
		{
			auto l = line;
			size_t n = start;
			while (n < l.length) {
				if (l[n] == term)
					return n + 1;
				if (l[n] == '\\') {
					n += 2;
					continue;
				}
				n++;
			}
			error(term == '"' ? "Unterminated string literal" : "Unterminated char literal");
			assert(0);
		}

		private Tok lexPpNumber(size_t start)
		{
			auto l = line;
			size_t n = start;
			while (n < l.length) {
				auto c = l[n];
				if ((c == 'E' || c == 'e' || c == 'P' || c == 'p')
					&& n + 1 < l.length
					&& (l[n + 1] == '+' || l[n + 1] == '-')) {
					n += 2;
					continue;
				}
				if (c != '.' && !c.isDigitOrNonDigit)
					return Tok(tk!`ppnumber`, n);
				n++;
			}
			return Tok(tk!`ppnumber`, n);
		}

		private auto scan()
		{
			import std.meta : aliasSeqOf;
			import std.string : split;

			auto l = line;
			if (l.length == 0)
				return Tok(tk!`newline`, 0);
			char c = l[0];

			if (c.isSpace) {
				size_t n = 1;
				while (n < line.length && line[n].isSpace)
					n++;
				return Tok(tk!`space`, n);
			}

			switch (c) {
			foreach (t; aliasSeqOf!(`[ ] { } ( ) ~ ? ; ,`.split)) {
				case t[0]:
					return Tok(tk!t, 1);
			}
				case '/':
					if (l.length > 1) {
						char d = l[1];
						if (d == '/')
							return Tok(tk!`space`, line.length);
						if (d == '*')
							return lexMultiLineComment();
						if (d == '=')
							return Tok(tk!`/=`, 2);
					}
					return Tok(tk!`/`, 1);
				case '.':
					if (l.length >= 2) {
						if (l[1].isDigit)
							return lexPpNumber(1);
						else if (l.length >= 3 && l[1 .. 3] == "..")
							return Tok(tk!`...`, 3);
					}
					return Tok(tk!`.`, 1);
				case '-':
					if (l.length >= 2) {
					auto d = l[1];
						if (d == '-')
							return Tok(tk!`--`, 2);
						else if (d == '=')
							return Tok(tk!`-=`, 2);
						else if (d == '>')
							return Tok(tk!`->`, 2);
					}
					return Tok(tk!`-`, 1);
				case '+':
					if (l.length >= 2) {
						auto d = l[1];
						if (d == '+')
							return Tok(tk!`++`, 2);
						else if (d == '=')
							return Tok(tk!`+=`, 2);
					}
					return Tok(tk!`+`, 1);
				case '&':
					if (l.length >= 2) {
						auto d = l[1];
						if (d == '&')
							return Tok(tk!`&&`, 2);
						else if (d == '=')
							return Tok(tk!`&=`, 2);
					}
					return Tok(tk!`&`, 1);
				case '*':
					if (l.length >= 2 && l[1] == '=')
						return Tok(tk!`*=`, 2);
					return Tok(tk!`*`, 1);
				case '!':
					if (l.length >= 2 && l[1] == '=')
						return Tok(tk!`!=`, 2);
					return Tok(tk!`!`, 1);
				case '%':
					if (l.length >= 2) {
						auto d = l[1];
						if (d == '=')
							return Tok(tk!`%=`, 2);
						else if (d == '>')
							return Tok(tk!`}`, 2);
						else if (d == ':') {
							if (l.length >= 4 && l[2 .. 4] == "%:")
								return Tok(tk!`##`, 4);
							return Tok(tk!`#`, 2);
						}
					}
					return Tok(tk!`%`, 1);
				case '<':
					if (includeState == IncludeState.include) {
						size_t i = 1;
						while (i < l.length && l[i] != '>')
							i++;
						if (i == l.length)
							error("Unterminated header name");
						return Tok(tk!`headername`, i + 1);
					} else if (l.length >= 2) {
						auto d = l[1];
						if (d == '=')
							return Tok(tk!`<=`, 2);
						else if (d == '%')
							return Tok(tk!`{`, 2);
						else if (d == ':')
							return Tok(tk!`[`, 2);
						else if (d == '<') {
							if (l.length >= 3 && l[2] == '=')
								return Tok(tk!`<<=`, 3);
							return Tok(tk!`<<`, 2);
						}
					}
					return Tok(tk!`<`, 1);
				case '>':
					if (l.length >= 2) {
						auto d = l[1];
						if (d == '=')
							return Tok(tk!`>=`, 2);
						else if (d == '>') {
							if (l.length >= 3 && l[2] == '=')
								return Tok(tk!`>>=`, 3);
							return Tok(tk!`>>`, 2);
						}
					}
					return Tok(tk!`>`, 1);
				case '=':
					if (l.length >= 2 && l[1] == '=')
						return Tok(tk!`==`, 2);
					return Tok(tk!`=`, 1);
				case '^':
					if (l.length >= 2 && l[1] == '=')
						return Tok(tk!`^=`, 2);
					return Tok(tk!`^`, 1);
				case '|':
					if (l.length >= 2) {
						auto d = l[1];
						if (d == '|')
							return Tok(tk!`||`, 2);
						else if (d == '=')
							return Tok(tk!`|=`, 2);
					}
					return Tok(tk!`|`, 1);
				case ':':
					if (l.length >= 2 && l[1] == '>')
						return Tok(tk!`]`, 2);
					return Tok(tk!`:`, 1);
				case '#':
					if (l.length >= 2 && l[1] == '#')
						return Tok(tk!`##`, 2);
					return Tok(tk!`#`, 1);
				case '"':
					return Tok(
						includeState == IncludeState.include
							? tk!`headername` : tk!`stringliteral`,
						lexCharOrStringLiteral(1, '"'));
				case '\'':
					return Tok(tk!`charconst`, lexCharOrStringLiteral(1, '\''));
				case '0': .. case '9':
					return lexPpNumber(0);
				case 'u': case 'U': case 'L':
					if (l.length >= 2) {
						auto d = l[1];
						if (d == '"' || d == '\'')
							return Tok(d == '"'
								? tk!`stringliteral`
								: tk!`charconst`,
								lexCharOrStringLiteral(2, d));
					}
					goto case;
				case 'A': .. case 'K':
				case 'M': .. case 'T':
				case 'V': .. case 'Z':
				case 'a': .. case 't':
				case 'v': .. case 'z':
				case '_': {
					size_t i = 1;
					while (i < l.length && l[i].isDigitOrNonDigit)
						i++;
					return Tok(tk!`identifier`, i);
				}
				default:
					// TODO: other tokens
					return Tok(tk!``, 1);
			}
		}

		private void next()
		{
			uint len = cast(uint) current.spelling.length;
			if (len != 0) {
				line = line[len .. $];
			} else {
				if (!nextLine()) {
					current.kind = tk!`eof`;
					return;
				}
			}
			auto tok = scan();

			// context-dependent lexing of header-names
			if (tok.kind == tk!`#`)
				includeState = IncludeState.hash;
			else if (includeState == IncludeState.hash
				&& tok.kind == tk!`identifier`
				&& line[0 .. tok.length] == "include")
				includeState = IncludeState.include;
			else if (tok.kind == tk!`space`)
			{}
			else
				includeState = IncludeState.none;

			current = Token(
				tok.kind,
				location,
				line[0 .. tok.length]);
			location.column += tok.length;
		}
	}

	return Lexer(input);
}

unittest
{
	crtest!("empty range of lines is lexed to empty range of tokens",
		() {
			assert(Line[].init.tokenize.empty);
		});

	crtest!("comments are lexed into spaces",
		() {
			assertEqual(
				("  \n // line comment\n   /* multi- // line \n\n\n\n" ~
				" comment */   \n/* short */\n/*ignore unterminated comment")
					.split.merge.tokenize,
				[
					Token(tk!`newline`, Location("", 0, 0), ""), // merger cuts spaces at the end of line
					Token(tk!`space`,   Location("", 1, 0), " "),
					Token(tk!`space`,   Location("", 1, 1), "// line comment"),
					Token(tk!`newline`, Location("", 1, 16), ""),
					Token(tk!`space`,   Location("", 2, 0), "   "),
					Token(tk!`space`,   Location("", 6, 0), " comment */"),
					Token(tk!`newline`, Location("", 6, 11), ""),
					Token(tk!`space`,   Location("", 7, 0), "/* short */"),
					Token(tk!`newline`, Location("", 7, 11), ""),
				]);
		});
}

unittest
{
	crtest!("lex punctuators",
		() {
			assertEqual(
				("[](){}.->++--&*+-~!/%<<>><><=>===!=^|&&||?:;..." ~
				"=*=/=%=+=-=<<=>>=&=^=|=,###<::><%%>%:%:%:")
					.split.merge.tokenize.map!(a => a.kind),
				[
					tks!`[ ] ( ) { } . ->`,
					tks!`++ -- & * + - ~ !`,
					tks!`/ % << >> < > <= >= == != ^ | && ||`,
					tks!`? : ; ...`,
					tks!`= *= /= %= += -=`,
					tks!`<<= >>= &= ^= |=`,
					tks!`, ## #`,
					tks!`[ ] { } ## #`,
					tks!`newline`
				]);
		});
}

unittest
{
	crtest!("lex some pptokens",
		() {
			assertEqual(
				"#include <stdio.h>\n<foo\"foo\"\n#include \"foo\""
					.split.merge.tokenize,
				[
					Token(tk!`#`,             Location("", 0, 0), "#"),
					Token(tk!`identifier`,    Location("", 0, 1), "include"),
					Token(tk!`space`,         Location("", 0, 8), " "),
					Token(tk!`headername`,    Location("", 0, 9), "<stdio.h>"),
					Token(tk!`newline`,       Location("", 0, 18), ""),
					Token(tk!`<`,             Location("", 1, 0), "<"),
					Token(tk!`identifier`,    Location("", 1, 1), "foo"),
					Token(tk!`stringliteral`, Location("", 1, 4), `"foo"`),
					Token(tk!`newline`,       Location("", 1, 9), ""),
					Token(tk!`#`,             Location("", 2, 0), "#"),
					Token(tk!`identifier`,    Location("", 2, 1), "include"),
					Token(tk!`space`,         Location("", 2, 8), " "),
					Token(tk!`headername`,    Location("", 2, 9), `"foo"`),
					Token(tk!`newline`,       Location("", 2, 14), ""),
				]);
		});
}

unittest
{
	crtest!("6.4.7, 4. Example",
		() {
			enum str = "0x3<1/a.h>1e2\n#include <1/a.h>\n#define const.member@$";
			assertEqual(
				str.split.merge.tokenize.map!(t => t.spelling),
				[
					"0x3", "<", "1", "/", "a", ".", "h", ">", "1e2", "",
					"#", "include", " ", "<1/a.h>", "",
					"#", "define", " ", "const", ".", "member", "@", "$", ""
				]);
			assertEqual(
				str.split.merge.tokenize.map!(t => t.kind),
				[
					tks!`ppnumber < ppnumber / identifier .`,
					tks!`identifier > ppnumber newline`,
					tks!`# identifier space headername newline`,
					tks!`# identifier space identifier . identifier`,
					tk!``, tk!``, tk!`newline`
				]);
		});
}

unittest
{
	enum str = `'a'"foo"L'x20'L"bar"u'x'U'x'u"foo"U"bar"`;
	crtest!("chars and strings",
		() {
			assertEqual(
				str.split.merge.tokenize.map!(t => t.spelling),
				[
					`'a'`, `"foo"`, `L'x20'`, `L"bar"`,
					`u'x'`, `U'x'`, `u"foo"`, `U"bar"`, ""
				]);
			assertEqual(str.split.merge.tokenize.map!(t => t.kind),
				[
					tks!`charconst stringliteral charconst stringliteral`,
					tks!`charconst charconst stringliteral stringliteral newline`
				]);
		});
}

Token pasteTokens(Token lhs, Token rhs) pure
{
	if (rhs.kind == tk!`placemarker`)
		return lhs;
	if (lhs.kind == tk!`placemarker`)
		return rhs;
	string spelling = lhs.spelling ~ rhs.spelling;
	auto line = Line(lhs.location.line, spelling, lhs.location.file, null);
	import std.range : only;
	return line.only.tokenize.front;
}
