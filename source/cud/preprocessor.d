module cud.preprocessor;

import std.range : isInputRange, ElementType, empty, popFront, front;

struct Line
{
	uint num;
	string content;
	string file;
	immutable(size_t)[] mergedOffsets;
}

enum TokenKind : int
{
	reserved,

	eof,

	space,
	newline,
}

struct Location
{
	string file;
	uint line;
	uint column;
}

struct Token
{
	TokenKind kind;
	Location location;
	string spelling;
}

private bool isSpace(char x) pure nothrow @nogc
{
	return x == ' ' || x == '\t' || x == '\0';
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
	import std.algorithm : equal;
	static assert("foo\nbar\rbaz\r\n\nquux".split.equal(
			[
				Line(0, "foo"),
				Line(1, "bar"),
				Line(2, "baz"),
				Line(3, ""),
				Line(4, "quux")
			]));
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
	import std.algorithm : equal;
	static assert("foo  b\\ \nar\\\nbaz\nquux".split.merge.equal(
			[
				Line(0, "foo  barbaz", null, [6, 8]),
				Line(3, "quux")
			]));
	static assert("foo\\".split.merge.equal([Line(0, "foo")]));
}

/*
3. The source file is decomposed into preprocessing tokens7) and sequences of
white-space characters (including comments). A source file shall not end in a
partial preprocessing token or in a partial comment. Each comment is replaced by
one space character. New-line characters are retained. Whether each nonempty
sequence of white-space characters other than new-line is retained or replaced by
one space character is implementation-defined.
*/
auto tokenize(R)(R input)
	if (isInputRange!R && is(ElementType!R : const(Line)))
{
	static struct Lexer
	{
		R input;
		Token current;
		Location location;
		string line;

		this(R input)
		{
			this.input = input;
			next();
		}

		@property bool empty() const pure nothrow
		{
			return current.kind == TokenKind.eof;
		}

		@property ref const(Token) front() const pure nothrow
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

		private auto multiLineComment()
		{
			auto l = line;
			size_t n = 2;
			for (;;) {
				if (n + 2 <= l.length) {
					if (l[n .. n + 2] == "*/")
						return Tok(TokenKind.space, n + 2);
					n++;
				} else {
					if (!nextLine())
						return Tok(TokenKind.eof, 0);
					l = line;
					n = 0;
				}
			}
		}

		private auto scan()
		{
			auto l = line;
			if (l.length == 0)
				return Tok(TokenKind.newline, 0);
			char c = l[0];

			if (c.isSpace) {
				size_t n = 1;
				while (n < line.length && line[n].isSpace)
					n++;
				return Tok(TokenKind.space, n);
			}

			if (c == '/') {
				if (l.length > 1) {
					char d = l[1];
					if (d == '/')
						return Tok(TokenKind.space, line.length);
					if (d == '*')
						return multiLineComment();
				}
			}

			// TODO: other tokens
			return Tok(TokenKind.reserved, 1);
		}

		private void next()
		{
			uint len = cast(uint) current.spelling.length;
			if (len != 0) {
				line = line[len .. $];
			} else {
				if (!nextLine()) {
					current.kind = TokenKind.eof;
					return;
				}
			}
			auto tok = scan();
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
	static assert(Line[].init.tokenize.empty);
}

unittest
{
	import std.algorithm : equal;
	static assert(
		"  \n // line comment\n   /* multi- // line \n\n\n\n comment */   \n/* short */\n/*ignore unterminated comment"
			.split.merge.tokenize.equal([
				Token(TokenKind.newline, Location("", 0, 0), ""), // merger cuts spaces at the end of line
				Token(TokenKind.space,   Location("", 1, 0), " "),
				Token(TokenKind.space,   Location("", 1, 1), "// line comment"),
				Token(TokenKind.newline, Location("", 1, 16), ""),
				Token(TokenKind.space,   Location("", 2, 0), "   "),
				Token(TokenKind.space,   Location("", 6, 0), " comment */"),
				Token(TokenKind.newline, Location("", 6, 11), ""),
				Token(TokenKind.space,   Location("", 7, 0), "/* short */"),
				Token(TokenKind.newline, Location("", 7, 11), ""),
			]));
}