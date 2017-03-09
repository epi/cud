module cud.preprocessor;

import std.range : isInputRange, ElementType;

struct Line
{
	uint num;
	string content;
	string file;
	immutable(size_t)[] mergedOffsets;
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
			return current.content == null;
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
	static assert("foo\nbar\rbaz\r\nquux".split.equal(
			[
				Line(0, "foo"),
				Line(1, "bar"),
				Line(2, "baz"),
				Line(3, "quux")
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
				while (l.length && (l[$ - 1] == ' ' || l[$ - 1] == '\t'))
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
