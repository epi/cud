/**
Run the same tests at compile time and run time.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/

module cud.test;

version(unittest):
import std.range : isInputRange, empty, front, popFront;

T as(T, U)(U f) {
	T result = cast(T) f;
	assert(result !is null, () {
			import std.string : format;
			return format("Object of type %s cannot be cast to type %s", typeid(f), T.stringof);
		}());
	return result;
}

void crtest(string description, alias fun)()
{
	version(StaticTest) {
		static assert((){ fun(); return true; }(), "Test failed: " ~ description);
	}

	scope(failure) {
		() @trusted {
			import std.stdio;
			debug stderr.writeln("Test failed: ", description);
		}();
	}
	fun();
}

@safe pure unittest
{
	crtest!("can use crtest in @safe pure unittest blocks", (){});
}

private auto prettify(T)(T a)
{
	import cud.token : TokenKind;
	static if (is(T : const(TokenKind))) {
		import cud.token : tokenStrings;
		return tokenStrings[a];
	} else {
		return a;
	}
}

void assertEqual(R1, R2)(R1 actual, R2 expected)
	if (isInputRange!R1 && isInputRange!R2 && is(typeof(actual.front == expected.front ? 1 : 0)))
{
	import std.string : format;
	import core.exception : AssertError;
	uint n = 0;
	while (!actual.empty && !expected.empty) {
		auto af = actual.front;
		auto ef = expected.front;
		if (af != ef) {
			throw new AssertError(format("Ranges differ at %d: Actual=%s, Expected=%s",
					n, prettify(af), prettify(ef)));
		}
		n++;
		actual.popFront;
		expected.popFront;
	}
	if (actual.empty && expected.empty)
		return;
	if (actual.empty)
		throw new AssertError(format("Range is shorter than expected. Expected[%d]=%s", n, expected.front));
	if (expected.empty)
		throw new AssertError(format("Range is longer than expected. Actual[%d]=%s", n, actual.front));
}
