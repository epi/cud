/**
Run the same tests at compile time and run time.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/

module cud.test;

version(unittest)
void crtest(string description, alias fun)()
{
	version(StaticTest) {
		static assert((){ fun(); return true; }(), "Test failed: " ~ description);
	}

	try {
		fun();
	} catch (Throwable t) {
		import std.stdio;
		stderr.writeln("Test failed: " ~ description);
		throw t;
	}
}
