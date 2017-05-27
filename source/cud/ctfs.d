/**
Compile-time file-system for cud preprocessor.

See_Also: `cud.preprocessor.isFS`

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/

module cud.ctfs;

private template importOne(string name)
{
	static if (is(typeof(import(name)) : string))
		enum importOne = import(name);
	else
		enum importOne = "";
}

private template importAll(string[] names)
{
	import std.meta : staticMap, aliasSeqOf;
	enum importAll = [ staticMap!(importOne, aliasSeqOf!names) ];
}

// This does BFS over the tree of included header names
// and returns a list of included headers ordered by depth of first inclusion
private template collectHeaderNames(string[] names, uint num_scanned)
{
	import std.algorithm : filter, map, joiner, sort, uniq, canFind;
	import std.range : array;
	import std.typecons : tuple;
	import cud.token;
	import cud.lexer : split, merge, tokenize;

	enum includedHeaderNames =
		importAll!(names[num_scanned .. $])
			.map!(src => src.split.merge.tokenize
				.filter!(token => token.kind == tk!`headername` && token.spelling.length >= 3)
				.map!(token => token.spelling[1 .. $ - 1]))
			.joiner
			.filter!(h => !names.canFind(h))
			.array
			.sort()
			.uniq
			.array;

	static if (includedHeaderNames.length) {
		enum collectHeaderNames = .collectHeaderNames!(
			names ~ includedHeaderNames,
			names.length);
	} else {
		enum collectHeaderNames = names;
	}
}

/**
Evaluates to an associative array containing contents of the specified file
and all files included from it.
*/
template collectHeaders(string filename)
{
	import std.algorithm : filter;
	import std.array : assocArray;
	import std.range : zip;
	enum headerNames = collectHeaderNames!([filename], 0);
	enum headerContents = importAll!headerNames;
	enum collectHeaders =
		zip(headerNames, headerContents)
			.filter!(p => p[1].length)
			.assocArray;
}

///
unittest
{
	import cud.preprocessor : isFS;
	import cud.ctfs;

	static assert(isFS!(typeof(collectHeaders!"")));
}
