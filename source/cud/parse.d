/**
CTFEable C expression parser.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.parse;

import std.range : front, popFront, empty;

import cud.token;

version(unittest) {
	import std.algorithm : map, filter, equal;
	import std.range : array;
	import std.exception : assertThrown, assertNotThrown;
	import std.stdio;
	import cud.test;
}

enum UnaryOp
{
	postfixInc,
	postfixDec,
	prefixInc,
	prefixDec,
	addrOf,
	dereference,
	plus,
	minus,
	neg,
	not,
}


class Type
{
}

class BuiltinType(_DType) : Type
{
	alias DType = _DType;
}

class Expression
{
	Location location;
	Type type;
	bool lvalue;

	this(Location location, Type type, bool lvalue)
	{
		this.location = location;
		this.type = type;
		this.lvalue = lvalue;
	}

	abstract void accept(ExpressionVisitor ev);
}

class IntegerConstant : Expression
{
	ulong value;

	this(Location location, Type type, ulong value)
	{
		super(location, type, false);
		this.value = value;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class IndexExpression : Expression
{
/+	this(Location location, Type type, ulong value)
	{
		super(location, type, false);
		this.value = value;
	}+/

	Expression indexed;
	Expression index;

	this(Location location, Expression indexed, Expression index)
	{
		super(location, null, true);
		this.indexed = indexed;
		this.index = index;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class CallExpression : Expression
{
	Expression callee;
	Expression[] args;

	this(Location location, Expression callee, Expression[] args)
	{
		super(location, null, true);
		this.callee = callee;
		this.args = args;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class UnaryExpression : Expression
{
	Expression operand;
	UnaryOp unaryOp;

	this(Location location, UnaryOp op, Expression sube)
	{
		super(location, null, false);
		unaryOp = op;
		operand = sube;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class MemberExpression : Expression
{
	Expression composite;
	string memberName;
	bool pointer;

	this(Location location, Expression composite, string member_name, bool pointer)
	{
		super(location, null, composite.lvalue);
		this.composite = composite;
		this.memberName = member_name;
		this.pointer = pointer;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

interface ExpressionVisitor
{
	void visit(IntegerConstant expr);
	void visit(IndexExpression expr);
	void visit(CallExpression expr);
	void visit(UnaryExpression expr);
	void visit(MemberExpression expr);
}

void print(Expression e)
{
	e.accept(new class ExpressionVisitor
		{
			string indent = "";
			void visit(IntegerConstant expr)
			{
				writefln("%s%s IntegerConstant!%s %s",
					indent, expr.location, expr.type.toString(), expr.value);
			}

			void visit(IndexExpression expr)
			{
				writeln(indent, expr.location, " IndexExpression");
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.indexed.accept(this);
				expr.index.accept(this);
			}

			void visit(CallExpression expr)
			{
				writeln(indent, expr.location, " CallExpression");
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.callee.accept(this);
				foreach (a; expr.args)
					a.accept(this);
			}

			void visit(UnaryExpression expr)
			{
				writeln(indent, expr.location, " UnaryExpression ", expr.unaryOp);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.operand.accept(this);
			}

			void visit(MemberExpression expr)
			{
				writefln("%s%s MemberExpression %s%s",
					indent, expr.location, expr.pointer ? "->" : ".", expr.memberName);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.composite.accept(this);
			}
		});
}

struct Parser {
	import std.meta : Filter;
	enum isParser(string f) = f.length >= 5 && f[0 .. 5] == "parse";
	enum parsers = Filter!(isParser, __traits(allMembers, typeof(this)));

	static struct ExpressionAt
	{
		Expression expr;
		size_t length;
	}

	ExpressionAt[size_t][parsers.length] memo_map;

	Expression memo(string fun)(ref const(Token)[] input)
	{
		import std.meta : staticIndexOf;
		enum i = staticIndexOf!(fun, parsers);
		static assert(i >= 0, "no such parser: " ~ fun);

		if (auto pexpr = input.length in memo_map[i]) {
			input = input[pexpr.length .. $];
			return pexpr.expr;
		}
		size_t length_before = input.length;
		Expression expr = mixin(fun ~ "(input)");
		size_t length_after = input.length;
		memo_map[i][length_before] = ExpressionAt(expr, length_after - length_before);
		return expr;
	}

	Expression parseAssignmentExpression(ref const(Token)[] input)
	{
		return parsePostfixExpression(input);
	}

	/+
	PrimaryExpression < Identifier
	                  / CharLiteral
	                  / StringLiteral
	                  / FloatLiteral
	                  / IntegerLiteral
	                  / '(' Expression ')'
	+/
	Expression parsePrimaryExpression(ref const(Token)[] input)
	{
		const tok = input.front;
		switch (tok.kind) with(TokenKind) {
			case intconstant:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!int, tok.signedInt64Value);
			case uintconstant:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!uint, tok.unsignedInt64Value);
			case longconstant:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!long, tok.signedInt64Value);
			case ulongconstant:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!ulong, tok.unsignedInt64Value);
			case lparen: {
				const(Token)[] temp_input = input;
				temp_input.popFront;
				Expression e = memo!"parseExpression"(temp_input);
				if (!e || temp_input.front.kind != TokenKind.rparen)
					return null;
				temp_input.popFront;
				input = temp_input;
				return e;
			}
			default:
				return null;
		}
	}

	/+
	PostfixExpression < PrimaryExpression ( '[' Expression ']'
		/ '(' ')'
		/ '(' ArgumentExpressionList ')'
		/ '.' Identifier
		/ "->" Identifier
		/ "++"
		/ "--"
		)*

		ArgumentExpressionList < AssignmentExpression (',' AssignmentExpression)*
	+/
	Expression parsePostfixExpression(ref const(Token)[] input)
	{
		const(Token)[] temp_input = input;
		Expression pe = memo!"parsePrimaryExpression"(temp_input);
		if (!pe)
			return null;
		input = temp_input;
		for (;;) {
			Expression outer;
			const tok = temp_input.front;
			switch (tok.kind) with(TokenKind) {
			case lbracket: {
				temp_input.popFront;
				Expression e = memo!"parseExpression"(temp_input);
				if (!e || temp_input.front.kind != rbracket)
					break;
				temp_input.popFront;
				input = temp_input;
				outer = new IndexExpression(tok.location, pe, e);
				break;
			}
			case lparen: {
				temp_input.popFront;
				Expression[] args;
				if (temp_input.front.kind != rparen) {
					for (;;) {
						Expression e = memo!"parseAssignmentExpression"(temp_input);
						if (!e)
							break;
						if (temp_input.front.kind == comma) {
							args ~= e;
							temp_input.popFront;
						} else if (temp_input.front.kind == rparen) {
							args ~= e;
							break;
						} else {
							break;
						}
					}
				}
				if (temp_input.front.kind == rparen) {
					temp_input.popFront;
					input = temp_input;
					outer = new CallExpression(tok.location, pe, args);
				}
				break;
			}
			case ptr:
			case dot: {
				temp_input.popFront;
				if (temp_input.front.kind == identifier) {
					auto ident = temp_input.front.spelling;
					temp_input.popFront;
					input = temp_input;
					outer = new MemberExpression(tok.location, pe, ident, tok.kind == ptr);
				}
				break;
			}
			case plusplus:
			case minusminus:
				temp_input.popFront;
				input = temp_input;
				outer = new UnaryExpression(
					tok.location,
					tok.kind == plusplus
						? UnaryOp.postfixInc
						: UnaryOp.postfixDec,
					pe);
				break;
			default:
			}
			if (!outer)
				return pe;
			pe = outer;
		}
		assert(0);
	}


	Expression parseExpression(ref const(Token)[] input)
	{
		return memo!"parsePostfixExpression"(input);
	}

}

version(unittest)
{
	Expression expr(string source)
	{
		import cud.lexer;
		import std.range : chain, only;
		const(Token)[] tokens =
			source
			.split
			.merge
			.tokenize
			.chain(only(Token(TokenKind.eof)))
			.filter!(t => t.kind != TokenKind.space && t.kind != TokenKind.newline)
			.map!(t => t.ppTokenToToken)
			.array;
		return Parser().parseExpression(tokens);
	}
}

unittest // primaryExpression
{
	crtest!("valid integer constants are parsed with types matching the specification",
		() {
			if (auto e = expr("0").as!IntegerConstant) {
				assert(e.value == 0);
				assert(e.type.as!(BuiltinType!int));
			}
			if (auto e = expr("42l").as!IntegerConstant) {
				assert(e.value == 42);
				assert(e.type.as!(BuiltinType!long));
			}
			if (auto e = expr("0777u").as!IntegerConstant) {
				assert(e.value == 0x1ff);
				assert(e.type.as!(BuiltinType!uint));
			}
			if (auto e = expr("1337ull").as!IntegerConstant) {
				assert(e.value == 1337);
				assert(e.type.as!(BuiltinType!ulong));
			}
		});
	crtest!("parenthesized constant",
		() {
			if (auto e = expr("(42)").as!IntegerConstant) {
				assert(e.value == 42);
				assert(e.type.as!(BuiltinType!int));
			}
		});
}

unittest
{
	print(expr("42()"));
	print(expr("(42).d"));
	print(expr("1()->d"));
	print(expr("42(1, 2, 4)"));
	print(expr("42(1, 2, 4)()"));
	print(expr("42(1, 2, 4)(2)"));
	print(expr("42++"));
	print(expr("42--"));
	print(expr("42[42]"));
	print(expr("42[]"));
}
