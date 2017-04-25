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
	postDecrement,
	postIncrement,
	preDecrement,
	preIncrement,
	address,
	indirection,
	plus,
	minus,
	complement,
	not,
	sizeof_,
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
	Expression[] arguments;

	this(Location location, Expression callee, Expression[] args)
	{
		super(location, null, true);
		this.callee = callee;
		this.arguments = args;
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

class SizeofExpression : Expression
{
	Type type;

	this(Location location, Type type)
	{
		super(location, new BuiltinType!int, false);
		this.type = type;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class CastExpression : Expression
{
	Expression operand;

	this(Location location, Type type, Expression operand)
	{
		super(location, type, false);
		this.operand = operand;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class BinaryExpression : Expression
{
	Expression lhs;
	Expression rhs;
	TokenKind binaryOp;

	this(Location location, TokenKind op, Expression lhs, Expression rhs)
	{
		super(location, null, false);
		this.binaryOp = op;
		this.lhs = lhs;
		this.rhs = rhs;
	}

	override void accept(ExpressionVisitor ev) { ev.visit(this); }
}

class ConditionalExpression : Expression
{
	Expression condition;
	Expression trueExpression;
	Expression falseExpression;

	this(Location location,
		Expression condition,
		Expression trueExpression,
		Expression falseExpression)
	{
		super(location, null, false);
		this.condition = condition;
		this.trueExpression = trueExpression;
		this.falseExpression = falseExpression;
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
	void visit(SizeofExpression expr);
	void visit(CastExpression expr);
	void visit(BinaryExpression expr);
	void visit(ConditionalExpression expr);
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
				foreach (a; expr.arguments)
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

			void visit(SizeofExpression expr)
			{
				writefln("%s%s SizeofExpression %s",
					indent, expr.location, typeid(expr.type));
			}

			void visit(CastExpression expr)
			{
				writefln("%s%s CastExpression %s",
					indent, expr.location, typeid(expr.type));
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.operand.accept(this);
			}

			void visit(BinaryExpression expr)
			{
				writefln("%s%s BinaryExpression %s",
					indent, expr.location, expr.binaryOp);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.lhs.accept(this);
				expr.rhs.accept(this);
			}

			void visit(ConditionalExpression expr)
			{
				writefln("%s%s ConditionalExpression",
					indent, expr.location);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.condition.accept(this);
				expr.trueExpression.accept(this);
				expr.falseExpression.accept(this);
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

	auto memo(string fun)(ref const(Token)[] input)
	{
/+		import std.meta : staticIndexOf;
		enum i = staticIndexOf!(fun, parsers);
		static assert(i >= 0, "no such parser: " ~ fun);

		if (auto pexpr = input.length in memo_map[i]) {
			input = input[pexpr.length .. $];
			return pexpr.expr;
		}+/
		size_t length_before = input.length;
		//writefln("trying %s on %d tokens", fun, length_before);
		auto expr = mixin(fun ~ "(input)");
		size_t length_after = input.length;
		if (length_after < length_before) {
		//	writefln("%s consumed %d tokens", fun, length_before - length_after);
		}
		//memo_map[i][length_before] = ExpressionAt(expr, length_after - length_before);
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
						? UnaryOp.postIncrement
						: UnaryOp.postDecrement,
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

	/+
	UnaryExpression < PostfixExpression
		/ IncrementExpression
		/ DecrementExpression
		/ UnaryOperator CastExpression
		/ "sizeof" UnaryExpression
		/ "sizeof" '(' TypeName ')'

	IncrementExpression < PlusPlus UnaryExpression

	PlusPlus <- "++"

	DecrementExpression < "--" UnaryExpression

	UnaryOperator <- [-&*+~!]
	+/
	Expression parseUnaryExpression(ref const(Token)[] input)
	{
		const(Token)[] temp_input = input;
		if (auto expr = memo!"parsePostfixExpression"(temp_input)) {
			input = temp_input;
			return expr;
		}
		const tok = temp_input.front;
		temp_input.popFront;
		UnaryOp op;
		switch (tok.kind) with(TokenKind) {
		case plusplus:
			op = UnaryOp.preIncrement; goto unaryexpr;
		case minusminus:
			op = UnaryOp.preDecrement; goto unaryexpr;
		case sizeof_:
			op = UnaryOp.sizeof_; goto unaryexpr;
		unaryexpr: {
			auto expr = memo!"parseUnaryExpression"(temp_input);
			if (expr) {
				input = temp_input;
				return new UnaryExpression(tok.location, op, expr);
			} else if (op == UnaryOp.sizeof_ && temp_input.front.kind == lparen) {
				temp_input.popFront;
				auto type = memo!"parseTypeName"(temp_input);
				if (!type || temp_input.front.kind != rparen)
					return null;
				temp_input.popFront;
				input = temp_input;
				return new SizeofExpression(tok.location, type);
			} else
				return null;
		}
		case minus:
			op = UnaryOp.minus; goto castexpr;
		case and:
			op = UnaryOp.address; goto castexpr;
		case mul:
			op = UnaryOp.indirection; goto castexpr;
		case plus:
			op = UnaryOp.plus; goto castexpr;
		case tilde:
			op = UnaryOp.complement; goto castexpr;
		case not:
			op = UnaryOp.not; goto castexpr;
		castexpr: {
			auto expr = memo!"parseCastExpression"(temp_input);
			if (!expr)
				return null;
			input = temp_input;
			return new UnaryExpression(tok.location, op, expr);
		}
		default:
			return null;
		}
	}

	/+
	CastExpression < UnaryExpression
		/ '(' TypeName ')' CastExpression
	+/
	Expression parseCastExpression(ref const(Token)[] input)
	{
		const(Token)[] temp_input = input;
		if (auto expr = memo!"parseUnaryExpression"(temp_input)) {
			input = temp_input;
			return expr;
		}
		const tok = temp_input.front;
		temp_input.popFront;
		if (tok.kind == TokenKind.lparen) {
			auto type = memo!"parseTypeName"(temp_input);
			if (!type || temp_input.front.kind != TokenKind.rparen)
				return null;
			temp_input.popFront;
			auto expr = memo!"parseCastExpression"(temp_input);
			if (!expr)
				return null;
			input = temp_input;
			return new CastExpression(tok.location, type, expr);
		}
		return null;
	}

	Expression parseBinaryExpression(string subparser, tokenKinds...)(ref const(Token)[] input)
	{
		Expression[] operands;
		TokenKind[] operators;
		Location[] locations;
		const(Token)[] temp_input = input;
		for (;;) {
			auto expr = memo!subparser(temp_input);
			if (!expr)
				break;
			operands ~= expr;
			input = temp_input;
			const tok = temp_input.front;
			temp_input.popFront;
			foreach (tk; tokenKinds) {
				if (tk == tok.kind) {
					locations ~= tok.location;
					operators ~= tk;
					goto next;
				}
			}
			break;
		next:
		}
		if (!operands.length)
			return null;
		Expression result = operands[0];
		operands.popFront;
		while (operands.length) {
			result = new BinaryExpression(locations[0], operators[0], result, operands[0]);
			locations.popFront;
			operands.popFront;
			operators.popFront;
		}
		return result;
	}

	// MultiplicativeExpression < CastExpression ([*%/] MultiplicativeExpression)*
	Expression parseMultiplicativeExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseCastExpression",
			TokenKind.mul, TokenKind.mod, TokenKind.div)(input);
	}

	// AdditiveExpression < MultiplicativeExpression ([-+] AdditiveExpression)*
	Expression parseAdditiveExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseMultiplicativeExpression",
			TokenKind.plus, TokenKind.minus)(input);
	}

	//ShiftExpression < AdditiveExpression (("<<" / ">>") ShiftExpression)*
	Expression parseShiftExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseAdditiveExpression",
			TokenKind.shl, TokenKind.shr)(input);
	}

	// RelationalExpression < ShiftExpression (("<=" / ">=" / "<" / ">") RelationalExpression)*
	Expression parseRelationalExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseShiftExpression",
			TokenKind.le, TokenKind.ge, TokenKind.lt, TokenKind.gt)(input);
	}

	// EqualityExpression < RelationalExpression (("==" / "!=") EqualityExpression)*
	Expression parseEqualityExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseRelationalExpression",
			TokenKind.equal, TokenKind.notequal)(input);
	}

	// ANDExpression < EqualityExpression ('&' ANDExpression)*
	Expression parseAndExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseEqualityExpression", TokenKind.and)(input);
	}

	// ExclusiveORExpression < ANDExpression ('^' ExclusiveORExpression)*
	Expression parseExclusiveOrExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseAndExpression", TokenKind.xor)(input);
	}

	// InclusiveORExpression < ExclusiveORExpression ('|' InclusiveORExpression)*
	Expression parseInclusiveOrExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseExclusiveOrExpression", TokenKind.or)(input);
	}

	// LogicalANDExpression < InclusiveORExpression ("&&" LogicalANDExpression)*
	Expression parseLogicalAndExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseInclusiveOrExpression", TokenKind.andand)(input);
	}

	// LogicalORExpression < LogicalANDExpression ("||" LogicalORExpression)*
	Expression parseLogicalOrExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseLogicalAndExpression", TokenKind.oror)(input);
	}

	// ConditionalExpression < LogicalORExpression ('?' Expression ':' ConditionalExpression)?
	Expression parseConditionalExpression(ref const(Token)[] input)
	{
		const(Token)[] temp_input = input;
		auto condition = parseLogicalOrExpression(temp_input);
		if (!condition)
			return null;
		input = temp_input;
		const qtok = temp_input.front;
		if (qtok.kind != TokenKind.question)
			return condition;
		temp_input.popFront;
		auto trueExpression = parseExpression(temp_input);
		if (!trueExpression)
			return condition;
		if (temp_input.front.kind != TokenKind.colon)
			return condition;
		temp_input.popFront;
		auto falseExpression = parseConditionalExpression(temp_input);
		if (!falseExpression)
			return condition;
		input = temp_input;
		return new ConditionalExpression(
			qtok.location, condition, trueExpression, falseExpression);
	}

	// TODO:
	/+
	AssignmentExpression < UnaryExpression AssignmentOperator AssignmentExpression
		/ ConditionalExpression
	+/

	/+
	AssignmentOperator <- "=" / "*=" / "/=" / "%=" / "+=" / "-=" / "<<=" / ">>=" / "&=" / "^=" / "|="
	+/

	/+
	Expression < AssignmentExpression (',' AssignmentExpression)*
	+/

	/+
	ConstantExpression <- ConditionalExpression
	+/

	//TODO:
	Type parseTypeName(ref const(Token)[] input)
	{
		switch (input.front.kind) with(TokenKind) {
		case int_:
			input.popFront;
			return new BuiltinType!int;
		default:
			return null;
		}
	}

	Expression parseExpression(ref const(Token)[] input)
	{
		return memo!"parseConditionalExpression"(input);
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
	crtest!("can nest postfix operators",
		() {
			auto e = expr("1[42]()(2, 3)->a.b++").as!UnaryExpression;
			assert(e.unaryOp == UnaryOp.postIncrement);
			auto e1 = e.operand.as!MemberExpression;
			assert(e1.memberName == "b");
			assert(!e1.pointer);
			auto c0 = e1.composite.as!MemberExpression;
			assert(c0.memberName == "a");
			assert(c0.pointer);
			auto c1 = c0.composite.as!CallExpression;
			assert(c1.arguments.length == 2);
			assert(c1.arguments[0].as!IntegerConstant.value == 2);
			assert(c1.arguments[1].as!IntegerConstant.value == 3);
			auto c2 = c1.callee.as!CallExpression;
			assert(c2.arguments.length == 0);
			auto ind = c2.callee.as!IndexExpression;
			assert(ind.indexed.as!IntegerConstant.value == 1);
			assert(ind.index.as!IntegerConstant.value == 42);
		});

	crtest!("binary operators are left-associative",
		() {
			auto e = expr("1 + 2 * 3 % 4 / 5 + 6").as!BinaryExpression;
			assert(e.binaryOp == TokenKind.plus);
			assert(e.rhs.as!IntegerConstant.value == 6);
			auto add = e.lhs.as!BinaryExpression;
			assert(add.binaryOp == TokenKind.plus);
			assert(add.lhs.as!IntegerConstant.value == 1);
			auto div = add.rhs.as!BinaryExpression;
			assert(div.binaryOp == TokenKind.div);
			assert(div.rhs.as!IntegerConstant.value == 5);
			auto rem = div.lhs.as!BinaryExpression;
			assert(rem.binaryOp == TokenKind.mod);
			assert(rem.rhs.as!IntegerConstant.value == 4);
			auto mul = rem.lhs.as!BinaryExpression;
			assert(mul.binaryOp == TokenKind.mul);
			assert(mul.rhs.as!IntegerConstant.value == 3);
			assert(mul.lhs.as!IntegerConstant.value == 2);
		});

	crtest!("prefix operators can be nested",
		() {
			auto c = expr("(int)--*&1").as!CastExpression;
			auto mm = c.operand.as!UnaryExpression;
			assert(mm.unaryOp == UnaryOp.preDecrement);
			auto ind = mm.operand.as!UnaryExpression;
			assert(ind.unaryOp == UnaryOp.indirection);
			auto ad = ind.operand.as!UnaryExpression;
			assert(ad.unaryOp == UnaryOp.address);
			assert(ad.operand.as!IntegerConstant.value == 1);
		});

	crtest!("?: is parsed alone",
		() {
			auto e = expr("1 ? 2 : 3").as!ConditionalExpression;
			assert(e.condition.as!IntegerConstant.value == 1);
			assert(e.trueExpression.as!IntegerConstant.value == 2);
			assert(e.falseExpression.as!IntegerConstant.value == 3);
		});

	crtest!("?: has lower priority than ||",
		() {
			auto e = expr("1 || 2 ? 3 || 4 : 5 || 6").as!ConditionalExpression;
			auto ce = e.condition.as!BinaryExpression;
			assert(ce.binaryOp == TokenKind.oror);
			assert(ce.lhs.as!IntegerConstant.value == 1);
			assert(ce.rhs.as!IntegerConstant.value == 2);
			auto te = e.trueExpression.as!BinaryExpression;
			assert(te.binaryOp == TokenKind.oror);
			assert(te.lhs.as!IntegerConstant.value == 3);
			assert(te.rhs.as!IntegerConstant.value == 4);
			auto fe = e.falseExpression.as!BinaryExpression;
			assert(fe.binaryOp == TokenKind.oror);
			assert(fe.lhs.as!IntegerConstant.value == 5);
			assert(fe.rhs.as!IntegerConstant.value == 6);
		});

	crtest!("?: is right-associative",
		() {
			auto e = expr("1 ? 2 : 3 ? 4 : 5 ? 6 : 7").as!ConditionalExpression;
			assert(e.condition.as!IntegerConstant.value == 1);
			assert(e.trueExpression.as!IntegerConstant.value == 2);
			auto e1 = e.falseExpression.as!ConditionalExpression;
			assert(e1.condition.as!IntegerConstant.value == 3);
			assert(e1.trueExpression.as!IntegerConstant.value == 4);
			auto e2 = e1.falseExpression.as!ConditionalExpression;
			assert(e2.condition.as!IntegerConstant.value == 5);
			assert(e2.trueExpression.as!IntegerConstant.value == 6);
			assert(e2.falseExpression.as!IntegerConstant.value == 7);
		});
}
