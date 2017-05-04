/**
CTFEable C expression parser.

Authors: $(LINK2 https://github.com/epi, Adrian Matoga)
Copyright: © 2017 Adrian Matoga
License: $(LINK2 http://www.boost.org/users/license.html, BSL-1.0).
*/
module cud.parse;

import std.range : front, popFront, empty;

import cud.token;
import cud.type;

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

enum TypeTrait
{
	sizeof_,
	alignof_,
}

class TypeTraitExpression : Expression
{
	TypeTrait trait;
	Type theType;

	this(Location location, TypeTrait trait, Type type)
	{
		super(location, new BuiltinType!int, false);
		this.trait = trait;
		this.theType = type;
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
	void visit(TypeTraitExpression expr);
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

			void visit(TypeTraitExpression expr)
			{
				writefln("%s%s TypeTraitExpression %s %s",
					indent, expr.location, expr.trait, typeid(expr.type));
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

class Declaration
{
	Location location;

	this(Location location)
	{
		this.location = location;
	}
}

class EnumConstantDeclaration : Declaration
{
	string name;
	Expression expression;

	this(Location location, string name, Expression expression)
	{
		super(location);
		this.name = name;
		this.expression = expression;
	}
}

class EnumDeclaration : Declaration
{
	string tag;
	EnumConstantDeclaration[] list;

	this(Location location, string tag, EnumConstantDeclaration[] list)
	{
		super(location);
		this.tag = tag;
		this.list = list;
	}
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
				return new TypeTraitExpression(tok.location, TypeTrait.alignof_, type);
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

	// 6.7.2.2
	EnumDeclaration parseEnumSpecifier(ref const(Token)[] input)
	{
		const(Token)[] temp_input = input;
		immutable decl_location = input.front.location;
		temp_input.popFront; // enum

		Token tok = temp_input.front;
		string tag;
		if (tok.kind == TokenKind.identifier) {
			tag = tok.spelling;
			temp_input.popFront;
			tok = temp_input.front;
		}
		if (tok.kind != TokenKind.lcurly) {
			if (!tag) {
				error(tok.location, "found '%s' when expecting identifier or '{' after 'enum'",
					tok.spelling);
				assert(0);
			}
			input = temp_input;
			return new EnumDeclaration(decl_location, tag, null);
		}
		temp_input.popFront; // lcurly

		EnumConstantDeclaration[] list;
		for (;;) {
			const id_tok = temp_input.front;
			if (id_tok.kind == TokenKind.rcurly) {
				temp_input.popFront;
				break;
			}
			if (id_tok.kind != TokenKind.identifier) {
				error(tok.location, "expected identifier, not '%s'", id_tok.spelling);
				assert(0);
			}
			string name = id_tok.spelling;
			Expression expr;
			temp_input.popFront;
			tok = temp_input.front;
			if (tok.kind == TokenKind.assign) {
				temp_input.popFront;
				expr = parseConditionalExpression(temp_input);
				if (!expr) {
					error(temp_input.front.location,
						"expected expression, not '%s'",
						temp_input.front.spelling);
					assert(0);
				}
				// TODO: evaluate expr
				tok = temp_input.front;
			}
			list ~= new EnumConstantDeclaration(id_tok.location, name, expr);
			if (tok.kind == TokenKind.rcurly) {
				temp_input.popFront;
				break;
			}
			if (tok.kind != TokenKind.comma) {
				error(tok.location, "expected ',', not '%s'", tok.spelling);
				assert(0);
			}
			temp_input.popFront;
		}
		if (list.length == 0) {
			error(decl_location, "empty enum declaration");
			assert(0);
		}
		input = temp_input;
		return new EnumDeclaration(decl_location, tag, list);
	}

	enum SpecifierSet
	{
		declarationSpecifiers,
		specifierQualifierList,
		typeQualifierList,
	}

	// 6.7. Declarations
	// declaration-specifiers <
	//  ( storage-class-specifier / type-specifier / type-qualifier / function-specifier / alignment-specifier )+
	//
	// 6.7.2.1. Structure and union specifiers
	// 6.7.7. Type names
	// specifier-qualifier-list <
	//  ( type-specifier / type-qualifier )
	//
	// 6.7.6 Declarators
	// type-qualifier-list <
	//  type-qualifier+
	auto parseSpecifiers(SpecifierSet set)(ref const(Token)[] ref_input)
	{
		struct Specifiers
		{
			// 6.7.3 Type qualifiers
			Token const_;
			Token restrict_;
			Token volatile_;
			Token _Atomic_;

			// 6.7.2 Type specifiers
			static if (set == SpecifierSet.specifierQualifierList || set == SpecifierSet.declarationSpecifiers) {
				Token void_;
				Token char_;
				Token short_;
				Token int_;
				Token long_;
				Token long_long_;
				Token float_;
				Token double_;
				Token signed_;
				Token unsigned_;
				Token _Bool_;
				Token _Complex_;
				Token other;
				Type type; // typedef, struct, union, enum, or atomic
			}

			// 6.7.1 Storage class specifiers
			// 6.7.4 Function specifiers
			// 6.7.5 Alignment specifier
			static if (set == SpecifierSet.declarationSpecifiers) {
				Token typedef_;
				Token extern_;
				Token static_;
				Token _Thread_local_;
				Token auto_;
				Token register_;

				Token inline_;
				Token _Noreturn_;

				Token _Alignas_;
				Expression[] alignment;
			}
		}

		Specifiers spec;

		void checkConflicts(Token checked_token, Token*[] conflicting_specifiers...)
		{
			foreach (pspec; conflicting_specifiers) {
				if (*pspec) {
					error(checked_token.location,
						"cannot combine '%s' with previous specifier '%s' at %s",
						checked_token.spelling, pspec.spelling, pspec.location);
					assert(0);
				}
			}
		}

		const(Token)[] input = ref_input;
		for (;;) {
			const tok = input.front;
			switch (tok.kind) with(TokenKind) {
				case const_:
					spec.const_ = tok;
					break;
				case restrict_:
					spec.restrict_ = tok;
					break;
				case volatile_:
					spec.volatile_ = tok;
					break;
				case _Atomic_:
					static if (set == SpecifierSet.typeQualifierList) {
						spec._Atomic_ = tok;
					} else static if (set == SpecifierSet.declarationSpecifiers
						|| set == SpecifierSet.specifierQualifierList)
					{
						if (input[1].kind != lparen) {
							spec._Atomic_ = tok;
						} else {
							checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
								&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
								&spec._Bool_, &spec._Complex_, &spec.other);
							input = input[2 .. $];
							spec.other = tok;
							spec.type = new AtomicType(
								() {
									if (auto t = parseTypeName(input))
										return t;
									error(input.front.location,
										"found '%s' when expecting type name in atomic type specifier",
										input.front.spelling);
									assert(0);
								}());
							if (input.front.kind != rparen) {
								error(input.front.location,
									"found '%s' when expecting ')' closing atomic type specifier",
									input.front.spelling);
								assert(0);
							}
						}
					} else {
						static assert(0);
					}
					break;
				static if (set == SpecifierSet.specifierQualifierList
					|| set == SpecifierSet.declarationSpecifiers)
				{
				case void_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec.void_ = tok;
					break;
				case char_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.char_ = tok;
					break;
				case short_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.short_ = tok;
					break;
				case int_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.int_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.int_ = tok;
					break;
				case long_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.long_long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					(spec.long_ ? spec.long_long_ : spec.long_) = tok;
					break;
				case float_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.long_long_, &spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.float_ = tok;
					break;
				case double_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_,
						&spec.long_long_, &spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.double_ = tok;
					break;
				case signed_:
					checkConflicts(tok, &spec.void_, &spec.float_, &spec.double_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.signed_ = tok;
					break;
				case unsigned_:
					checkConflicts(tok, &spec.void_, &spec.float_, &spec.double_, &spec.signed_,
						&spec._Bool_, &spec.other);
					spec.unsigned_ = tok;
					break;
				case _Bool_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec._Bool_ = tok;
					break;
				case _Complex_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_,
						&spec.long_long_, &spec.signed_, &spec.unsigned_, &spec._Bool_);
					spec._Complex_ = tok;
					break;
				case enum_:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec.other = tok;
						auto decl = parseEnumSpecifier(input);
						// type = decl.type;
					continue;
				case struct_:
				case union_:
					assert(0);
				}
				static if (set == SpecifierSet.declarationSpecifiers) {
				case typedef_:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.typedef_ = tok;
					break;
				case extern_:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec.auto_, &spec.register_);
					spec.extern_ = tok;
					break;
				case static_:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec.auto_, &spec.register_);
					spec.static_ = tok;
					break;
				case _Thread_local_:
					checkConflicts(tok, &spec.typedef_, &spec.auto_, &spec.register_);
					spec._Thread_local_ = tok;
					break;
				case auto_:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.auto_ = tok;
					break;
				case register_:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.auto_ = tok;
					break;
				case inline_:
					spec.inline_ = tok;
					break;
				case _Noreturn_:
					spec._Noreturn_ = tok;
					break;
				case _Alignas_: {
					spec._Alignas_ = tok;
					input.popFront;
					if (input.front.kind != lparen) {
						error(input.front.location, "expected '(' after _Alignas, not '%s'",
							input.front.spelling);
						assert(0);
					}
					input.popFront;
					Expression expr = parseConditionalExpression(input);
					if (!expr) {
						Type type = parseTypeName(input);
						if (!type) {
							error(input.front.location,
								"expected constant expression or type name");
							assert(0);
						}
						expr = new TypeTraitExpression(tok.location, TypeTrait.alignof_, type);
					}
					spec.alignment ~= expr;
					if (input.front.kind != rparen) {
						error(input.front.location, "expected ')'");
						assert(0);
					}
					break;
				}
				}
				default:
					ref_input = input;
					return spec;
			}
			input.popFront;
		}
	}

}

version(unittest)
{
	Expression expr(string source)
	{
		return parse!"Expression"(source);
	}

	auto parse(string what)(string source, TokenKind[] remainder...)
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
		auto result = mixin("Parser().parse" ~ what ~ "(tokens)");
		assertEqual(
			tokens.map!(t => t.kind),
			chain(remainder, only(TokenKind.eof)));
		return result;
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

unittest
{
	crtest!("enum without enumerator list",
		() {
			auto e = parse!"EnumSpecifier"(`enum foo;`, TokenKind.semicolon).as!EnumDeclaration;
			assert(e.tag == "foo");
			assert(e.list.length == 0);
		});

	crtest!("enum with one constant with default value",
		() {
			foreach (src; [`enum foo { bar };`, `enum foo { bar, };`]) {
				auto e = parse!"EnumSpecifier"(src, TokenKind.semicolon)
					.as!EnumDeclaration;
				assert(e.tag == "foo");
				assert(e.list.length == 1);
				assert(e.list[0].name == "bar");
				assert(e.list[0].expression is null);
			}
		});

	crtest!("enum with multiple constants",
		() {
			auto e =
				parse!"EnumSpecifier"(
					`enum foo { bar, baz = 13, quux = 42, };`,
					TokenKind.semicolon)
				.as!EnumDeclaration;
			assert(e.tag == "foo");
			assert(e.list.length == 3);
			assert(e.list[0].name == "bar");
			assert(e.list[0].expression is null);
			assert(e.list[1].name == "baz");
			assert(e.list[1].expression.as!IntegerConstant.value == 13);
			assert(e.list[2].name == "quux");
			assert(e.list[2].expression.as!IntegerConstant.value == 42);
		});

	crtest!("malformed enum spec",
		() {
			foreach (src; [
					"enum",
					"enum {}",
					"enum { a",
					"enum {,}",
					"enum a {}",
					"enum a {b",
					"enum a {b,",
					"enum a {,}",
					"enum a {b=}",
					"enum a {b=,}",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});
}

unittest
{
	crtest!("parse type qualifiers",
		() {
			auto tq = parse!"Specifiers!(Parser.SpecifierSet.typeQualifierList)"(
				"const volatile restrict int", TokenKind.int_);
			assert(tq.const_);
			assert(tq.volatile_);
			assert(!tq._Atomic_);
			assert(tq.restrict_);

			tq = parse!"Specifiers!(Parser.SpecifierSet.typeQualifierList)"(
				"_Atomic restrict typedef", TokenKind.typedef_);
			assert(!tq.const_);
			assert(!tq.volatile_);
			assert(tq._Atomic_);
			assert(tq.restrict_);
		});

	crtest!("parse declaration-specifiers",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"extern _Thread_local unsigned _Alignas(16) const _Alignas(int) *",
				TokenKind.mul);
			assert(sq.extern_);
			assert(sq._Thread_local_);
			assert(sq._Alignas_);
			assert(sq.const_);
			assert(sq.alignment.length == 2);
			assert(sq.alignment[0].as!IntegerConstant.value == 16);
			auto tt = sq.alignment[1].as!TypeTraitExpression;
			assert(tt.trait == TypeTrait.alignof_);
			assert(tt.theType.as!(BuiltinType!int));
			assert(!sq.restrict_);
			assert(!sq.volatile_);
			assert(!sq._Atomic_);
		});

	crtest!("parse _Atomic(type)",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"const _Atomic(int)");
			assert(!sq.extern_);
			assert(!sq._Thread_local_);
			assert(!sq._Alignas_);
			assert(sq.const_);
			assert(!sq.alignment);
			assert(!sq.restrict_);
			assert(!sq.volatile_);
			assert(!sq._Atomic_);
		});

	crtest!("parse _Atomic _Atomic(type)",
		() {
			auto ds = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"extern const _Atomic _Atomic(int)");
			assert(ds.extern_);
			assert(ds.const_);
			assert(!ds.restrict_);
			assert(!ds.volatile_);
			assert(ds._Atomic_);
		});

	crtest!("parse specifier-qualifier-list",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.specifierQualifierList)"(
				"const int long unsigned long volatile restrict _Atomic");
			assert(sq.const_);
			assert(sq.int_);
			assert(sq.long_);
			assert(sq.long_long_);
			assert(sq.unsigned_);
			assert(sq.volatile_);
			assert(sq.restrict_);
			assert(sq._Atomic_);
			assert(!sq.other);
			assert(!sq.short_);
			assert(!sq.signed_);
		});

	crtest!("void combines with typedef",
		() {
			auto ds = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"void typedef *", TokenKind.mul);
			assert(ds.void_);
			assert(ds.typedef_);
		});

	crtest!("error if conflicting type specifiers are given",
		() {
			foreach (src; [
					"void int",
					"int void",
					"long _Atomic(int)",
					"_Atomic(int) long",
					"_Atomic(int) unsigned",
					"_Atomic(int) int",
					"float double",
					"double float",
					"long double int",
					"long int double",
					"double int long",
					"long float",
					"_Bool long",
					"_Bool int",
					"int _Bool",
					"short char",
					"short double",
					"long short",
					"short long",
					"char int",
					"unsigned signed",
					"signed unsigned",
					"_Complex int",
					"int _Complex",
					"enum { a } short",
					"signed enum { a }",
					"static typedef",
					"static static",
					"static extern",
					"extern typedef",
					"extern static",
					"extern extern",
					"typedef typedef",
					"typedef static",
					"typedef extern",
					"auto typedef",
					"auto static",
					"auto extern",
					"auto _Thread_local",
					"auto auto",
					"auto register",
					"register typedef",
					"register static",
					"register extern",
					"register _Thread_local",
					"register auto",
					"register register",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});

	crtest!("malformed _Atomic()",
		() {
			foreach (src; [
					"_Atomic(",
					"_Atomic(",
					"_Atomic( foo)",
					"_Atomic(,)",
					"_Atomic(int",
					"_Atomic(,",
					"_Atomic()",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});

	crtest!("malformed _Alignas",
		() {
			foreach (src; [
					"_Alignas int",
					"_Alignas *",
					"_Alignas foo",
					"_Alignas (",
					"_Alignas()",
					"_Alignas(foo)",
					"_Alignas(int",
					"_Alignas(int foo",
					"_Alignas(2",
					"_Alignas(2 foo",
				])
			{
				assertThrown(parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(src), src);
			}
		});
}
