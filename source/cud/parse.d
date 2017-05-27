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
	TokenKind unaryOp;

	this(Location location, TokenKind op, Expression sube)
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
		switch (tok.kind) {
			case tk!`intconst`:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!int, tok.signedInt64Value);
			case tk!`uintconst`:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!uint, tok.unsignedInt64Value);
			case tk!`longconst`:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!long, tok.signedInt64Value);
			case tk!`ulongconst`:
				input.popFront;
				return new IntegerConstant(tok.location, new BuiltinType!ulong, tok.unsignedInt64Value);
			case tk!`(`: {
				const(Token)[] temp_input = input;
				temp_input.popFront;
				Expression e = parseExpression(temp_input);
				if (!e || temp_input.front.kind != tk!`)`)
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
		Expression pe = parsePrimaryExpression(temp_input);
		if (!pe)
			return null;
		input = temp_input;
		for (;;) {
			Expression outer;
			const tok = temp_input.front;
			switch (tok.kind) {
			case tk!`[`: {
				temp_input.popFront;
				Expression e = parseExpression(temp_input);
				if (!e || temp_input.front.kind != tk!`]`)
					break;
				temp_input.popFront;
				input = temp_input;
				outer = new IndexExpression(tok.location, pe, e);
				break;
			}
			case tk!`(`: {
				temp_input.popFront;
				Expression[] args;
				if (temp_input.front.kind != tk!`)`) {
					for (;;) {
						Expression e = parseAssignmentExpression(temp_input);
						if (!e)
							break;
						if (temp_input.front.kind == tk!`,`) {
							args ~= e;
							temp_input.popFront;
						} else if (temp_input.front.kind == tk!`)`) {
							args ~= e;
							break;
						} else {
							break;
						}
					}
				}
				if (temp_input.front.kind == tk!`)`) {
					temp_input.popFront;
					input = temp_input;
					outer = new CallExpression(tok.location, pe, args);
				}
				break;
			}
			case tk!`->`:
			case tk!`.`: {
				temp_input.popFront;
				if (temp_input.front.kind == tk!`identifier`) {
					auto ident = temp_input.front.spelling;
					temp_input.popFront;
					input = temp_input;
					outer = new MemberExpression(
						tok.location, pe, ident, tok.kind == tk!`->`);
				}
				break;
			}
			case tk!`++`:
			case tk!`--`:
				temp_input.popFront;
				input = temp_input;
				outer = new UnaryExpression(
					tok.location,
					tok.kind == tk!`++` ? tk!`post++` : tk!`post--`,
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
		if (auto expr = parsePostfixExpression(temp_input)) {
			input = temp_input;
			return expr;
		}
		const tok = temp_input.front;
		temp_input.popFront;
		switch (tok.kind) {
		foreach (t; tks!`++ -- sizeof`) {
			case t: goto unaryexpr;
		}
		unaryexpr: {
			auto expr = parseUnaryExpression(temp_input);
			if (expr) {
				input = temp_input;
				return new UnaryExpression(tok.location, tok.kind, expr);
			} else if (tok.kind == tk!`sizeof` && temp_input.front.kind == tk!`(`) {
				temp_input.popFront;
				auto type = parseTypeName(temp_input);
				if (!type || temp_input.front.kind != tk!`)`)
					return null;
				temp_input.popFront;
				input = temp_input;
				return new TypeTraitExpression(tok.location, TypeTrait.alignof_, type);
			} else
				return null;
		}
		foreach (t; tks!`- & * + ~ !`) {
			case t: goto castexpr;
		}
		castexpr: {
			auto expr = parseCastExpression(temp_input);
			if (!expr)
				return null;
			input = temp_input;
			return new UnaryExpression(tok.location, tok.kind, expr);
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
		if (auto expr = parseUnaryExpression(temp_input)) {
			input = temp_input;
			return expr;
		}
		const tok = temp_input.front;
		temp_input.popFront;
		if (tok.kind == tk!`(`) {
			auto type = parseTypeName(temp_input);
			if (!type || temp_input.front.kind != tk!`)`)
				return null;
			temp_input.popFront;
			auto expr = parseCastExpression(temp_input);
			if (!expr)
				return null;
			input = temp_input;
			return new CastExpression(tok.location, type, expr);
		}
		return null;
	}

	Expression parseBinaryExpression(string subparser, tokenKinds...)(
		ref const(Token)[] input)
	{
		Expression[] operands;
		TokenKind[] operators;
		Location[] locations;
		const(Token)[] temp_input = input;
		for (;;) {
			auto expr = mixin(subparser ~ "(temp_input)");
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
			tk!`*`, tk!`%`, tk!`/`)(input);
	}

	// AdditiveExpression < MultiplicativeExpression ([-+] AdditiveExpression)*
	Expression parseAdditiveExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseMultiplicativeExpression",
			tk!`+`, tk!`-`)(input);
	}

	//ShiftExpression < AdditiveExpression (("<<" / ">>") ShiftExpression)*
	Expression parseShiftExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseAdditiveExpression",
			tk!`<<`, tk!`>>`)(input);
	}

	// RelationalExpression < ShiftExpression (("<=" / ">=" / "<" / ">") RelationalExpression)*
	Expression parseRelationalExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseShiftExpression",
			tk!`<=`, tk!`>=`, tk!`<`, tk!`>`)(input);
	}

	// EqualityExpression < RelationalExpression (("==" / "!=") EqualityExpression)*
	Expression parseEqualityExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseRelationalExpression",
			tk!`==`, tk!`!=`)(input);
	}

	// ANDExpression < EqualityExpression ('&' ANDExpression)*
	Expression parseAndExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseEqualityExpression", tk!`&`)(input);
	}

	// ExclusiveORExpression < ANDExpression ('^' ExclusiveORExpression)*
	Expression parseExclusiveOrExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseAndExpression", tk!`^`)(input);
	}

	// InclusiveORExpression < ExclusiveORExpression ('|' InclusiveORExpression)*
	Expression parseInclusiveOrExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseExclusiveOrExpression", tk!`|`)(input);
	}

	// LogicalANDExpression < InclusiveORExpression ("&&" LogicalANDExpression)*
	Expression parseLogicalAndExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseInclusiveOrExpression", tk!`&&`)(input);
	}

	// LogicalORExpression < LogicalANDExpression ("||" LogicalORExpression)*
	Expression parseLogicalOrExpression(ref const(Token)[] input)
	{
		return parseBinaryExpression!("parseLogicalAndExpression", tk!`||`)(input);
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
		if (qtok.kind != tk!`?`)
			return condition;
		temp_input.popFront;
		auto trueExpression = parseExpression(temp_input);
		if (!trueExpression)
			return condition;
		if (temp_input.front.kind != tk!`:`)
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

	Expression parseExpression(ref const(Token)[] input)
	{
		return parseConditionalExpression(input);
	}

	// 6.7.2.2
	EnumDeclaration parseEnumSpecifier(ref const(Token)[] input)
	{
		const(Token)[] temp_input = input;
		immutable decl_location = input.front.location;
		temp_input.popFront; // enum

		Token tok = temp_input.front;
		string tag;
		if (tok.kind == tk!`identifier`) {
			tag = tok.spelling;
			temp_input.popFront;
			tok = temp_input.front;
		}
		if (tok.kind != tk!`{`) {
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
			if (id_tok.kind == tk!`}`) {
				temp_input.popFront;
				break;
			}
			if (id_tok.kind != tk!`identifier`) {
				error(tok.location, "expected identifier, not '%s'", id_tok.spelling);
				assert(0);
			}
			string name = id_tok.spelling;
			Expression expr;
			temp_input.popFront;
			tok = temp_input.front;
			if (tok.kind == tk!`=`) {
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
			if (tok.kind == tk!`}`) {
				temp_input.popFront;
				break;
			}
			if (tok.kind != tk!`,`) {
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
			switch (tok.kind) {
				case tk!`const`:
					spec.const_ = tok;
					break;
				case tk!`restrict`:
					spec.restrict_ = tok;
					break;
				case tk!`volatile`:
					spec.volatile_ = tok;
					break;
				case tk!`_Atomic`:
					static if (set == SpecifierSet.typeQualifierList) {
						spec._Atomic_ = tok;
					} else static if (set == SpecifierSet.declarationSpecifiers
						|| set == SpecifierSet.specifierQualifierList)
					{
						if (input[1].kind != tk!`(`) {
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
							if (input.front.kind != tk!`)`) {
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
				case tk!`void`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec.void_ = tok;
					break;
				case tk!`char`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.char_ = tok;
					break;
				case tk!`short`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.short_ = tok;
					break;
				case tk!`int`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.int_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					spec.int_ = tok;
					break;
				case tk!`long`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.long_long_,
						&spec.float_, &spec.double_, &spec._Bool_, &spec._Complex_, &spec.other);
					(spec.long_ ? spec.long_long_ : spec.long_) = tok;
					break;
				case tk!`float`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.long_long_, &spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.float_ = tok;
					break;
				case tk!`double`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_,
						&spec.long_long_, &spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.double_ = tok;
					break;
				case tk!`signed`:
					checkConflicts(tok, &spec.void_, &spec.float_, &spec.double_, &spec.unsigned_,
						&spec._Bool_, &spec.other);
					spec.signed_ = tok;
					break;
				case tk!`unsigned`:
					checkConflicts(tok, &spec.void_, &spec.float_, &spec.double_, &spec.signed_,
						&spec._Bool_, &spec.other);
					spec.unsigned_ = tok;
					break;
				case tk!`_Bool`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec._Bool_ = tok;
					break;
				case tk!`_Complex`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_,
						&spec.long_long_, &spec.signed_, &spec.unsigned_, &spec._Bool_);
					spec._Complex_ = tok;
					break;
				case tk!`enum`:
					checkConflicts(tok, &spec.void_, &spec.char_, &spec.short_, &spec.int_, &spec.long_,
						&spec.float_, &spec.double_, &spec.signed_, &spec.unsigned_,
						&spec._Bool_, &spec._Complex_, &spec.other);
					spec.other = tok;
						auto decl = parseEnumSpecifier(input);
						// type = decl.type;
					continue;
				case tk!`struct`:
				case tk!`union`:
					assert(0);
				}
				static if (set == SpecifierSet.declarationSpecifiers) {
				case tk!`typedef`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.typedef_ = tok;
					break;
				case tk!`extern`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec.auto_, &spec.register_);
					spec.extern_ = tok;
					break;
				case tk!`static`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec.auto_, &spec.register_);
					spec.static_ = tok;
					break;
				case tk!`_Thread_local`:
					checkConflicts(tok, &spec.typedef_, &spec.auto_, &spec.register_);
					spec._Thread_local_ = tok;
					break;
				case tk!`auto`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.auto_ = tok;
					break;
				case tk!`register`:
					checkConflicts(tok, &spec.typedef_, &spec.static_, &spec.extern_,
						&spec._Thread_local_, &spec.auto_, &spec.register_);
					spec.auto_ = tok;
					break;
				case tk!`inline`:
					spec.inline_ = tok;
					break;
				case tk!`_Noreturn`:
					spec._Noreturn_ = tok;
					break;
				case tk!`_Alignas`: {
					spec._Alignas_ = tok;
					input.popFront;
					if (input.front.kind != tk!`(`) {
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
					if (input.front.kind != tk!`)`) {
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

	static Type specifiersToType(S)(ref S spec, bool implicit_int = false)
	{
		if (spec.long_long_) {
			if (spec.signed_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.slonglong_);
			else if (spec.unsigned_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.ulonglong_);
			else
				return new BaseBuiltinType(BaseBuiltinType.Kind.longlong_);
		} else if (spec.long_) {
			if (spec.signed_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.slong_);
			else if (spec.unsigned_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.ulong_);
			else
				return new BaseBuiltinType(BaseBuiltinType.Kind.long_);
		} else if (spec.short_) {
			if (spec.signed_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.sshort_);
			else if (spec.unsigned_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.ushort_);
			else
				return new BaseBuiltinType(BaseBuiltinType.Kind.ulong_);
		} else {
			if (spec.signed_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.sint_);
			else if (spec.unsigned_)
				return new BaseBuiltinType(BaseBuiltinType.Kind.uint_);
			else if (spec.int_ || implicit_int)
				return new BaseBuiltinType(BaseBuiltinType.Kind.int_);
		}
		return null;

	}

	static struct Declarator
	{
		Type type;
		string identifier;
	}

	Type parsePointer(ref const(Token)[] ref_input, Type type)
	{
		while (ref_input.front.kind == tk!`*`) {
			type = new PointerType(type);
			ref_input.popFront;
			auto qualifiers = parseSpecifiers!(SpecifierSet.typeQualifierList)(ref_input);
			if (qualifiers.const_ || qualifiers.restrict_ || qualifiers.volatile_) {
				type = new QualifiedType(type,
					qualifiers.const_    ? Qualifiers.const_ : Qualifiers.none,
					qualifiers.restrict_ ? Qualifiers.restrict_ : Qualifiers.none,
					qualifiers.volatile_ ? Qualifiers.volatile_ : Qualifiers.none);
			}
			if (qualifiers._Atomic_)
				type = new AtomicType(type);
		}
		return type;
	}

	Type parseDeclaratorBrackets(ref const(Token)[] ref_input)
	{
		const tok = ref_input.front;
		switch (tok.kind) {
		case tk!`(`:
			assert(0);
		case tk!`[`:
			assert(0);
		default:
			return null;
		}
	}

	Declarator parseDeclarator(ref const(Token)[] ref_input, Type type = null)
	{
		const(Token)[] paren_decl;
		const(Token)[] input = ref_input;
		string identifier;
		type = parsePointer(input, type);
		const tok = input.front;
		if (tok.kind == tk!`(`) {
			input.popFront;
			paren_decl = input;
			int nest = 1;
			while (nest) {
				switch (input.front.kind) {
				case tk!`(`:
					nest++;
					break;
				case tk!`)`:
					nest--;
					break;
				case tk!`eof`:
				case tk!`;`:
					error(input.front.location, "expected ')'");
					assert(0);
				default:
				}
				input.popFront;
			}
		} else if (tok.kind == tk!`identifier`) {
			identifier = input.front.spelling;
			input.popFront;
		} else {
			error(tok.location, "expected identifier");
			assert(0);
		}

		if (auto t = parseDeclaratorBrackets(input))
			type = t;

		if (paren_decl) {
			auto result = parseDeclarator(paren_decl, type);
			if (paren_decl.front.kind != tk!`)`) {
				error(paren_decl.front.location,
					"expected ')' after declarator, not '%s'",
					paren_decl.front.spelling);
				assert(0);
			}
			type = result.type;
			identifier = result.identifier;
		}

		ref_input = input;
		return Declarator(type, identifier);
	}

	/+
	type-name:
		specifier-qualifier-list abstract-declarator?

	abstract-declarator:
		pointer
		pointer? direct-abstract-declarator

	direct-abstract-declarator:
		( abstract-declarator )
		direct-abstract-declarator? [ type-qualifier-list? assignment-expression? ]
		direct-abstract-declarator? [ static type-qualifier-list? assignment-expression ]
		direct-abstract-declarator? [ type-qualifier-list static assignment-expression ]
		direct-abstract-declarator? [ * ]
		direct-abstract-declarator? ( parameter-type-list opt )
	+/
	Type parseTypeName(ref const(Token)[] ref_input)
	{
		const(Token)[] input = ref_input;
		auto specifiers = parseSpecifiers!(SpecifierSet.specifierQualifierList)(input);

		ref_input = input;
		return specifiersToType(specifiers);
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
			.chain(only(Token(tk!`eof`)))
			.filter!(t => t.kind != tk!`space` && t.kind != tk!`newline`)
			.map!(t => t.ppTokenToToken)
			.array;
		auto result = mixin("Parser().parse" ~ what ~ "(tokens)");
		assertEqual(
			tokens.map!(t => t.kind),
			chain(remainder, only(tk!`eof`)));
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
			assert(e.unaryOp == tk!`post++`);
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
			assert(e.binaryOp == tk!`+`);
			assert(e.rhs.as!IntegerConstant.value == 6);
			auto add = e.lhs.as!BinaryExpression;
			assert(add.binaryOp == tk!`+`);
			assert(add.lhs.as!IntegerConstant.value == 1);
			auto div = add.rhs.as!BinaryExpression;
			assert(div.binaryOp == tk!`/`);
			assert(div.rhs.as!IntegerConstant.value == 5);
			auto rem = div.lhs.as!BinaryExpression;
			assert(rem.binaryOp == tk!`%`);
			assert(rem.rhs.as!IntegerConstant.value == 4);
			auto mul = rem.lhs.as!BinaryExpression;
			assert(mul.binaryOp == tk!`*`);
			assert(mul.rhs.as!IntegerConstant.value == 3);
			assert(mul.lhs.as!IntegerConstant.value == 2);
		});

	crtest!("prefix operators can be nested",
		() {
			auto c = expr("(int)--*&1").as!CastExpression;
			auto mm = c.operand.as!UnaryExpression;
			assert(mm.unaryOp == tk!`--`);
			auto ind = mm.operand.as!UnaryExpression;
			assert(ind.unaryOp == tk!`*`);
			auto ad = ind.operand.as!UnaryExpression;
			assert(ad.unaryOp == tk!`&`);
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
			assert(ce.binaryOp == tk!`||`);
			assert(ce.lhs.as!IntegerConstant.value == 1);
			assert(ce.rhs.as!IntegerConstant.value == 2);
			auto te = e.trueExpression.as!BinaryExpression;
			assert(te.binaryOp == tk!`||`);
			assert(te.lhs.as!IntegerConstant.value == 3);
			assert(te.rhs.as!IntegerConstant.value == 4);
			auto fe = e.falseExpression.as!BinaryExpression;
			assert(fe.binaryOp == tk!`||`);
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
			auto e = parse!"EnumSpecifier"(`enum foo;`, tk!`;`).as!EnumDeclaration;
			assert(e.tag == "foo");
			assert(e.list.length == 0);
		});

	crtest!("enum with one constant with default value",
		() {
			foreach (src; [`enum foo { bar };`, `enum foo { bar, };`]) {
				auto e = parse!"EnumSpecifier"(src, tk!`;`)
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
					tk!`;`)
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
				"const volatile restrict int", tk!`int`);
			assert(tq.const_);
			assert(tq.volatile_);
			assert(!tq._Atomic_);
			assert(tq.restrict_);

			tq = parse!"Specifiers!(Parser.SpecifierSet.typeQualifierList)"(
				"_Atomic restrict typedef", tk!`typedef`);
			assert(!tq.const_);
			assert(!tq.volatile_);
			assert(tq._Atomic_);
			assert(tq.restrict_);
		});

	crtest!("parse declaration-specifiers",
		() {
			auto sq = parse!"Specifiers!(Parser.SpecifierSet.declarationSpecifiers)"(
				"extern _Thread_local unsigned _Alignas(16) const _Alignas(int) *",
				tk!`*`);
			assert(sq.extern_);
			assert(sq._Thread_local_);
			assert(sq._Alignas_);
			assert(sq.const_);
			assert(sq.alignment.length == 2);
			assert(sq.alignment[0].as!IntegerConstant.value == 16);
			auto tt = sq.alignment[1].as!TypeTraitExpression;
			assert(tt.trait == TypeTrait.alignof_);
			assert(tt.theType.as!BaseBuiltinType.kind == BaseBuiltinType.kind.int_);
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
				"void typedef *", tk!`*`);
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

unittest
{
	crtest!("parse declarators with pointers and qualifiers",
		() {
			auto decl = parse!"Declarator"(`foo`);
			assert(decl.identifier == "foo");

			decl = parse!"Declarator"(`*bar`);
			assert(decl.identifier == "bar");
			assert(decl.type.toString == "ptr(0)");

			decl = parse!"Declarator"(`*(bar)`);
			assert(decl.identifier == "bar");
			assert(decl.type.toString == "ptr(0)");

			decl = parse!"Declarator"(`*const baz`);
			assert(decl.identifier == "baz");
			assert(decl.type.toString == "const(ptr(0))");

			decl = parse!"Declarator"(`* const _Atomic * restrict ** volatile * const restrict volatile quux`);
			assert(decl.identifier == "quux");
			assert(decl.type.toString ==
				"const restrict volatile(ptr(volatile(ptr(ptr(restrict(ptr(_Atomic(const(ptr(0))))))))))");
		});

	crtest!("malformed declarator",
		() {
			foreach (src; [
					"* const",
					"*(",
					"((",
					"((dar)",
					"*(int)",
					"()",
					"*()",
					"*())",
					"(foo bar)",
					"(*foo bar)",
				])
			{
				assertThrown(parse!"Declarator"(src), src);
			}
		});
}

unittest
{
	crtest!("parse type name",
		() {
			foreach (src, type_str; [
					"int"           : "int",
				/+	"int *"         : "ptr(int)",
					"int *[3]"      : "array[3](ptr(int))",
					"int (*)[3]"    : "ptr(array[3](int))",
					"int (*)[*]"    : "ptr(vlarray[?](int))",
					"int *()"       : "func(?:ptr(int))",
					"int (*)(void)" : "func(:int)",
					"int (*const [])(unsigned int, ...)"
					                : "array[?](const(ptr(func(uint,...:int))))"+/
				])
			{
				import std.conv : text;
				assert(parse!"TypeName"(src).toString == type_str, text(src, " !=> ", type_str));
			}
		});
}