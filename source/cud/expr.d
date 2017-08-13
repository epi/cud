module cud.expr;

import cud.token;
import cud.type;

class Expr
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

deprecated alias Expression = Expr;

class IntegerConstant : Expr
{
	long value;

	this(Location location, Type type, long value)
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
		//FIXME: don't create new BuiltinType here
		super(location, new BuiltinType(BuiltinType.Kind.int_), false);
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
	import std.stdio;
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
