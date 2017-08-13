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

	abstract void accept(ExprVisitor ev);
}

class IntConst : Expr
{
	long value;

	this(Location location, Type type, long value)
	{
		super(location, type, false);
		this.value = value;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class IndexExpr : Expr
{
	Expr indexed;
	Expr index;

	this(Location location, Expr indexed, Expr index)
	{
		super(location, null, true);
		this.indexed = indexed;
		this.index = index;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class CallExpr : Expr
{
	Expr callee;
	Expr[] arguments;

	this(Location location, Expr callee, Expr[] args)
	{
		super(location, null, true);
		this.callee = callee;
		this.arguments = args;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class UnaryExpr : Expr
{
	Expr operand;
	TokenKind unaryOp;

	this(Location location, TokenKind op, Expr sube)
	{
		super(location, null, false);
		unaryOp = op;
		operand = sube;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class MemberExpr : Expr
{
	Expr composite;
	string memberName;
	bool pointer;

	this(Location location, Expr composite, string member_name, bool pointer)
	{
		super(location, null, composite.lvalue);
		this.composite = composite;
		this.memberName = member_name;
		this.pointer = pointer;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

enum TypeTrait
{
	sizeof_,
	alignof_,
}

class TypeTraitExpr : Expr
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

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class CastExpr : Expr
{
	Expr operand;

	this(Location location, Type type, Expr operand)
	{
		super(location, type, false);
		this.operand = operand;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class BinaryExpr : Expr
{
	Expr lhs;
	Expr rhs;
	TokenKind binaryOp;

	this(Location location, TokenKind op, Expr lhs, Expr rhs)
	{
		super(location, null, false);
		this.binaryOp = op;
		this.lhs = lhs;
		this.rhs = rhs;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

class CondExpr : Expr
{
	Expr condition;
	Expr trueExpr;
	Expr falseExpr;

	this(Location location,
		Expr condition,
		Expr trueExpr,
		Expr falseExpr)
	{
		super(location, null, false);
		this.condition = condition;
		this.trueExpr = trueExpr;
		this.falseExpr = falseExpr;
	}

	override void accept(ExprVisitor ev) { ev.visit(this); }
}

interface ExprVisitor
{
	void visit(IntConst expr);
	void visit(IndexExpr expr);
	void visit(CallExpr expr);
	void visit(UnaryExpr expr);
	void visit(MemberExpr expr);
	void visit(TypeTraitExpr expr);
	void visit(CastExpr expr);
	void visit(BinaryExpr expr);
	void visit(CondExpr expr);
}

void print(Expr e)
{
	import std.stdio;
	e.accept(new class ExprVisitor
		{
			string indent = "";
			void visit(IntConst expr)
			{
				writefln("%s%s IntegerConstant!%s %s",
					indent, expr.location, expr.type.toString(), expr.value);
			}

			void visit(IndexExpr expr)
			{
				writeln(indent, expr.location, " IndexExpr");
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.indexed.accept(this);
				expr.index.accept(this);
			}

			void visit(CallExpr expr)
			{
				writeln(indent, expr.location, " CallExpr");
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.callee.accept(this);
				foreach (a; expr.arguments)
					a.accept(this);
			}

			void visit(UnaryExpr expr)
			{
				writeln(indent, expr.location, " UnaryExpr ", expr.unaryOp);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.operand.accept(this);
			}

			void visit(MemberExpr expr)
			{
				writefln("%s%s MemberExpr %s%s",
					indent, expr.location, expr.pointer ? "->" : ".", expr.memberName);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.composite.accept(this);
			}

			void visit(TypeTraitExpr expr)
			{
				writefln("%s%s TypeTraitExpr %s %s",
					indent, expr.location, expr.trait, typeid(expr.type));
			}

			void visit(CastExpr expr)
			{
				writefln("%s%s CastExpr %s",
					indent, expr.location, typeid(expr.type));
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.operand.accept(this);
			}

			void visit(BinaryExpr expr)
			{
				writefln("%s%s BinaryExpr %s",
					indent, expr.location, expr.binaryOp);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.lhs.accept(this);
				expr.rhs.accept(this);
			}

			void visit(CondExpr expr)
			{
				writefln("%s%s CondExpr",
					indent, expr.location);
				indent ~= "  ";
				scope(exit) indent = indent[0 .. $ - 2];
				expr.condition.accept(this);
				expr.trueExpr.accept(this);
				expr.falseExpr.accept(this);
			}
		});
}
