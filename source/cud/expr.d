module cud.expr;

import cud.token;
import cud.type;
import cud.visitor : Visitor;

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

	abstract void accept(Visitor ev);

	override string toString()
	{
		import cud.visitor : tostr = toString;
		return tostr(this);
	}
}

class IntConst : Expr
{
	long value;

	this(Location location, Type type, long value)
	{
		super(location, type, false);
		this.value = value;
	}

	override void accept(Visitor ev) { ev.visit(this); }
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

	override void accept(Visitor ev) { ev.visit(this); }
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

	override void accept(Visitor ev) { ev.visit(this); }
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

	override void accept(Visitor ev) { ev.visit(this); }
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

	override void accept(Visitor ev) { ev.visit(this); }
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

	this(Location location, Type result_type, TypeTrait trait, Type type)
	{
		super(location, result_type, false);
		this.trait = trait;
		this.theType = type;
	}

	override void accept(Visitor ev) { ev.visit(this); }
}

class CastExpr : Expr
{
	Expr operand;

	this(Location location, Type type, Expr operand)
	{
		super(location, type, false);
		this.operand = operand;
	}

	override void accept(Visitor ev) { ev.visit(this); }
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

	override void accept(Visitor ev) { ev.visit(this); }
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

	override void accept(Visitor ev) { ev.visit(this); }
}
