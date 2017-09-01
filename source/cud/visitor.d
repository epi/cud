module cud.visitor;

import cud.expr;
import cud.type;

abstract class Visitor
{
	void visit(IntConst expr) { assert(0); }
	void visit(IndexExpr expr) { assert(0); }
	void visit(CallExpr expr) { assert(0); }
	void visit(UnaryExpr expr) { assert(0); }
	void visit(MemberExpr expr) { assert(0); }
	void visit(TypeTraitExpr expr) { assert(0); }
	void visit(CastExpr expr) { assert(0); }
	void visit(BinaryExpr expr) { assert(0); }
	void visit(CondExpr expr) { assert(0); }

	void visit(IncompleteArrayType) { assert(0); }
	void visit(ConstantArrayType) { assert(0); }
	void visit(VariableArrayType) { assert(0); }
	void visit(EnumType) { assert(0); }
	// void visit(StructType);
	// void visit(UnionType);
	// void visit(FunctionType);
	void visit(AtomicType) { assert(0); }
	void visit(BuiltinType) { assert(0); }
	void visit(QualifiedType) { assert(0); }
	void visit(PointerType) { assert(0); }
}

private class Printer : Visitor
{
private:
	import std.array : Appender, appender;
	import std.conv : to;
	import std.format : formattedWrite;
	import cud.token : tokenStrings;

	string indent = "";
	Appender!string m_app;

	void put(T...)(T args)
	{
		foreach (a; args)
			m_app.put(a.to!string);
	}

public:
	override void visit(IntConst expr)
	{
		put(indent, expr.location, " IntConst!");
		expr.type.accept(this);
		put(" ", expr.value, "\n");
	}

	override void visit(IndexExpr expr)
	{
		put(indent, expr.location, " IndexExpr\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.indexed.accept(this);
		expr.index.accept(this);
	}

	override void visit(CallExpr expr)
	{
		put(indent, expr.location, " CallExpr\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.callee.accept(this);
		foreach (a; expr.arguments)
			a.accept(this);
	}

	override void visit(UnaryExpr expr)
	{
		put(indent, expr.location, " UnaryExpr ",
			tokenStrings[expr.unaryOp], "\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.operand.accept(this);
	}

	override void visit(MemberExpr expr)
	{
		put(indent, expr.location, " MemberExpr ",
			expr.pointer ? "->" : ".", expr.memberName, "\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.composite.accept(this);
	}

	override void visit(TypeTraitExpr expr)
	{
		put(indent, expr.location, " TypeTraitExpr ",
			expr.trait, " ");
		expr.type.accept(this);
		put("\n");
	}

	override void visit(CastExpr expr)
	{
		put(indent, expr.location, " CastExpr ");
		expr.type.accept(this);
		put("\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.operand.accept(this);
	}

	override void visit(BinaryExpr expr)
	{
		put(indent, expr.location, " BinaryExpr ",
			tokenStrings[expr.binaryOp], "\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.lhs.accept(this);
		expr.rhs.accept(this);
	}

	override void visit(CondExpr expr)
	{
		put(indent, expr.location, " CondExpr\n");
		indent ~= "  ";
		scope(exit) indent = indent[0 .. $ - 2];
		expr.condition.accept(this);
		expr.trueExpr.accept(this);
		expr.falseExpr.accept(this);
	}

	override void visit(AtomicType type)
	{
		put("_Atomic ");
		type.underlyingType.accept(this);
	}

	override void visit(BuiltinType type)
	{
		put(type.ty.to!string[0 .. $ - 1]);
	}

	override void visit(QualifiedType type)
	{
		import std.meta : AliasSeq;
		bool printed_something;
		foreach (q; AliasSeq!("const", "restrict", "volatile")) {
			if (mixin("type.qualifiers." ~ q ~ "_")) {
				put(printed_something ? " " : "", q);
				printed_something = true;
			}
		}
		type.underlyingType.accept(this);
	}

	override void visit(PointerType type)
	{
		put("ptr ");
		type.pointeeType.accept(this);
	}

	override void visit(IncompleteArrayType type)
	{
		put("array[] ");
		type.elementType.accept(this);
	}

	override void visit(ConstantArrayType type)
	{
		put("array[", type.numElements, "] ");
		type.elementType.accept(this);
	}

	override void visit(VariableArrayType type)
	{
		put("array[*] ");
		type.elementType.accept(this);
	}
}

string toString(T)(T obj)
{
	auto p = new Printer;
	obj.accept(p);
	return p.m_app.data;
}
