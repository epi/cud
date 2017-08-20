module cud.type;

import cud.visitor : Visitor;

class Type
{
	abstract void accept(Visitor);

	override string toString()
	{
		import cud.visitor : tostr = toString;
		return tostr(this);
	}
}

struct BuiltinTypes
{
	mixin({
			string result;
			foreach (ms; __traits(allMembers, BuiltinType.Kind)) {
				result ~= "BuiltinType " ~ ms ~ ";";
			}
			return result;
		}());

	void initialize()
	{
		foreach (ms; __traits(allMembers, BuiltinType.Kind)) {
			auto bt = new BuiltinType(mixin("BuiltinType.Kind." ~ ms));
			mixin("this." ~ ms ~ " = bt;");
		}
	}
}

class BuiltinType : Type
{
	enum Kind {
		void_,
		bool_,
		char_,
		schar_,
		uchar_,
		wchar_,
		char16_,
		char32_,
		short_,
		sshort_,
		ushort_,
		int_,
		sint_,
		uint_,
		long_,
		slong_,
		ulong_,
		longlong_,
		slonglong_,
		ulonglong_,
		float_,
		double_,
		longdouble_,
		float_Complex_,
		double_Complex_,
		longdouble_Complex_,
	}

	Kind kind;

	this(Kind kind)
	{
		this.kind = kind;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}

enum ty(string t) = mixin("BuiltinType.Kind." ~ t ~ "_");

static assert(ty!`longdouble` == BuiltinType.Kind.longdouble_);

class AtomicType : Type
{
	Type underlyingType;

	this(Type type)
	{
		this.underlyingType = type;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}

struct FunctionSpecifiers
{
	bool inline_;
	bool _Noreturn_;
}

struct StorageClass
{
	bool typedef_;
	bool extern_;
	bool static_;
	bool _Thread_local_;
	bool auto_;
	bool register_;
}

struct Qualifiers
{
	bool const_;
	bool volatile_;
	bool restrict_;

	@property bool any() pure nothrow const @safe @nogc
	{
		return const_ | volatile_ | restrict_;
	}

	alias any this; // implicit conversion to bool
}

class QualifiedType : Type
{
	Type underlyingType;
	Qualifiers qualifiers;

	this(Type type, Qualifiers qualifiers)
	{
		this.underlyingType = type;
		this.qualifiers = qualifiers;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}

class PointerType : Type
{
	Type pointeeType;

	this(Type type)
	{
		this.pointeeType = type;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}

abstract class ArrayType : Type
{
	Type elementType;

	this(Type element_type)
	{
		this.elementType = element_type;
	}

	override void accept(Visitor visitor) { import std.stdio; writeln("A??A? ", typeid(this)); }
}

class IncompleteArrayType : ArrayType
{
	this(Type element_type)
	{
		super(element_type);
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}

class ConstantArrayType : ArrayType
{
	ulong size;

	this(Type element_type, ulong size)
	{
		super(element_type);
		this.size = size;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}

class VariableArrayType : ArrayType
{
	import cud.expr : Expr;
	Expr size;

	this(Type element_type, Expr size)
	{
		super(element_type);
		this.size = size;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }
}
