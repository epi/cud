module cud.type;

class Type
{
	abstract void accept(TypeVisitor);

	override string toString()
	{
		import std.array : appender;
		import std.stdio : writeln;
		auto app = appender!string;
		format(this, &app.put!(const(char[])));
		return app.data;
	}
}

class BaseBuiltinType : Type
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

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

deprecated
class BuiltinType(_DType) : BaseBuiltinType
{
	alias DType = _DType;

	this()
	{
		super(mixin("BaseBuiltinType.Kind." ~ _DType.stringof ~ "_"));
	}
}

class AtomicType : Type
{
	Type underlyingType;

	this(Type type)
	{
		this.underlyingType = type;
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

enum Qualifiers : uint
{
	none                   = 0,
	volatile_              = 1 << 0,
	restrict_              = 1 << 1,
	restrictVolatile_      = restrict_ | volatile_,
	const_                 = 1 << 2,
	constVolatile_         = const_ | volatile_,
	constRestrict_         = const_ | restrict_,
	constRestrictVolatile_ = const_ | restrict_ | volatile_,
}

class QualifiedType : Type
{
	Type underlyingType;
	uint qualifiers;

	this(Type type, Qualifiers[] qualifiers...)
	{
		foreach (q; qualifiers)
			this.qualifiers |= q;
		this.underlyingType = type;
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

class PointerType : Type
{
	Type pointeeType;

	this(Type type)
	{
		this.pointeeType = type;
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

class ArrayType : Type
{
	import cud.parse : Expression;
	Type elementType;
	Expression size;

	this(Type element_type, Expression size)
	{
		this.elementType = element_type;
		this.size = size;
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

interface TypeVisitor
{
	void visit(ArrayType);
	// void visit(StructType);
	// void visit(UnionType);
	// void visit(FunctionType);
	void visit(AtomicType);
	void visit(BaseBuiltinType);
	void visit(QualifiedType);
	void visit(PointerType);
}

void format(Type t, scope void delegate(in char[]) dg)
{
	if (!t) {
		dg("0");
		return;
	}
	t.accept(new class TypeVisitor
		{
			void visit(AtomicType type)
			{
				dg("_Atomic(");
				format(type.underlyingType, dg);
				dg(")");
			}

			void visit(BaseBuiltinType type)
			{
				import std.conv : to;
				dg(type.kind.to!string[0 .. $ - 1]);
			}

			void visit(QualifiedType type)
			{
				import std.meta : AliasSeq;
				bool printed_something;
				foreach (q; AliasSeq!("const", "restrict", "volatile")) {
					if (type.qualifiers & mixin("Qualifiers." ~ q ~ "_")) {
						if (printed_something)
							dg(" ");
						dg(q);
						printed_something = true;
					}
				}
				dg("(");
				format(type.underlyingType, dg);
				dg(")");
			}

			void visit(PointerType type)
			{
				dg("ptr(");
				format(type.pointeeType, dg);
				dg(")");
			}

			void visit(ArrayType type)
			{
				dg("array[");
				if (type.size)
					dg("3");
				else
					dg("*");
				dg("](");
				format(type.elementType, dg);
				dg(")");
			}
		});
}

void print(Type t)
{
	import std.array : appender;
	import std.stdio : writeln;
	auto app = appender!string;
	format(t, &app.put!(const(char[])));
	writeln(app.data);
}
