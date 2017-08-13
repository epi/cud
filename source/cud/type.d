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

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
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

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
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

abstract class ArrayType : Type
{
	Type elementType;

	this(Type element_type)
	{
		this.elementType = element_type;
	}

	override void accept(TypeVisitor visitor) { import std.stdio; writeln("A??A? ", typeid(this)); }
}

class IncompleteArrayType : ArrayType
{
	this(Type element_type)
	{
		super(element_type);
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

class ConstantArrayType : ArrayType
{
	ulong size;

	this(Type element_type, ulong size)
	{
		super(element_type);
		this.size = size;
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

class VariableArrayType : ArrayType
{
	import cud.parse : Expression;
	Expression size;

	this(Type element_type, Expression size)
	{
		super(element_type);
		this.size = size;
	}

	override void accept(TypeVisitor visitor) { visitor.visit(this); }
}

interface TypeVisitor
{
	void visit(IncompleteArrayType);
	void visit(ConstantArrayType);
	void visit(VariableArrayType);
	// void visit(StructType);
	// void visit(UnionType);
	// void visit(FunctionType);
	void visit(AtomicType);
	void visit(BuiltinType);
	void visit(QualifiedType);
	void visit(PointerType);
}

void format(Type t, scope void delegate(in char[]) dg)
{
	if (!t) {
		dg("(null)");
		return;
	}
	t.accept(new class TypeVisitor
		{
			void visit(AtomicType type)
			{
				dg("_Atomic ");
				format(type.underlyingType, dg);
			}

			void visit(BuiltinType type)
			{
				import std.conv : to;
				dg(type.kind.to!string[0 .. $ - 1]);
			}

			void visit(QualifiedType type)
			{
				import std.meta : AliasSeq;
				bool printed_something;
				foreach (q; AliasSeq!("const", "restrict", "volatile")) {
					if (mixin("type.qualifiers." ~ q ~ "_")) {
						if (printed_something)
							dg(" ");
						dg(q);
						printed_something = true;
					}
				}
				format(type.underlyingType, dg);
			}

			void visit(PointerType type)
			{
				dg("ptr ");
				format(type.pointeeType, dg);
			}

			void visit(IncompleteArrayType type)
			{
				dg("array[] ");
				format(type.elementType, dg);
			}

			void visit(ConstantArrayType type)
			{
				dg("array[3] ");
				format(type.elementType, dg);
			}

			void visit(VariableArrayType type)
			{
				dg("array[*] ");
				format(type.elementType, dg);
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
