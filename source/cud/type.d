module cud.type;

import cud.visitor : Visitor;

/// Enum for built-in types
enum Ty
{
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

class Type
{
	import cud.expr : Expr;

	abstract void accept(Visitor);

	override string toString()
	{
		import cud.visitor : tostr = toString;
		return tostr(this);
	}

	abstract @property ulong size() const;

	AtomicType atomic()
	{
		return new AtomicType(this);
	}

	Type qualified(Qualifiers q)
	{
		if (q)
			return new QualifiedType(this, q);
		else
			return this;
	}

	VariableArrayType variableArray(Expr num_elements)
	{
		return new VariableArrayType(this, num_elements);
	}

	ConstantArrayType constantArray(ulong num_elements)
	{
		return new ConstantArrayType(this, num_elements);
	}

	IncompleteArrayType incompleteArray()
	{
		return new IncompleteArrayType(this);
	}
}

// Target-dependent stuff, mostly size and alignment of types.
class Target
{
	mixin({
			string result;
			foreach (ms; __traits(allMembers, Ty)) {
				result ~= "BuiltinType " ~ ms ~ ";";
			}
			return result;
		}());

	BuiltinType size_t_;

	this()
	{
		foreach (ms; __traits(allMembers, Ty)) {
			enum ty = mixin("Ty." ~ ms);
			auto bt = new BuiltinType(ty, typeSizes[ty]);
			mixin("this." ~ ms ~ " = bt;");
		}
		version (D_LP64) {
			size_t_ = this.ulong_;
		} else {
			static assert(0);
		}
	}

	PointerType pointer(Type t)
	{
		return new PointerType(t, pointerSize);
	}

private:
	version (D_LP64) {
		static immutable ulong[Ty.max + 1] typeSizes = [
			Ty.void_ : 1,
			Ty.bool_ : 1,
			Ty.char_ : 1,
			Ty.schar_ : 1,
			Ty.uchar_ : 1,
			Ty.wchar_ : 4,
			Ty.char16_ : 2,
			Ty.char32_ : 4,
			Ty.short_ : 2,
			Ty.sshort_ : 2,
			Ty.ushort_ : 2,
			Ty.int_ : 4,
			Ty.sint_ : 4,
			Ty.uint_ : 4,
			Ty.long_ : 8,
			Ty.slong_ : 8,
			Ty.ulong_ : 8,
			Ty.longlong_ : 8,
			Ty.slonglong_ : 8,
			Ty.ulonglong_ : 8,
			Ty.float_ : 4,
			Ty.double_ : 8,
			Ty.longdouble_ : 16,
			Ty.float_Complex_ : 8,
			Ty.double_Complex_ : 16,
			Ty.longdouble_Complex_ : 32,
		];

		enum ulong pointerSize = 8;
		enum bool charIsSigned = true;
	} else {
		static assert(0, "not implemented");
	}
}

class BuiltinType : Type
{
	Ty ty;
	private ulong m_size;

	private this(Ty ty, ulong size)
	{
		this.ty = ty;
		this.m_size = size;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		return m_size;
	}

	override bool opEquals(Object o)
	{
		if (auto other = cast(BuiltinType) o)
			return other.ty == this.ty;
		return false;
	}
}

class AtomicType : Type
{
	Type underlyingType;

	private this(Type type)
	{
		this.underlyingType = type;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		return this.underlyingType.size;
	}

	override AtomicType atomic()
	{
		return this;
	}

	override bool opEquals(Object o)
	{
		if (auto other = cast(AtomicType) o)
			return other.underlyingType == this.underlyingType;
		return false;
	}
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

	private this(Type type, Qualifiers qualifiers)
	{
		this.underlyingType = type;
		this.qualifiers = qualifiers;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		return this.underlyingType.size;
	}

	override Type qualified(Qualifiers q)
	{
		auto sum = Qualifiers(
			this.qualifiers.const_ || q.const_,
			this.qualifiers.volatile_ || q.volatile_,
			this.qualifiers.restrict_ || q.restrict_);
		if (sum != q)
			return new QualifiedType(this, sum);
		else
			return this;
	}
}

class PointerType : Type
{
	Type pointeeType;
	private ulong m_size;

	private this(Type type, ulong size)
	{
		this.pointeeType = type;
		this.m_size = size;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		return this.m_size;
	}
}

abstract class ArrayType : Type
{
	Type elementType;

	private this(Type element_type)
	{
		this.elementType = element_type;
	}

	override void accept(Visitor visitor) { assert(0); }
}

class IncompleteArrayType : ArrayType
{
	private this(Type element_type)
	{
		super(element_type);
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		assert(0); // TODO
	}
}

class ConstantArrayType : ArrayType
{
	ulong numElements;

	private this(Type element_type, ulong num_elements)
	{
		super(element_type);
		this.numElements = num_elements;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		assert(0); // TODO
	}
}

class VariableArrayType : ArrayType
{
	import cud.expr : Expr;
	Expr numElements;

	private this(Type element_type, Expr num_elements)
	{
		super(element_type);
		this.numElements = num_elements;
	}

	override void accept(Visitor visitor) { visitor.visit(this); }

	override @property ulong size() const
	{
		assert(0); // TODO
	}
}
