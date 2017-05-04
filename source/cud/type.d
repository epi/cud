module cud.type;

class Type
{
}

class BuiltinType(_DType) : Type
{
	alias DType = _DType;
}

class AtomicType : Type
{
	Type underlyingType;

	this(Type type)
	{
		this.underlyingType = type;
	}
}
