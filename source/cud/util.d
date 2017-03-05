module cud.util;

enum bool isInputRange(R) = is(typeof(
(inout int = 0)
{
	R r = R.init;     // can define a range object
	if (r.empty) {}   // can test for empty
	r.popFront;       // can invoke popFront()
	auto h = r.front; // can get the front of the range
}));

template ElementType(R)
{
    static if (is(typeof(R.init.front.init) T))
        alias ElementType = T;
    else
        alias ElementType = void;
}
