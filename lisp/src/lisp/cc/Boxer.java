
package lisp.cc;

import org.objectweb.asm.Type;

public class Boxer
{
    protected static final String CLDESC = "Ljava/lang/Class;";

    protected static final Type BYTE_TYPE = Type.getObjectType ("java/lang/Byte");

    protected static final Type BOOLEAN_TYPE = Type.getObjectType ("java/lang/Boolean");

    protected static final Type SHORT_TYPE = Type.getObjectType ("java/lang/Short");

    protected static final Type CHARACTER_TYPE = Type.getObjectType ("java/lang/Character");

    protected static final Type INTEGER_TYPE = Type.getObjectType ("java/lang/Integer");

    protected static final Type FLOAT_TYPE = Type.getObjectType ("java/lang/Float");

    protected static final Type LONG_TYPE = Type.getObjectType ("java/lang/Long");

    protected static final Type DOUBLE_TYPE = Type.getObjectType ("java/lang/Double");

    protected static final Type NUMBER_TYPE = Type.getObjectType ("java/lang/Number");

    protected static final Type OBJECT_TYPE = Type.getObjectType ("java/lang/Object");

    // protected static final Method BOOLEAN_VALUE = Method.getMethod ("boolean booleanValue()");
    //
    // protected static final Method CHAR_VALUE = Method.getMethod ("char charValue()");
    //
    // protected static final Method INT_VALUE = Method.getMethod ("int intValue()");
    //
    // protected static final Method FLOAT_VALUE = Method.getMethod ("float floatValue()");
    //
    // protected static final Method LONG_VALUE = Method.getMethod ("long longValue()");
    //
    // protected static final Method DOUBLE_VALUE = Method.getMethod ("double doubleValue()");

    protected static Type[][] BOXED_PRIMITIVES =
	{
	 {BYTE_TYPE, Type.BYTE_TYPE},
	 {BOOLEAN_TYPE, Type.BOOLEAN_TYPE},
	 {SHORT_TYPE, Type.SHORT_TYPE},
	 {CHARACTER_TYPE, Type.CHAR_TYPE},
	 {INTEGER_TYPE, Type.INT_TYPE},
	 {FLOAT_TYPE, Type.FLOAT_TYPE},
	 {LONG_TYPE, Type.LONG_TYPE},
	 {DOUBLE_TYPE, Type.DOUBLE_TYPE}};

    protected Type getBoxedType (final Type type)
    {
	for (final Type[] clause : BOXED_PRIMITIVES)
	{
	    if (clause[1] == type)
	    {
		return clause[0];
	    }
	}
	return null;
    }

    protected Type getUnboxedType (final Type type)
    {
	for (final Type[] clause : BOXED_PRIMITIVES)
	{
	    if (clause[0] == type)
	    {
		return clause[1];
	    }
	}
	return null;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
