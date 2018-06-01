
package lisp.special;

import lisp.eval.*;

public class And extends LogicDefiner
{
    @DefineLisp (special = true, classname = "lisp.special.AndFunction")
    public Object and (final LexicalContext context, final Object... arguments) throws Exception
    {
	Object result = Boolean.TRUE;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = context.eval (arg);
	    if (!isTrue (value))
	    {
		return false;
	    }
	    result = value;
	}
	return result;
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
