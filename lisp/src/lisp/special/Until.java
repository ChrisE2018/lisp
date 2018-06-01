
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class Until extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "until", classname = "lisp.special.UntilFunction")
    public Object untilForm (final LexicalContext context, final Object test, final Object... arguments) throws Exception
    {
	Object result = true;
	while (!isTrue (context.eval (test)))
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = context.eval (arg);
	    }
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
