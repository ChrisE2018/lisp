
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class While extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "while", classname = "lisp.special.WhileFunction")
    public Object whileForm (final LexicalContext context, final Object test, final Object... arguments) throws Exception
    {
	while (isTrue (context.eval (test)))
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		context.eval (arguments[i]);
	    }
	}
	return false;
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
