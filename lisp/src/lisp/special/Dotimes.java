
package lisp.special;

import java.util.List;

import org.objectweb.asm.Opcodes;

import lisp.Symbol;
import lisp.eval.*;

public class Dotimes extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "dotimes", classname = "lisp.special.DotimesFunction")
    public Object dotimes (final LexicalContext context, final List<?> control, final Object... arguments) throws Exception
    {
	Object result = true;
	final Symbol var = (Symbol)control.get (0);
	final int n = (Integer)context.eval (control.get (1));
	final LexicalContext subcontext = new LexicalContext (context);
	subcontext.bind (var, 0);
	for (int j = 0; j < n; j++)
	{
	    subcontext.set (var, j);
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = subcontext.eval (arg);
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
