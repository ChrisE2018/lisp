
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;
import lisp.lang.*;

public class Dotimes extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "dotimes", classname = "lisp.special.DotimesFunction")
    public Object dotimes (final LexicalContext context, final LispList control, final Object... arguments) throws Exception
    {
	final Symbol var = control.head ();
	final int n = (Integer)context.eval (control.get (1));
	final LexicalContext subcontext = new LexicalContext (context);
	subcontext.bind (var, 0);
	for (int j = 0; j < n; j++)
	{
	    subcontext.set (var, j);
	    for (int i = 0; i < arguments.length; i++)
	    {
		subcontext.eval (arguments[i]);
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
