
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class Repeat extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "repeat", classname = "lisp.special.RepeatFunction")
    public Object repeat (final LexicalContext context, final Object count, final Object... arguments) throws Exception
    {
	final int n = (Integer)context.eval (count);
	for (int j = 0; j < n; j++)
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		context.eval (arguments[i]);
	    }
	}
	return false;
    }
}
