
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class When extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "when", classname = "lisp.special.WhenFunction")
    public Object whenForm (final LexicalContext context, final Object test, final Object... arguments) throws Exception
    {
	if (isTrue (context.eval (test)))
	{
	    Object result = true;
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = context.eval (arg);
	    }
	    return result;
	}
	return false;
    }
}
