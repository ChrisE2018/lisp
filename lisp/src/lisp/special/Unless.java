
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class Unless extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, name = "unless", classname = "lisp.special.UnlessFunction")
    public Object unlessForm (final LexicalContext context, final Object test, final Object... arguments) throws Exception
    {
	if (!isTrue (context.eval (test)))
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
