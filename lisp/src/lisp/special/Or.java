
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.*;

public class Or extends LogicDefiner implements Opcodes
{
    @DefineLisp (special = true, classname = "lisp.special.OrFunction")
    public Object or (final LexicalContext context, final Object... arguments) throws Exception
    {
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    final Object value = context.eval (arg);
	    if (isTrue (value))
	    {
		return value;
	    }
	}
	return false;
    }
}
