
package lisp.special;

import lisp.eval.*;

public class Progn extends Definer
{
    /**
     * Interpreter for progn forms.
     *
     * @param context Lexical binding context.
     * @param arguments Forms to evaluate.
     * @return Value of the last form evaluated.
     * @throws Exception
     */
    @DefineLisp (special = true, classname = "lisp.special.PrognFunction")
    public Object progn (final LexicalContext context, final Object... arguments) throws Exception
    {
	Object result = null;
	for (int i = 0; i < arguments.length; i++)
	{
	    final Object arg = arguments[i];
	    result = context.eval (arg);
	}
	return result;
    }
}
