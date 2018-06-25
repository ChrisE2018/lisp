
package lisp.special;

import java.lang.reflect.InvocationTargetException;

import lisp.eval.*;
import lisp.lang.Symbol;

public class BlockNamed extends Definer
{
    /**
     * Interpreter for block-named forms.
     *
     * @param context Lexical binding context.
     * @param arguments Forms to evaluate.
     * @return Value of the last form evaluated.
     * @throws Exception
     */
    @DefineLisp (special = true, name = "block-named", classname = "lisp.special.BlockNamedFunction")
    public Object blockNamed (final LexicalContext context, final Symbol name, final Object... arguments) throws Exception
    {
	// (block-named foo (return-from foo 6) 9)
	// (block-named foo (block-named bar (return-from foo 6) 7) 8) => 6
	// (block-named foo (block-named bar (return-from bar 6) 7) 8) => 8
	final LexicalContext nestedContext = new LexicalContext (context);

	final Symbol key = name.gensym ();
	nestedContext.addBlock (name, key);
	Object result = null;
	try
	{
	    for (int i = 0; i < arguments.length; i++)
	    {
		final Object arg = arguments[i];
		result = nestedContext.eval (arg);
	    }
	}
	catch (final InvocationTargetException e)
	{
	    Throwable cause = e.getCause ();
	    while (cause instanceof InvocationTargetException)
	    {
		cause = cause.getCause ();
	    }
	    if (cause instanceof ReturnThrow)
	    {
		final ReturnThrow rt = (ReturnThrow)cause;
		if (rt.getName () == key)
		{
		    result = rt.getValue ();
		}
		else
		{
		    throw e;
		}
	    }
	}
	return result;
    }
}
