
package lisp.special;

import java.lang.reflect.InvocationTargetException;

import lisp.eval.*;
import lisp.lang.*;

public class Block extends Definer
{
    private static Symbol BLOCK_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("block");

    /**
     * Interpreter for block forms.
     *
     * @param context Lexical binding context.
     * @param arguments Forms to evaluate.
     * @return Value of the last form evaluated.
     * @throws Exception
     */
    @DefineLisp (special = true, name = "block", classname = "lisp.special.BlockFunction")
    public Object block (final LexicalContext context, final Object... arguments) throws Exception
    {
	// (block (return 6))
	final LexicalContext nestedContext = new LexicalContext (context);
	final Symbol key = BLOCK_SYMBOL.gensym ();
	nestedContext.addBlock (BLOCK_SYMBOL, key);
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
