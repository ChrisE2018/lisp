
package lisp.special;

import lisp.eval.*;
import lisp.lang.*;

public class Return extends Definer
{
    private static Symbol BLOCK_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("block");

    /**
     * Interpreter for return forms.
     *
     * @param context Lexical binding context.
     * @param arguments Forms to evaluate.
     * @return Value of the last form evaluated.
     * @throws Exception
     * @throws ReturnThrow
     */
    @DefineLisp (special = true, name = "return", classname = "lisp.special.ReturnFunction")
    public Object blockReturn (final LexicalContext context, final Object form) throws Exception, ReturnThrow
    {
	final Symbol key = context.getBlock (BLOCK_SYMBOL);
	if (key == null)
	{
	    throw new IllegalStateException ("There is no enclosing anonymous block to return to");
	}
	throw new ReturnThrow (null, context.eval (form));
    }
}
