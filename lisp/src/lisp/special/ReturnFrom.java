
package lisp.special;

import lisp.eval.*;
import lisp.lang.Symbol;

public class ReturnFrom extends Definer
{
    /**
     * Interpreter for progn forms.
     *
     * @param context Lexical binding context.
     * @param arguments Forms to evaluate.
     * @return Value of the last form evaluated.
     * @throws Exception
     * @throws ReturnThrow
     */
    @DefineLisp (special = true, name = "return-from", classname = "lisp.special.ReturnFromFunction")
    public Object blockReturnFrom (final LexicalContext context, final Symbol name, final Object form)
            throws Exception, ReturnThrow
    {
	final Object result = context.eval (form);
	throw new ReturnThrow (name, result);
    }
}
