
package lisp.special;

import lisp.eval.*;

public class Return extends Definer
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
    @DefineLisp (special = true, name = "return", classname = "lisp.special.ReturnFunction")
    public Object blockReturn (final LexicalContext context, final Object form) throws Exception, ReturnThrow
    {
	throw new ReturnThrow (null, context.eval (form));
    }
}
