
package lisp.primitives;

import lisp.eval.*;

public class IOPrimitives extends Definer
{
    @DefineLisp
    public Object printf (final String format, final Object... arguments)
    {
	System.out.printf (format, arguments);
	return null;
    }
}
