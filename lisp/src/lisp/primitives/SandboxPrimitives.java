
package lisp.primitives;

import lisp.eval.*;

/** Experimental lisp primitives. */
public class SandboxPrimitives extends Definer
{
    @DefineLisp
    public int rr (final int a, final int b)
    {
	return a % b;
    }

    @DefineLisp
    public Object incr (final Integer a)
    {
	return a + 1;
    }

    @DefineLisp
    public Object iincr (final int a)
    {
	return a + 1;
    }
}
