
package lisp.describe;

import lisp.eval.*;

public class DescribePrimitives extends Definer
{
    private final Describe describe = new Describe ();
    private final Inspector inspector = new Inspector ();

    @DefineLisp
    public Object describe (final Object arg)
    {
	System.out.printf ("Describe: %s \n", arg);
	describe.describe (arg);
	return arg;
    }

    @DefineLisp
    public Object inspect (final Object arg)
    {
	System.out.printf ("Inspect: %s \n", arg);
	inspector.inspect (arg);
	return arg;
    }
}
