
package lisp.eval;

/** Experimental lisp primitives. */
public class SandboxPrimitives extends Definer
{
    @DefineLisp
    public int rr (final int a, final int b)
    {
	return a % b;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}