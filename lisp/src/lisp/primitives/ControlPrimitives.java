
package lisp.primitives;

import lisp.eval.*;

/** Control primitives that don't require special compiler support. */
public class ControlPrimitives extends Definer
{
    @DefineLisp
    public Object sleep (final Object a) throws InterruptedException
    {
	final Object result = null;
	long ms = 0;
	if (a instanceof Integer)
	{
	    ms = 1000 * ((int)a);
	}
	else if (a instanceof Double)
	{
	    ms = Math.round (1000 * (double)a);
	}
	else
	{
	    throw new IllegalArgumentException ("Sleep seconds required " + a);
	}
	Thread.sleep (ms);
	return result;
    }

    @DefineLisp
    public void exit (final int status)
    {
	System.exit (status);
    }

    @DefineLisp
    public void exit ()
    {
	System.exit (0);
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
