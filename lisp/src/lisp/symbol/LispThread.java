
package lisp.symbol;

import java.util.*;

import lisp.*;

public class LispThread extends Thread
{
    private static LispReader defaultLispReader = new LispReader ();

    public static LispReader getLispThreadReader ()
    {
	try
	{
	    final LispThread thread = (LispThread)Thread.currentThread ();
	    return thread.threadReader;
	}
	catch (final ClassCastException e)
	{
	    return defaultLispReader;
	}
    }

    private final LispReader threadReader = new LispReader ();

    public LispThread (final Runnable runnable)
    {
	super (runnable);
    }

    public LispThread ()
    {
	super ();
    }

    public LispReader getThreadReader ()
    {
	return threadReader;
    }

    public Map<Symbol, Object> getThreadLocals ()
    {
	return threadLocals;
    }

    private final Map<Symbol, Object> threadLocals = new HashMap<Symbol, Object> ();

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
