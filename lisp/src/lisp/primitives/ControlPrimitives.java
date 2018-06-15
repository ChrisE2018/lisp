
package lisp.primitives;

import java.lang.reflect.Constructor;

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

    @DefineLisp (name = "throw")
    public void throwFunction (final Class<? extends Throwable> exception, final String format, final Object... args)
            throws Throwable
    {
	final String message = String.format (format, args);
	final Constructor<? extends Throwable> c = exception.getConstructor (String.class);
	throw c.newInstance (message);
    }

    @DefineLisp (name = "throw")
    public void throwFunction (final Class<? extends Throwable> exception, final String message) throws Throwable
    {
	final Constructor<? extends Throwable> c = exception.getConstructor (String.class);
	throw c.newInstance (message);
    }

    @DefineLisp (name = "rethrow")
    public void rethrowFunction (final Throwable cause, final Class<? extends Throwable> exception, final String format,
            final Object... args) throws Throwable
    {
	final String message = String.format (format, args);
	final Constructor<? extends Throwable> c = exception.getConstructor (String.class, Throwable.class);
	throw c.newInstance (message, cause);
    }

    @DefineLisp (name = "rethrow")
    public void rethrowFunction (final Throwable cause, final Class<? extends Throwable> exception, final String message)
            throws Throwable
    {
	final Constructor<? extends Throwable> c = exception.getConstructor (String.class, Throwable.class);
	throw c.newInstance (message, cause);
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
