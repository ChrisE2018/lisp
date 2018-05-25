
package lisp.cc;

import lisp.*;

public class CompiledShell
{
    // Instance variables to hold the methodName methodArgs and methodBody

    @SuppressWarnings ("unused")
    private final String methodName = null;

    @SuppressWarnings ("unused")
    private final LispList methodArgs = null;

    @SuppressWarnings ("unused")
    private final LispList methodBody = null;

    @SuppressWarnings ("unused")
    private Symbol getPublicSymbol (final String packageName, final String symbolName)
    {
	return PackageFactory.getPackage (packageName).findSymbol (symbolName);
    }

    @SuppressWarnings ("unused")
    private boolean isTrue (final Object value)
    {
	// Test for value equal to Boolean true
	if (value != null)
	{
	    if (value instanceof Boolean)
	    {
		return (Boolean)value;
	    }
	}
	return false;
    }

    @SuppressWarnings ("unused")
    private boolean isFalse (final Object value)
    {
	// Test for value equal to Boolean false
	if (value != null)
	{
	    if (value instanceof Boolean)
	    {
		return !(Boolean)value;
	    }
	}
	return false;
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
