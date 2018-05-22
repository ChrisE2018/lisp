
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
	if (value != null)
	{
	    if (value instanceof Boolean)
	    {
		if (false == (Boolean)value)
		{
		    return false;
		}
	    }
	    return true;
	}
	return false;
    }

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
