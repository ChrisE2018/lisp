
package lisp.cc3;

import lisp.*;

public class CompiledShell
{
    // Instance variable to hold the methodName
    private final String methodName = null;

    @SuppressWarnings ("unused")
    private Symbol getSymbol (final String packageName, final String symbolName)
    {
	return PackageFactory.getPackage (packageName).findSymbol (symbolName);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (" ");
	buffer.append (methodName);
	buffer.append (">");
	return buffer.toString ();
    }
}
