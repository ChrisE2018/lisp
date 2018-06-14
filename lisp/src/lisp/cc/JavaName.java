
package lisp.cc;

import lisp.Symbol;

public class JavaName
{

    /**
     * Convert a lisp symbol name into a valid Java identifier String. The same symbol will always
     * convert to the same Java name. Symbols of the same name in different packages may produce
     * conflicts.
     *
     * @param symbol The symbol to extract a name from.
     * @return A valid Java identifier corresponding to the symbol.
     */
    public String createJavaSymbolName (final Symbol symbol)
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("SYM_");
	final String symbolName = symbol.getName ();
	for (int i = 0; i < symbolName.length (); i++)
	{
	    final char c = symbolName.charAt (i);
	    if (Character.isJavaIdentifierPart (c))
	    {
		buffer.append (c);
	    }
	    else
	    {
		final int codePoint = symbolName.codePointAt (i);
		final String cn = Character.getName (codePoint);
		if (cn != null)
		{
		    buffer.append (c);
		}
		else
		{
		    buffer.append (String.format ("%03d", codePoint));
		}
	    }
	}
	return buffer.toString ();
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
