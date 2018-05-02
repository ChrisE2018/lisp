
package lisp;

import java.util.*;

/** Extended List with nice external representation. */
public class LispList extends ArrayList<Object>
{
    /**
     * The parsing control should be obtained from the current package. Currently it is a constant.
     */
    private static Parsing parsing = new Parsing ();

    /** Character that starts the list. */
    private final char openChar;

    /** Character that finishes the list. */
    private final char closeChar;

    public LispList (final char openChar, final char closeChar)
    {
	super ();
	this.openChar = openChar;
	this.closeChar = closeChar;
    }

    public LispList (final char openChar, final char closeChar, final List<Object> members)
    {
	super (members);
	this.openChar = openChar;
	this.closeChar = closeChar;
    }

    public LispList ()
    {
	this (parsing.getDefaultOpenChar (), parsing.getDefaultCloseChar ());
    }

    public LispList (final List<Object> members)
    {
	this (parsing.getDefaultOpenChar (), parsing.getDefaultCloseChar (), members);
    }

    /** Character that starts the list. */
    public char getOpenChar ()
    {
	return openChar;
    }

    /** Character that finishes the list. */
    public char getCloseChar ()
    {
	return closeChar;
    }

    /** Print in the same format as the reader. */
    public void print (final StringBuilder buffer)
    {
	// Special case for quoted wrapper forms
	if (size () == 2)
	{
	    final Object head = get (0);
	    if (head instanceof Symbol)
	    {
		final Symbol s = (Symbol)head;
		final Character chr = parsing.getWrapperQuote (s);
		if (chr != null)
		{
		    buffer.append (chr);
		    buffer.append (get (1).toString ());
		    return;
		}
	    }
	}
	buffer.append (getOpenChar ());
	for (int i = 0; i < size (); i++)
	{
	    if (i > 0)
	    {
		buffer.append (' ');
	    }
	    LispReader.printElement (buffer, get (i));
	}
	buffer.append (getCloseChar ());
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	// buffer.append ("#<");
	// buffer.append (getClass ().getSimpleName ());
	// buffer.append (" ");
	// print (buffer);
	// buffer.append (">");
	return buffer.toString ();
    }
}
