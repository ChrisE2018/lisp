
package lisp;

import java.util.*;

public class LispList extends ArrayList<Object>
{
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
	this ('(', ')');
    }

    public LispList (final List<Object> members)
    {
	this ('(', ')', members);
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

    public void print (final StringBuilder buffer)
    {
	// Special case for quote
	// [TODO] Use table of SINGLE_CHAR_FORMS to map quote instead.
	if (size () == 2)
	{
	    final Object head = get (0);
	    if (head instanceof Symbol)
	    {
		final Symbol s = (Symbol)head;
		if (s.getName ().equals ("quote"))
		{
		    buffer.append ("'");
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
	    Reader.printElement (buffer, get (i));
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
