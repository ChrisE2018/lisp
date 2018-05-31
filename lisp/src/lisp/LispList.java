
package lisp;

import java.util.*;

/** Extended List with nice external representation. */
public class LispList extends ArrayList<Object> implements Describer
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

    public LispList (final List<Object> p)
    {
	this (parsing.getDefaultOpenChar (), parsing.getDefaultCloseChar (), p);
    }

    public LispList (final Object... p)
    {
	this (parsing.getDefaultOpenChar (), parsing.getDefaultCloseChar ());
	for (final Object o : p)
	{
	    add (o);
	}
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

    /** First element of a list, converted to a Symbol for convenience */
    public Symbol head ()
    {
	final Object result = get (0);
	if (result instanceof Symbol)
	{
	    return (Symbol)result;
	}
	throw new IllegalArgumentException ("List does not start with a Symbol " + result);
    }

    /** First element of a list, using standard Lisp terminology. */
    public Object car ()
    {
	return get (0);
    }

    /** First element of a list. */
    public Object first ()
    {
	return get (0);
    }

    public Object last ()
    {
	return get (size () - 1);
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
	if (isBraceMap ())
	{
	    buffer.append (getOpenChar ());
	    for (int i = 0; i < size (); i++)
	    {
		if (i > 0)
		{
		    buffer.append (parsing.getMapSeparator ());
		    buffer.append (' ');
		}
		final Object rawItem = get (i);
		if (rawItem instanceof List)
		{
		    final List<?> item = (List<?>)get (i);
		    for (int j = 0; j < item.size (); j++)
		    {
			if (j > 0)
			{
			    buffer.append (' ');
			}
			LispReader.printElement (buffer, item.get (j));
		    }
		}
		else
		{
		    LispReader.printElement (buffer, rawItem);
		}
	    }
	    buffer.append (getCloseChar ());
	    return;
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

    private boolean isBraceMap ()
    {
	return getOpenChar () == parsing.getMapOpen () && getCloseChar () == parsing.getMapClose ();
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

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	@SuppressWarnings ("unchecked")
	final List<Object> t = (List<Object>)target;
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	for (int i = 0; i < t.size (); i++)
	{
	    result.put ("element" + i, t.get (i));
	}
	return result;
    }
}
