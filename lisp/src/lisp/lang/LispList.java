
package lisp.lang;

import java.util.*;

import lisp.util.MultiMap;

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
	try
	{
	    return (Symbol)result;
	}
	catch (final ClassCastException e)
	{
	    throw new IllegalArgumentException ("List does not start with a Symbol " + result, e);
	}
    }

    // /** Convenience method that allows the caller to avoid a cast. */
    // public LispList getSublist (final int i)
    // {
    // final Object result = get (i);
    // try
    // {
    // return (LispList)result;
    // }
    // catch (final ClassCastException e)
    // {
    // throw new IllegalArgumentException ("List member " + i + " is not a sublist", e);
    // }
    // }

    public Object last ()
    {
	return get (size () - 1);
    }

    public LispList subList (final int i)
    {
	final LispList result = new LispList ();
	result.addAll (subList (i, size ()));
	return result;
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
	return buffer.toString ();
    }

    @Override
    public MultiMap<String, Object> getDescriberValues (final Object target)
    {
	@SuppressWarnings ("unchecked")
	final List<Object> t = (List<Object>)target;
	final MultiMap<String, Object> result = new MultiMap<String, Object> ();
	for (int i = 0; i < t.size (); i++)
	{
	    result.put ("element" + i, t.get (i));
	}
	return result;
    }
}
