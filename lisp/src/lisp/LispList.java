
package lisp;

import java.util.*;

public abstract class LispList extends ArrayList<Lisp> implements Lisp
{
    public abstract ListKind getListKind ();

    public LispList ()
    {
	super ();
    }

    public LispList (final List<Lisp> members)
    {
	super (members);
    }

    public void print (final StringBuilder buffer)
    {
	// Special case for quote
	if (size () == 2)
	{
	    final Lisp head = get (0);
	    if (head instanceof Symbol)
	    {
		final Symbol s = (Symbol)head;
		if (s.getName ().equals ("quote"))
		{
		    buffer.append ("'");
		    get (1).print (buffer);
		    return;
		}
	    }
	}
	buffer.append (getListKind ().getOpenChar ());
	for (int i = 0; i < size (); i++)
	{
	    if (i > 0)
	    {
		buffer.append (' ');
	    }
	    get (i).print (buffer);
	}
	buffer.append (getListKind ().getCloseChar ());
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	print (buffer);
	buffer.append (">");
	return buffer.toString ();
    }
}
