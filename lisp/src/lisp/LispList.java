
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
	buffer.append (">");
	return buffer.toString ();
    }
}
