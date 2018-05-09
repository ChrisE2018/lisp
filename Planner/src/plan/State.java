
package plan;

import lisp.LispList;

public class State
{
    private final LispList facts;

    public State (final LispList facts)
    {
	this.facts = facts;
    }

    public LispList getFacts ()
    {
	return facts;
    }

    public int getFactCount ()
    {
	return facts.size ();
    }

    public Object getFact (final int i)
    {
	return facts.get (i);
    }

    public void print (final StringBuilder buffer)
    {
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	facts.print (buffer);
	buffer.append (">");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }
}
