
package plan;

import lisp.*;

public class State implements Lisp
{
    private final LispList facts;

    public State (final LispList facts)
    {
	this.facts = facts;
    }

    @Override
    public void print (final StringBuilder buffer)
    {
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	facts.print (buffer);
	buffer.append (">");
    }

    public LispList getFacts ()
    {
	return facts;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (facts);
	buffer.append (">");
	return buffer.toString ();
    }
}
