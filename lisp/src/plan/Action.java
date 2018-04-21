
package plan;

import lisp.*;

public class Action
{
    private final Symbol name;
    private final LispList precondition;
    private final LispList postcondition;

    public Action (final Symbol name, final LispList precondition, final LispList postcondition)
    {
	this.name = name;
	this.precondition = precondition;
	this.postcondition = postcondition;
    }

    public Symbol getName ()
    {
	return name;
    }

    public LispList getPrecondition ()
    {
	return precondition;
    }

    public LispList getPostcondition ()
    {
	return postcondition;
    }

    public void print (final StringBuilder buffer)
    {
	buffer.append (name.getName ());
	buffer.append (' ');
	precondition.print (buffer);
	buffer.append (' ');
	postcondition.print (buffer);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }
}
