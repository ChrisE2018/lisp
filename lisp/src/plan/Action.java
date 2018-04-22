
package plan;

import java.util.*;

import lisp.Symbol;

public class Action
{
    private static List<Action> actions = new ArrayList<Action> ();

    private final Symbol name;
    private final List<Condition> precondition;
    private final List<Condition> postcondition;

    public static List<Action> getActions ()
    {
	return actions;
    }

    public Action (final Symbol name, final List<Condition> precondition, final List<Condition> postcondition)
    {
	this.name = name;
	this.precondition = precondition;
	this.postcondition = postcondition;
	actions.add (this);
    }

    public Symbol getName ()
    {
	return name;
    }

    public List<Condition> getPrecondition ()
    {
	return precondition;
    }

    public List<Condition> getPostcondition ()
    {
	return postcondition;
    }

    public void print (final StringBuilder buffer)
    {
	buffer.append (name.getName ());
	if (precondition.size () > 0)
	{
	    buffer.append (" [precondition");
	    for (final Condition condition : precondition)
	    {
		buffer.append (' ');
		condition.print (buffer);
	    }
	    buffer.append (']');
	}
	if (postcondition.size () > 0)
	{
	    buffer.append (" [postcondition");
	    for (final Condition condition : postcondition)
	    {
		buffer.append (' ');
		condition.print (buffer);
	    }
	    buffer.append (']');
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }
}
