
package plan;

import java.util.*;

import lisp.*;

/** Domain action with preconditions and postconditions. */
public class Action implements Describer
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

    public Action (final Action action, final Bindings bindings)
    {
	name = action.name.gensym ();
	precondition = new ArrayList<Condition> ();
	postcondition = new ArrayList<Condition> ();
	for (final Condition c : action.precondition)
	{
	    precondition.add (c.bind (bindings));
	}
	for (final Condition c : action.postcondition)
	{
	    postcondition.add (c.bind (bindings));
	}
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

    // public List<Condition> getAddConditions ()
    // {
    // final List<Condition> result = new ArrayList<Condition> ();
    // for (final Condition c : postcondition)
    // {
    // if (!c.isNegated ())
    // {
    // result.add (c);
    // }
    // }
    // return result;
    // }
    //
    // public List<Condition> getDeleteConditions ()
    // {
    // final List<Condition> result = new ArrayList<Condition> ();
    // for (final Condition c : postcondition)
    // {
    // if (c.isNegated ())
    // {
    // result.add (c);
    // }
    // }
    // return result;
    // }

    public Bindings canAchieve (final Condition condition)
    {
	for (final Condition c : postcondition)
	{
	    final Bindings match = c.matches (condition);
	    if (match != null)
	    {
		return match;
	    }
	}
	return null;
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

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Name", name);
	result.put ("Precondition", precondition);
	result.put ("Postcondition", postcondition);
	return result;
    }
}
