
package plan;

import java.util.List;

public class ActionAchiever extends Achiever
{
    private final Action action;
    private final Bindings bindings;
    private final double incrementCost;

    ActionAchiever (final Plan parent, final Action action, final Bindings bindings, final double incrementCost)
    {
	super (parent);
	this.action = action;
	this.bindings = bindings;
	this.incrementCost = incrementCost;
    }

    public Action getAction ()
    {
	return action;
    }

    public Bindings getBindings ()
    {
	return bindings;
    }

    @Override
    public double getIncrementCost ()
    {
	return incrementCost;
    }

    @Override
    public Plan expand (final List<Plan> result)
    {
	throw new UnsupportedOperationException ("Can't expand actions yet");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (incrementCost);
	buffer.append (" ");
	if (action == null)
	{
	    buffer.append (" dummy action ");
	}
	else
	{
	    buffer.append (action.getName ());
	}
	buffer.append (" ");
	buffer.append (bindings);
	buffer.append (">");
	return buffer.toString ();
    }
}
