
package plan;

import java.util.*;

import lisp.Symbol;

public class Plan
{
    private final Symbol name;

    private List<Node> nodes;

    private final Plan parent;

    private List<Plan> children;

    public Plan (final Symbol name, final Plan parent)
    {
	this.name = name;
	this.parent = parent;
    }

    /** Determine if this plan is a solution. */
    public boolean isSolution ()
    {
	for (final Node node : nodes)
	{
	    if (node.hasOpenSubgoals ())
	    {
		return false;
	    }
	}
	return true;
    }

    public List<Node> getOpenNodes ()
    {
	final List<Node> result = new ArrayList<Node> ();
	for (final Node node : nodes)
	{
	    if (node.hasOpenSubgoals ())
	    {
		result.add (node);
	    }
	}
	return result;
    }

    public List<Plan> expand (final Node node, final Condition condition)
    {
	final List<Plan> result = new ArrayList<Plan> ();
	// [TODO] Implement
	final List<Action> actions = Action.getActions ();
	for (final Action action : actions)
	{
	    // If the action can achieve the condition, then make an expanded plan by inserting the
	    // action. Figure out all variable bindings.
	}
	// Search for causal links that can make the condition true
	// Search for actions that can be inserted to make the condition true
	return result;
    }

    public Symbol getName ()
    {
	return name;
    }

    public Plan getParent ()
    {
	return parent;
    }

    public void addNode (final Node node)
    {
	if (nodes == null)
	{
	    nodes = new ArrayList<Node> ();
	}
	nodes.add (node);
    }

    public void addChild (final Plan child)
    {
	if (children == null)
	{
	    children = new ArrayList<Plan> ();
	}
	children.add (child);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	buffer.append (">");
	return buffer.toString ();
    }
}
