
package plan;

import java.util.*;

import lisp.Symbol;

public class Node
{
    private final Symbol name;

    /** List of nodes that are before this one. */
    private final List<Node> previous = new ArrayList<Node> ();

    /** List of nodes that are after this one. */
    private final List<Node> next = new ArrayList<Node> ();

    /** List of conditions that are made true when this node is executed. */
    private final List<Condition> addConditions = new ArrayList<Condition> ();

    /** List of conditions that are made false when this node is executed. */
    private final List<Condition> deleteConditions = new ArrayList<Condition> ();

    /** List of conditions that are required when this node is executed. */
    private final List<Condition> goalConditions = new ArrayList<Condition> ();

    private final List<ProtectionInterval> causalLinks = new ArrayList<ProtectionInterval> ();

    public Node (final Symbol name)
    {
	this.name = name;
    }

    /**
     * Make this node earlier than another node.
     *
     * @param node A node that will be after this node.
     */
    public void addSuccessor (final Node node)
    {
	if (!next.contains (node))
	{
	    next.add (node);
	}
	if (!node.previous.contains (this))
	{
	    node.previous.add (this);
	}
    }

    /**
     * Determine if this node is before another node.
     *
     * @param node The node being checked.
     * @return True if the node parameter must be after this node.
     */
    public boolean before (final Node node)
    {
	if (next.contains (node))
	{
	    return true;
	}
	for (final Node n : next)
	{
	    if (n.before (node))
	    {
		return true;
	    }
	}
	return false;
    }

    /**
     * Determine if this node is after another node.
     *
     * @param node The node being checked.
     * @return True if the node parameter must be before this node.
     */
    public boolean after (final Node node)
    {
	if (previous.contains (node))
	{
	    return true;
	}
	for (final Node n : previous)
	{
	    if (n.after (node))
	    {
		return true;
	    }
	}
	return false;
    }

    public Symbol getName ()
    {
	return name;
    }

    public List<Condition> getAddConditions ()
    {
	return addConditions;
    }

    public List<Condition> getDeleteConditions ()
    {
	return deleteConditions;
    }

    public List<Condition> getGoalConditions ()
    {
	return goalConditions;
    }

    public boolean hasOpenSubgoals ()
    {
	return goalConditions.size () > 0;
    }

    public List<ProtectionInterval> getCausalLinks ()
    {
	return causalLinks;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	for (final Condition condition : addConditions)
	{
	    buffer.append (" +");
	    buffer.append (condition);
	}
	for (final Condition condition : deleteConditions)
	{
	    buffer.append (" -");
	    buffer.append (condition);
	}
	for (final Condition condition : goalConditions)
	{
	    buffer.append (" ?");
	    buffer.append (condition);
	}
	buffer.append (">");
	return buffer.toString ();
    }
}
