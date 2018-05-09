
package plan;

import java.util.*;

import lisp.*;

public class Node implements Describer
{
    private static final boolean PRINT_SHORT = true;

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

    /** Causal links starting here to another protected node. */
    private final List<ProtectionInterval> causalLinks = new ArrayList<ProtectionInterval> ();

    /** Goals achieved here by another node. */
    private final List<ProtectionInterval> protectedGoals = new ArrayList<ProtectionInterval> ();

    private Action action = null;

    public Node (final Symbol name)
    {
	this.name = name;
	this.name.setValue (this);
    }

    public Node (final Node parent)
    {
	name = parent.name.gensym ();
	name.setValue (this);
	addConditions.addAll (parent.addConditions);
	deleteConditions.addAll (parent.deleteConditions);
	goalConditions.addAll (parent.goalConditions);
	action = parent.action;
	// Previous, next and causal links must be done later
    }

    public Action getAction ()
    {
	return action;
    }

    public void setAction (final Action action)
    {
	this.action = action;
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

    public List<Node> getPrevious ()
    {
	return previous;
    }

    public List<Node> getNext ()
    {
	return next;
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

    public boolean causes (final Condition c)
    {
	for (final Condition a : addConditions)
	{
	    if (a.causes (c))
	    {
		return true;
	    }
	}
	for (final Condition a : deleteConditions)
	{
	    if (a.deleteCauses (c))
	    {
		return true;
	    }
	}
	return false;
    }

    public boolean conflicts (final Condition c)
    {
	for (final Condition a : addConditions)
	{
	    if (a.conflicts (c))
	    {
		return true;
	    }
	}
	for (final Condition a : deleteConditions)
	{
	    if (a.deleteConflicts (c))
	    {
		return true;
	    }
	}
	return false;
    }

    public boolean hasOpenSubgoals ()
    {
	return goalConditions.size () > 0;
    }

    /** Causal links starting here to another protected node. */
    public List<ProtectionInterval> getCausalLinks ()
    {
	return causalLinks;
    }

    /**
     * Add an already constructed PI to this node. This constructor is used when making a copy of a
     * plan.
     *
     * @param pi
     */
    public void addPI (final ProtectionInterval pi)
    {
	causalLinks.add (pi);
	pi.getProtectedNode ().protectedGoals.add (pi);
    }

    /**
     * Add a PI for a condition to this node. This method is used when creating a protection
     * interval initially.
     *
     * @param condition
     * @param protectedNode
     * @return
     */
    public ProtectionInterval addPI (final Condition condition, final Node protectedNode)
    {
	if (protectedNode.goalConditions.contains (condition))
	{
	    addSuccessor (protectedNode);
	    final ProtectionInterval result = new ProtectionInterval (condition, this, protectedNode);
	    causalLinks.add (result);
	    protectedNode.protectedGoals.add (result);
	    protectedNode.goalConditions.remove (condition);
	    return result;
	}
	throw new IllegalArgumentException (condition + " is not a goal of " + protectedNode);
    }

    /** Goals achieved here by another node. */
    public List<ProtectionInterval> getProtectedGoals ()
    {
	return protectedGoals;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	if (action != null)
	{
	    buffer.append (" ");
	    buffer.append (action.getName ());
	}
	if (!PRINT_SHORT)
	{
	    for (final Condition condition : goalConditions)
	    {
		buffer.append (" ?");
		buffer.append (condition);
	    }
	    for (final Condition condition : deleteConditions)
	    {
		buffer.append (" -");
		buffer.append (condition);
	    }
	    for (final Condition condition : addConditions)
	    {
		buffer.append (" +");
		buffer.append (condition);
	    }
	    for (final ProtectionInterval link : causalLinks)
	    {
		buffer.append (" $");
		buffer.append (link.getCondition ());
		buffer.append ("=>");
		buffer.append (link.getProtectedNode ().getName ());
	    }
	}
	buffer.append (">");
	return buffer.toString ();
    }

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Name", name);
	if (action != null)
	{
	    result.put ("Action", action);
	}
	if (deleteConditions.size () > 0)
	{
	    result.put ("Delete", deleteConditions);
	}
	if (addConditions.size () > 0)
	{
	    result.put ("Add", addConditions);
	}
	if (causalLinks.size () > 0)
	{
	    result.put ("Links", causalLinks);
	}
	if (goalConditions.size () > 0)
	{
	    result.put ("Goals", goalConditions);
	}
	if (previous.size () > 0)
	{
	    result.put ("previous", previous);
	}
	if (next.size () > 0)
	{
	    result.put ("next", next);
	}

	return result;
    }
}
