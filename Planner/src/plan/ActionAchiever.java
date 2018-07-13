
package plan;

import java.util.*;

import lisp.lang.Symbol;

public class ActionAchiever extends Achiever
{
    private final Node node;
    private final Condition condition;
    private final Action action;
    private final Bindings bindings;
    private final double incrementCost;

    ActionAchiever (final Plan parent, final Node node, final Condition condition, final Action action, final Bindings bindings,
            final double incrementCost)
    {
	super (parent);
	this.node = node;
	this.condition = condition;
	this.action = action;
	this.bindings = bindings;
	this.incrementCost = incrementCost;
    }

    public Node getNode ()
    {
	return node;
    }

    public Condition getCondition ()
    {
	return condition;
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
    public ProtectionInterval expand ()
    {
	makeChild ();
	final Plan child = getChild ();
	final Map<Node, Node> nodeMap = getNodeMap ();
	// Make a new node for the action
	final Symbol actionNodeName = action.getName ().gensym ();
	final Node a = new Node (actionNodeName);
	a.setAction (action);
	child.addNode (a);
	nodeMap.get (getParent ().getInitialNode ()).addSuccessor (a);
	// Add a causal link from the action node to the goal node
	final ProtectionInterval pi = a.addPI (condition, nodeMap.get (node));
	for (final Condition pre : action.getPrecondition ())
	{
	    a.getGoalConditions ().add (pre.bind (bindings));
	}
	for (final Condition c : action.getPostcondition ())
	{
	    if (!c.isNegated ())
	    {
		a.getAddConditions ().add (c.bind (bindings));
	    }
	    else
	    {
		a.getDeleteConditions ().add (c.bind (true, bindings));
	    }
	}
	return pi;
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
