
package plan;

import java.util.*;

import lisp.lang.Symbol;
import plan.gui.PlanView;

public class LinkAchiever extends Achiever
{
    private final Node protectedNode;
    private final Condition condition;
    private final Node achieverNode;
    private final Bindings bindings;

    LinkAchiever (final Plan parent, final Node protectedNode, final Condition condition, final Node achieverNode,
            final Bindings bindings)
    {
	super (parent);
	this.protectedNode = protectedNode;
	this.condition = condition;
	this.achieverNode = achieverNode;
	this.bindings = bindings;
    }

    @Override
    public double getIncrementCost ()
    {
	return 1.0;
    }

    public Node getProtectedNode ()
    {
	return protectedNode;
    }

    public Node getAchieverNode ()
    {
	return achieverNode;
    }

    public Condition getCondition ()
    {
	return condition;
    }

    public Bindings getBindings ()
    {
	return bindings;
    }

    @Override
    public ProtectionInterval expand ()
    {
	// Create a child by adding a causal link from n to node
	makeChild (bindings);
	final Plan child = getChild ();
	if (Symbol.value ("PlanView") == Boolean.TRUE)
	{
	    PlanView.makeView (getParent ());
	    PlanView.makeView (child);
	}
	final Map<Node, Node> nodeMap = getNodeMap ();
	final Node achNode = nodeMap.get (getAchieverNode ());
	final Node protNode = nodeMap.get (protectedNode);
	final Condition bCond = condition.bind (bindings);
	final ProtectionInterval pi = achNode.addPI (bCond, protNode);
	// child.revisionGoal = pi;
	return pi;
    }

    @Override
    public Plan expand (final List<Plan> result)
    {
	throw new UnsupportedOperationException ("Can't expand LinkAchiever");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (condition);
	buffer.append ("@");
	buffer.append (achieverNode.getName ());
	buffer.append ("=>");
	buffer.append (protectedNode.getName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
