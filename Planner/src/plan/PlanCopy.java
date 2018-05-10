
package plan;

import java.util.*;

import lisp.Symbol;

/** Construct a copy of a plan with a map from parent plan nodes to child plan nodes. */
public class PlanCopy
{
    private final Plan parent;
    private final Plan child;
    private final Map<Node, Node> nodeMap;

    /** Construct a copy of a plan with a map from parent plan nodes to child plan nodes. */
    public PlanCopy (final Plan plan, final double incrementCost)
    {
	parent = plan;
	final Symbol name = plan.getName ();
	final Symbol childName = name.gensym ();
	child = new Plan (childName, plan, incrementCost);
	nodeMap = new HashMap<Node, Node> ();
	final List<Node> nodes = plan.getNodes ();
	for (final Node n : nodes)
	{
	    final Node c = new Node (n);
	    nodeMap.put (n, c);
	    child.addNode (c);
	}
	for (final Node n : nodes)
	{
	    final Node n2 = nodeMap.get (n);
	    for (final Node a : n.getNext ())
	    {
		final Node a2 = nodeMap.get (a);
		n2.addSuccessor (a2);
	    }
	}
	// Copy causal links
	for (final Node n : nodes)
	{
	    final Node n2 = nodeMap.get (n);
	    for (final ProtectionInterval pi : n.getCausalLinks ())
	    {
		final Condition condition = pi.getCondition ();
		final Node achieverCopy = nodeMap.get (pi.getAchiever ());
		final Node protectedNodeCopy = nodeMap.get (pi.getProtectedNode ());
		final ProtectionInterval pi2 = new ProtectionInterval (condition, achieverCopy, protectedNodeCopy);
		n2.addPI (pi2);
	    }
	}
    }

    /** Get the original plan. */
    public Plan getParent ()
    {
	return parent;
    }

    /** Get the copy of the original plan. */
    public Plan getChild ()
    {
	return child;
    }

    /** Get the map from original plan nodes to copy plan nodes. */
    public Map<Node, Node> getNodeMap ()
    {
	return nodeMap;
    }

    /** Get the copy of a node using the nodemap. */
    public Node getChildNode (final Node node)
    {
	return nodeMap.get (node);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" parent:");
	buffer.append (parent.getName ());
	buffer.append (" child:");
	buffer.append (child.getName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
