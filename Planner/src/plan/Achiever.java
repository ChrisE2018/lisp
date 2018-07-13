
package plan;

import java.util.*;

import lisp.lang.Symbol;

public abstract class Achiever
{
    private final Plan parent;
    private Plan child = null;

    private Map<Node, Node> nodeMap = null;

    Achiever (final Plan parent)
    {
	this.parent = parent;
    }

    public Plan getParent ()
    {
	return parent;
    }

    public abstract double getIncrementCost ();

    public abstract ProtectionInterval expand ();

    public abstract Plan expand (final List<Plan> result);

    public Plan getChild ()
    {
	return child;
    }

    /** Construct a copy of a plan with a map from parent plan nodes to child plan nodes. */
    public void makeChild ()
    {
	makeChild (new Bindings ());
    }

    /** Construct a copy of a plan with a map from parent plan nodes to child plan nodes. */
    public void makeChild (final Bindings bindings)
    {
	final Symbol parentName = parent.getName ();
	final Symbol childName = parentName.gensym ();
	child = new Plan (childName, this);
	nodeMap = new HashMap<Node, Node> ();
	final List<Node> nodes = parent.getNodes ();
	for (final Node n : nodes)
	{
	    final Node c = new Node (n, bindings);
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
		final Condition condition = pi.getCondition ().bind (bindings);
		final Node achieverCopy = nodeMap.get (pi.getAchiever ());
		final Node protectedNodeCopy = nodeMap.get (pi.getProtectedNode ());
		final ProtectionInterval pi2 = new ProtectionInterval (condition, achieverCopy, protectedNodeCopy);
		n2.addPI (pi2);
	    }
	}
    }

    /** Get the map from original plan nodes to child plan nodes. */
    public Map<Node, Node> getNodeMap ()
    {
	return nodeMap;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
