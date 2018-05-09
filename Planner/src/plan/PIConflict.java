
package plan;

import java.util.*;

import util.Pair;

/**
 * A conflict is created when a node that denies a protected condition may possibly be between the
 * achieving node and the protected node in some possible linearization of the plan.
 */
public class PIConflict
{
    /** The conflict node. This denies the protected condition. */
    private final Node node;

    /** The protection interval that can be broken. */
    private final ProtectionInterval pi;

    /**
     * A conflict is created when a node that denies a protected condition may possibly be between
     * the achieving node and the protected node in some possible linearization of the plan.
     */
    public PIConflict (final Node node, final ProtectionInterval pi)
    {
	this.node = node;
	this.pi = pi;
    }

    /** The conflict node. This denies the protected condition. */
    public Node getNode ()
    {
	return node;
    }

    /** The protection interval that can be broken. */
    public ProtectionInterval getPi ()
    {
	return pi;
    }

    /**
     * Get a list of ordered pairs of nodes that are possible resolutions of this conflict.
     */
    public List<Pair<Node, Node>> getResolutions ()
    {
	final List<Pair<Node, Node>> result = new ArrayList<Pair<Node, Node>> ();

	final Node from = pi.getAchiever ();
	final Node to = pi.getProtectedNode ();
	// Try node before from
	if (node != from && !from.before (node))
	{
	    final Pair<Node, Node> option = new Pair<Node, Node> (node, from);
	    result.add (option);
	}
	// Try to before node
	if (node != to && !node.before (to))
	{
	    final Pair<Node, Node> option = new Pair<Node, Node> (to, node);
	    result.add (option);
	}
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (node.getName ());
	buffer.append (" ");
	buffer.append (pi.getCondition ());
	buffer.append (" from: ");
	buffer.append (pi.getAchiever ().getName ());
	buffer.append (" to: ");
	buffer.append (pi.getProtectedNode ().getName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
