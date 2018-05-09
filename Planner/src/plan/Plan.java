
package plan;

import java.util.*;

import lisp.*;
import plan.gui.PlanView;
import util.Pair;

// [TODO] Implement ProblemState and use search on this
public class Plan implements Describer
{
    /** Name of this plan. */
    private final Symbol name;

    /** Name of the parent plan, if any, otherwise null. */
    private final Symbol parentName;

    /** References to children. Note that the name is stored, not the object. */
    private final List<Symbol> children = new ArrayList<Symbol> ();

    /** Revisions made to the parent plan to generate this plan. */
    private Object revisionGoal = null;
    private Object revisionSupport = null;

    private final List<Node> nodes = new ArrayList<Node> ();

    public Plan (final Symbol parentName, final Symbol name)
    {
	this.parentName = parentName;
	this.name = name;
	name.setValue (this);
	if (parentName != null)
	{
	    final Plan parent = (Plan)parentName.getValue ();
	    parent.children.add (name);
	}
    }

    /** Determine if this plan is a solution. */
    public boolean solved ()
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

    public List<Node> getNodes ()
    {
	return nodes;
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

    public List<Plan> expand ()
    {
	return expand (true, true);
    }

    public List<Plan> expand (final boolean useActions, final boolean useLinks)
    {
	final List<Plan> result = new ArrayList<Plan> ();
	for (final Node node : nodes)
	{
	    if (node.hasOpenSubgoals ())
	    {
		for (final Condition goal : node.getGoalConditions ())
		{
		    System.out.printf ("Expanding %s: %s %n", node, goal);
		    if (useActions)
		    {
			insertActions (result, node, goal);
		    }
		    if (useLinks)
		    {
			insertLinks (result, node, goal);
		    }
		    if (!result.isEmpty ())
		    {
			return result;
		    }
		}
	    }
	}
	return result;
    }

    private void insertActions (final List<Plan> result, final Node node, final Condition condition)
    {
	// Search for actions that can be inserted to make the condition true
	final List<Pair<Action, Bindings>> actions = getAchievingActions (condition);
	if (actions != null)
	{
	    for (final Pair<Action, Bindings> matchedAction : actions)
	    {
		final Action action = matchedAction.getFirst ();
		final Bindings match = matchedAction.getSecond ();
		// If the action can achieve the condition, then make an expanded plan by inserting
		// the action. Figure out all variable bindings.
		System.out.printf ("   Attempting %s with bindings %s %n", action, match);
		// Copy the plan and create a new node after the initial node and before 'node' with
		// the action.
		expand (result, node, condition, action, match);
	    }
	}
    }

    private List<Pair<Action, Bindings>> getAchievingActions (final Condition condition)
    {
	List<Pair<Action, Bindings>> result = null;
	final List<Action> actions = Action.getActions ();
	for (final Action action : actions)
	{
	    final Bindings match = action.canAchieve (condition);
	    if (match != null)
	    {
		if (result == null)
		{
		    result = new ArrayList<Pair<Action, Bindings>> ();
		}
		result.add (new Pair<Action, Bindings> (action, match));
	    }
	}
	return result;
    }

    private void expand (final List<Plan> result, final Node node, final Condition condition, final Action action,
            final Bindings match)
    {
	final PlanCopy copy = new PlanCopy (this);
	final Plan child = copy.getChild ();
	final Map<Node, Node> nodeMap = copy.getNodeMap ();
	// Make a new node for the action
	final Symbol actionNodeName = action.getName ().gensym ();
	final Node a = new Node (actionNodeName);
	a.setAction (action);
	child.addNode (a);
	nodeMap.get (getInitialNode ()).addSuccessor (a);
	// Add a causal link from the action node to the goal node
	final ProtectionInterval pi = a.addPI (condition, nodeMap.get (node));
	child.revisionGoal = pi;
	child.revisionSupport = action;
	for (final Condition pre : action.getPrecondition ())
	{
	    a.getGoalConditions ().add (pre.bind (match));
	}
	for (final Condition c : action.getPostcondition ())
	{
	    if (!c.isNegated ())
	    {
		a.getAddConditions ().add (c.bind (match));
	    }
	    else
	    {
		a.getDeleteConditions ().add (c.bind (true, match));
	    }
	}

	child.determineAndResolveConflicts (result, pi);
    }

    private void insertLinks (final List<Plan> result, final Node node, final Condition condition)
    {
	// Search for causal links that can make the condition true
	final List<Node> achievers = getPossibleAchievers (node, condition);
	if (achievers != null)
	{
	    for (final Node n : achievers)
	    {
		// Create a child by adding a causal link from n to node
		final PlanCopy copy = new PlanCopy (this);
		final Plan child = copy.getChild ();
		if (Symbol.value ("user:::PlanView") == Boolean.TRUE)
		{
		    PlanView.makeView (this);
		    PlanView.makeView (child);
		}
		final Map<Node, Node> nodeMap = copy.getNodeMap ();
		final ProtectionInterval pi = nodeMap.get (n).addPI (condition, nodeMap.get (node));
		child.revisionGoal = pi;

		child.determineAndResolveConflicts (result, pi);
	    }
	}
    }

    public List<Node> getPossibleAchievers (final Node node, final Condition condition)
    {
	List<Node> result = null;
	for (final Node n : nodes)
	{
	    if (!n.after (node))
	    {
		if (n.causes (condition))
		{
		    if (result == null)
		    {
			result = new ArrayList<Node> ();
		    }
		    result.add (n);
		}
	    }
	}
	return result;
    }

    /** Resolve conflicts with pi. */
    private void determineAndResolveConflicts (final List<Plan> result, final ProtectionInterval pi)
    {
	final List<PIConflict> conflicts = new ArrayList<PIConflict> ();
	getConflicts (conflicts, pi);
	if (conflicts.size () == 0)
	{
	    result.add (this);
	}
	else
	{
	    resolveConflicts (result, conflicts);
	}
    }

    private void getConflicts (final List<PIConflict> conflicts, final ProtectionInterval pi)
    {
	final Condition c = pi.getCondition ();
	final Node from = pi.getAchiever ();
	final Node to = pi.getProtectedNode ();
	for (final Node n : nodes)
	{
	    if (n != to)
	    {
		if (n.conflicts (c))
		{
		    if (!n.before (from) && !to.before (n))
		    {
			// This is a possibly conflicting node
			System.out.printf ("Conflicted condition %s %n", c);
			System.out.printf ("Conflict node %s %n", n);
			System.out.printf ("Conflict interval %s => %s %n", from, to);
			final PIConflict conflict = new PIConflict (n, pi);
			conflicts.add (conflict);
		    }
		}
	    }
	}
    }

    private void resolveConflicts (final List<Plan> result, final List<PIConflict> conflicts)
    {
	if (conflicts.size () == 1 && Math.abs (1) < 0)
	{
	    final PIConflict conflict = conflicts.get (0);
	    final List<Pair<Node, Node>> resolutions = conflict.getResolutions ();
	    for (final Pair<Node, Node> resolution : resolutions)
	    {
		final Node before = resolution.getFirst ();
		final Node after = resolution.getSecond ();
		final PlanCopy copy = new PlanCopy (this);
		final Plan child = copy.getChild ();
		final Map<Node, Node> nodeMap = copy.getNodeMap ();
		final Node nodeCopy = nodeMap.get (before);
		final Node fromCopy = nodeMap.get (after);
		nodeCopy.addSuccessor (fromCopy);
		result.add (child);
	    }
	    result.remove (this);
	}
	else
	{
	    final LinkedList<List<Pair<Node, Node>>> partialResolutions = new LinkedList<List<Pair<Node, Node>>> ();
	    for (final PIConflict conflict : conflicts)
	    {
		partialResolutions.add (conflict.getResolutions ());
	    }
	    final List<List<Pair<Node, Node>>> fullResolutions = computeCombinations (partialResolutions);
	    // cartesianProduct (partialResolutions);
	    for (final List<Pair<Node, Node>> resolutions : fullResolutions)
	    {
		for (final Pair<Node, Node> resolution : resolutions)
		{
		    final Node before = resolution.getFirst ();
		    final Node after = resolution.getSecond ();
		    final PlanCopy copy = new PlanCopy (this);
		    final Plan child = copy.getChild ();
		    child.revisionGoal = conflicts;
		    child.revisionSupport = resolution;
		    final Map<Node, Node> nodeMap = copy.getNodeMap ();
		    final Node nodeCopy = nodeMap.get (before);
		    final Node fromCopy = nodeMap.get (after);
		    nodeCopy.addSuccessor (fromCopy);
		    result.add (child);
		}
	    }
	}
    }

    // private List<List<Pair<Node, Node>>> cartesianProduct (final LinkedList<List<Pair<Node,
    // Node>>> partialResolutions)
    // {
    // if (partialResolutions.size () == 0)
    // {
    // return new ArrayList<List<Pair<Node, Node>>> ();
    // }
    // else
    // {
    // final List<List<Pair<Node, Node>>> result = new ArrayList<List<Pair<Node, Node>>> ();
    // final List<Pair<Node, Node>> r = partialResolutions.pop ();
    // final List<List<Pair<Node, Node>>> recursiveResult = cartesianProduct (partialResolutions);
    // for (final Pair<Node, Node> rr : r)
    // {
    // for (final List<Pair<Node, Node>> tail : recursiveResult)
    // {
    // final List<Pair<Node, Node>> option = new ArrayList<Pair<Node, Node>> ();
    // option.add (rr);
    // option.addAll (tail);
    // result.add (option);
    // }
    // }
    // return result;
    // }
    // }

    /**
     * Cartesian product implement from the WWW.
     *
     * @see https://codereview.stackexchange.com/questions/67804/generate-cartesian-product-of-list-in-java
     * @param lists
     * @return
     */
    public <T> List<List<T>> computeCombinations (final List<List<T>> lists)
    {
	List<List<T>> result = Arrays.asList (Arrays.asList ());
	for (final List<T> list : lists)
	{
	    final List<List<T>> extraColumnCombinations = new ArrayList<> ();
	    for (final List<T> combination : result)
	    {
		for (final T element : list)
		{
		    final List<T> newCombination = new ArrayList<> (combination);
		    newCombination.add (element);
		    extraColumnCombinations.add (newCombination);
		}
	    }
	    result = extraColumnCombinations;
	}
	return result;
    }

    /** Name of this plan. */
    public Symbol getName ()
    {
	return name;
    }

    /** Name of the parent plan, if any, otherwise null. */
    public Symbol getParentName ()
    {
	return parentName;
    }

    public List<Symbol> getChildren ()
    {
	return children;
    }

    /** Revisions made to the parent plan to generate this plan. */
    public Object getRevisionGoal ()
    {
	return revisionGoal;
    }

    public Object getRevisionSupport ()
    {
	return revisionSupport;
    }

    public void addNode (final Node node)
    {
	nodes.add (node);
    }

    public Node getNode (final Symbol nodeName)
    {
	for (final Node node : nodes)
	{
	    if (node.getName () == nodeName)
	    {
		return node;
	    }
	}
	throw new IllegalArgumentException ("Plan has no node named " + nodeName);
    }

    public Node getInitialNode ()
    {
	for (final Node n : nodes)
	{
	    if (n.getPrevious ().isEmpty ())
	    {
		return n;
	    }
	}
	throw new Error ("Plan has no initial state");
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	for (final Node node : nodes)
	{
	    buffer.append (" ");
	    buffer.append (node.getName ());
	}
	buffer.append (">");
	return buffer.toString ();
    }

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Name", name);
	result.put ("Solved", solved ());
	// result.put ("Nodes", nodes);
	for (final Node n : nodes)
	{
	    result.put (n.getName ().getName (), n);
	}

	// result.put ("Parent", parent);
	// result.put ("Children", children);
	return result;
    }
}
