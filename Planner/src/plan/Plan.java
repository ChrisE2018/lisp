
package plan;

import java.util.*;

import lisp.*;
import plan.gui.PlanView;
import search.ProblemState;
import util.Pair;

/** Implementation of nonlinear plan. */
public class Plan implements Describer, ProblemState
{
    /** Name of this plan. */
    private final Symbol name;

    /** References to children. Note that the name is stored, not the object. */
    private final List<Symbol> children = new ArrayList<Symbol> ();

    /** Revisions made to the parent plan to generate this plan. */
    private Object revisionGoal = null;
    private Object revisionSupport = null;

    /** The plan revision that created this plan. */
    private final Achiever planAchiever;

    private final List<Node> nodes = new ArrayList<Node> ();

    public Plan (final Symbol name)
    {
	this.name = name;
	planAchiever = new EmptyAchiever (this);
	name.setValue (this);
    }

    public Plan (final Symbol name, final Achiever achiever)
    {
	this.name = name;
	planAchiever = achiever;
	name.setValue (this);
	planAchiever.getParent ().children.add (name);
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

    public List<Plan> expandPlan ()
    {
	return expandPlan (true, true);
    }

    public List<Plan> expandPlan (final boolean useActions, final boolean useLinks)
    {
	final List<Plan> result = new ArrayList<Plan> ();
	for (final Node node : nodes)
	{
	    if (node.hasOpenSubgoals ())
	    {
		for (final Condition goal : node.getGoalConditions ())
		{
		    System.out.printf ("Expanding %s: %s %n", node, goal);
		    if (useLinks)
		    {
			insertLinks (result, node, goal);
		    }
		    if (useActions)
		    {
			insertActions (result, node, goal);
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

    /**
     * Get all link achievers for this plan. This was implemented for testing, not for actual use.
     * This returns achievers for all open conditions, but actual plan expansion should select one
     * open condition.
     *
     * @return
     */
    public List<Achiever> getLinkAchievers ()
    {
	final List<Achiever> result = new ArrayList<Achiever> ();
	for (final Node node : nodes)
	{
	    if (node.hasOpenSubgoals ())
	    {
		for (final Condition goal : node.getGoalConditions ())
		{
		    final List<LinkAchiever> achievers = getPossibleAchievers (node, goal);
		    if (achievers != null)
		    {
			result.addAll (achievers);
		    }
		}
	    }
	}
	return result;
    }

    private void insertLinks (final List<Plan> result, final Node protectedNode, final Condition condition)
    {
	// Search for causal links that can make the condition true
	final List<LinkAchiever> achievers = getPossibleAchievers (protectedNode, condition);
	if (achievers != null)
	{
	    for (final LinkAchiever achiever : achievers)
	    {
		// Create a child by adding a causal link from n to node
		final Bindings bindings = achiever.getBindings ();
		if (!bindings.isEmpty ())
		{
		    System.out.printf ("Expanding bound link %n");
		}
		achiever.makeChild (bindings);
		final Plan child = achiever.getChild ();
		if (Symbol.value ("user:::PlanView") == Boolean.TRUE)
		{
		    PlanView.makeView (this);
		    PlanView.makeView (child);
		}
		final Map<Node, Node> nodeMap = achiever.getNodeMap ();
		final Node achNode = nodeMap.get (achiever.getAchieverNode ());
		final Node protNode = nodeMap.get (protectedNode);
		final Condition bCond = condition.bind (bindings);
		final ProtectionInterval pi = achNode.addPI (bCond, protNode);
		child.revisionGoal = pi;

		child.determineAndResolveConflicts (result, pi);
	    }
	}
    }

    public List<LinkAchiever> getPossibleAchievers (final Node protectedNode, final Condition condition)
    {
	List<LinkAchiever> result = null;
	for (final Node possibleAchiever : nodes)
	{
	    if (possibleAchiever != protectedNode && !possibleAchiever.after (protectedNode))
	    {
		final List<Bindings> binds = possibleAchiever.causalBindings (condition);
		if (binds != null)
		{
		    for (final Bindings b : binds)
		    {
			if (result == null)
			{
			    result = new ArrayList<LinkAchiever> ();
			}
			result.add (new LinkAchiever (this, possibleAchiever, protectedNode, b));
		    }
		}
	    }
	}
	return result;
    }

    private void insertActions (final List<Plan> result, final Node node, final Condition condition)
    {
	// Search for actions that can be inserted to make the condition true
	final List<ActionAchiever> actions = getAchievingActions (condition);
	if (actions != null)
	{
	    for (final ActionAchiever achiever : actions)
	    {
		// If the action can achieve the condition, then make an expanded plan by inserting
		// the action. Figure out all variable bindings.
		System.out.printf ("   Attempting %s %n", achiever);
		// Copy the plan and create a new node after the initial node and before 'node' with
		// the action.
		expand (result, node, condition, achiever);
	    }
	}
    }

    private List<ActionAchiever> getAchievingActions (final Condition condition)
    {
	List<ActionAchiever> result = null;
	final List<Action> actions = Action.getActions ();
	for (final Action action : actions)
	{
	    final Bindings match = action.canAchieve (condition);
	    if (match != null)
	    {
		if (result == null)
		{
		    result = new ArrayList<ActionAchiever> ();
		}
		result.add (new ActionAchiever (this, action, match, 2.0));
	    }
	}
	return result;
    }

    private void expand (final List<Plan> result, final Node node, final Condition condition, final ActionAchiever achiever)
    {
	achiever.makeChild ();
	final Plan child = achiever.getChild ();
	final Map<Node, Node> nodeMap = achiever.getNodeMap ();
	// Make a new node for the action
	final Action action = achiever.getAction ();
	final Symbol actionNodeName = action.getName ().gensym ();
	final Node a = new Node (actionNodeName);
	a.setAction (action);
	child.addNode (a);
	nodeMap.get (getInitialNode ()).addSuccessor (a);
	// Add a causal link from the action node to the goal node
	final ProtectionInterval pi = a.addPI (condition, nodeMap.get (node));
	child.revisionGoal = pi;
	child.revisionSupport = action;
	final Bindings match = achiever.getBindings ();
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
	final LinkedList<List<Pair<Node, Node>>> partialResolutions = new LinkedList<List<Pair<Node, Node>>> ();
	for (final PIConflict conflict : conflicts)
	{
	    partialResolutions.add (conflict.getResolutions ());
	}
	final List<List<Pair<Node, Node>>> fullResolutions = computeCombinations (partialResolutions);
	for (final List<Pair<Node, Node>> resolutions : fullResolutions)
	{
	    for (final Pair<Node, Node> resolution : resolutions)
	    {
		final Node before = resolution.getFirst ();
		final Node after = resolution.getSecond ();
		final LinkAchiever achiever = new LinkAchiever (this, before, after);
		achiever.makeChild ();
		final Plan child = achiever.getChild ();
		child.revisionGoal = conflicts;
		child.revisionSupport = resolution;
		final Map<Node, Node> nodeMap = achiever.getNodeMap ();
		final Node nodeCopy = nodeMap.get (before);
		final Node fromCopy = nodeMap.get (after);
		nodeCopy.addSuccessor (fromCopy);
		result.add (child);
	    }
	}
    }

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

    /** The parent plan, if any, otherwise null. */
    public Plan getParent ()
    {
	return planAchiever.getParent ();
    }

    // /** Name of the parent plan, if any, otherwise null. */
    // public Symbol getParentName ()
    // {
    // if (parent != null)
    // {
    // return parent.getName ();
    // }
    // return null;
    // }

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

    /** Expand method for best first search algorithm. */
    @Override
    public Map<ProblemState, Double> expand ()
    {
	final Map<ProblemState, Double> result = new HashMap<ProblemState, Double> ();
	final List<Plan> childPlans = expandPlan ();
	for (final Plan plan : childPlans)
	{
	    double cost = 0.1;
	    if (plan.revisionSupport != null)
	    {
		if (plan.revisionSupport instanceof Action)
		{
		    cost += 1;
		}
	    }
	    result.put (plan, cost);
	}
	return result;
    }

    /** Cost of using this plan for search purposes. */
    public double getCost ()
    {
	final Plan parent = planAchiever.getParent ();
	if (parent == null)
	{
	    return planAchiever.getIncrementCost ();
	}
	else
	{
	    return parent.getCost () + planAchiever.getIncrementCost ();
	}
    }

    public double getIncrementCost ()
    {
	return planAchiever.getIncrementCost ();
    }

    /** Heuristic estimate for best first search algorithm. */
    @Override
    public double estimateRemainingCost ()
    {
	double result = 0;
	for (final Node node : nodes)
	{
	    result += node.getGoalConditions ().size ();
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
	result.put ("Achiever", planAchiever);
	// result.put ("Parent", parent);
	// result.put ("Children", children);
	return result;
    }
}
