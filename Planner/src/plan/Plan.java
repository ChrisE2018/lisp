
package plan;

import java.util.*;

import lisp.lang.*;
import lisp.util.MultiMap;
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

    public List<OpenGoalCondition> getOpenGoals ()
    {
	final List<OpenGoalCondition> result = new ArrayList<OpenGoalCondition> ();
	for (final Node node : nodes)
	{
	    if (node.hasOpenSubgoals ())
	    {
		for (final Condition goal : node.getGoalConditions ())
		{
		    result.add (new OpenGoalCondition (node, goal));
		}
	    }
	}
	return result;
    }

    public List<Plan> expandPlan ()
    {
	System.out.printf ("%nExpanding %s %n", this);
	OpenGoalCondition bestGoal = null;
	final List<OpenGoalCondition> goals = getOpenGoals ();
	for (final OpenGoalCondition goal : goals)
	{
	    final List<Achiever> achievers = goal.getAchievers ();
	    getLinkAchievers (achievers, goal.getNode (), goal.getCondition ());
	    getActionAchievers (achievers, goal.getNode (), goal.getCondition ());
	    if (bestGoal == null || achievers.size () < bestGoal.getAchievers ().size ())
	    {
		bestGoal = goal;
	    }
	}
	System.out.printf ("Expanding %s %n", bestGoal);
	final List<Achiever> achievers = bestGoal.getAchievers ();
	final List<Plan> result = new ArrayList<Plan> ();
	for (final Achiever achiever : achievers)
	{
	    final ProtectionInterval pi = achiever.expand ();
	    final Plan child = achiever.getChild ();
	    child.revisionGoal = pi;

	    final List<PIConflict> conflicts = child.getConflicts (pi);
	    if (conflicts.size () == 0)
	    {
		result.add (child);
	    }
	    else
	    {
		child.resolveConflicts (result, conflicts);
	    }
	}
	if (result.isEmpty ())
	{
	    System.out.printf ("Failure Expanding %s %n", bestGoal);
	}
	return result;
    }

    // public List<Plan> expandPlanx ()
    // {
    // System.out.printf ("%nExpanding %s %n", this);
    // final List<Plan> result = new ArrayList<Plan> ();
    // for (final Node node : nodes)
    // {
    // if (node.hasOpenSubgoals ())
    // {
    // for (final Condition goal : node.getGoalConditions ())
    // {
    // System.out.printf ("Expanding %s: %s %n", node, goal);
    // final List<Achiever> achievers = new ArrayList<Achiever> ();
    // getLinkAchievers (achievers, node, goal);
    // getActionAchievers (achievers, node, goal);
    // for (final Achiever achiever : achievers)
    // {
    // final ProtectionInterval pi = achiever.expand ();
    // final Plan child = achiever.getChild ();
    // child.revisionGoal = pi;
    //
    // final List<PIConflict> conflicts = child.getConflicts (pi);
    // if (conflicts.size () == 0)
    // {
    // result.add (child);
    // }
    // else
    // {
    // child.resolveConflicts (result, conflicts);
    // }
    // }
    // if (result.isEmpty ())
    // {
    // System.out.printf ("Failure Expanding %s: %s %n", node, goal);
    // }
    // return result;
    // }
    // }
    // }
    // return result;
    // }

    public void getLinkAchievers (final List<Achiever> result, final Node goalNode, final Condition goalCondition)
    {
	for (final Node possibleAchiever : nodes)
	{
	    if (possibleAchiever != goalNode && !possibleAchiever.after (goalNode))
	    {
		final List<Bindings> binds = possibleAchiever.causalBindings (goalCondition);
		if (binds != null)
		{
		    for (final Bindings b : binds)
		    {
			result.add (new LinkAchiever (this, goalNode, goalCondition, possibleAchiever, b));
		    }
		}
	    }
	}
    }

    private void getActionAchievers (final List<Achiever> result, final Node goalNode, final Condition goalCondition)
    {
	final List<Action> actions = Action.getActions ();
	for (final Action action : actions)
	{
	    final Bindings match = action.canAchieve (goalCondition);
	    if (match != null)
	    {
		final Bindings anonymousBindings = getAnonymousActionBindings (action);
		final Action anonymousAction = getAnonymousAction (action, anonymousBindings);
		final Bindings rebind = anonymousAction.canAchieve (goalCondition);
		result.add (new ActionAchiever (this, goalNode, goalCondition, anonymousAction, rebind, 2.0));
	    }
	}
    }

    private Action getAnonymousAction (final Action action, final Bindings bindings)
    {
	final List<Condition> precondition = new ArrayList<Condition> ();
	final List<Condition> postcondition = new ArrayList<Condition> ();
	for (final Condition c : action.getPrecondition ())
	{
	    precondition.add (c.bind (bindings));
	}
	for (final Condition c : action.getPostcondition ())
	{
	    postcondition.add (c.bind (bindings));
	}
	return new Action (action.getName ().gensym (), precondition, postcondition);
    }

    private Bindings getAnonymousActionBindings (final Action action)
    {
	final Bindings bindings = new Bindings ();
	anonymousVariables (bindings, action.getPrecondition ());
	anonymousVariables (bindings, action.getPostcondition ());
	return bindings;
    }

    private void anonymousVariables (final Bindings bindings, final List<Condition> conditions)
    {
	for (final Condition c : conditions)
	{
	    for (final Symbol s : c.getTerms ())
	    {
		if (s.isVariable ())
		{
		    final Symbol value = bindings.get (s);
		    if (value == null)
		    {
			bindings.put (s, s.gensym ());
		    }
		}
	    }
	}
    }

    private List<PIConflict> getConflicts (final ProtectionInterval pi)
    {
	final List<PIConflict> conflicts = new ArrayList<PIConflict> ();
	final Condition c = pi.getCondition ();
	final Node from = pi.getAchiever ();
	final Node to = pi.getProtectedNode ();
	for (final Node n : nodes)
	{
	    if (n != to)
	    {
		if (n.conflicts (c))
		{
		    // [TODO] If conditions in n or c itself have variables, then
		    // neq (not equal) constraints can be used to deconflict.
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
	return conflicts;
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
		final DeconflictAchiever achiever = new DeconflictAchiever (this, after, before);
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
    public MultiMap<String, Object> getDescriberValues (final Object target)
    {
	final MultiMap<String, Object> result = new MultiMap<> ();
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
