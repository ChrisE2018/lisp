
package plan;

import java.awt.Rectangle;
import java.util.*;
import java.util.logging.Logger;

import lisp.eval.*;
import lisp.lang.*;
import lisp.lang.Package;
import lisp.util.LogString;
import plan.gui.*;
import search.BestFirstSearch;

public class PlanFunctions extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (PlanFunctions.class.getName ());

    @SuppressWarnings ("unused")
    private static PlanFunctions planFunctions = new PlanFunctions ();

    public static void initialize ()
    {
    }

    private final Matcher matcher = new Matcher ();

    private final Package pkg = PackageFactory.getDefaultPackage ();

    private final Symbol PREDICATE_SYMBOL = pkg.internSymbol ("predicate");
    private final Symbol PRECONDITION_SYMBOL = pkg.internSymbol ("precondition");
    private final Symbol POSTCONDITION_SYMBOL = pkg.internSymbol ("postcondition");
    private final Symbol CONSTRAINTS_SYMBOL = pkg.internSymbol ("constraints");

    private PlanFunctions ()
    {
    }

    /**
     * @param context
     */
    @DefineLisp (special = true)
    public Object defpredicate (final LexicalContext context, final Symbol name, final Object... args)
    {
	name.put (PREDICATE_SYMBOL, args);
	return name;
    }

    /**
     * @param context
     */
    @DefineLisp (special = true)
    public Object defaction (final LexicalContext context, final Symbol name, final Object... body)
    {
	final List<Condition> preconditions = new ArrayList<Condition> ();
	final List<Condition> postconditions = new ArrayList<Condition> ();
	final List<Condition> constraints = new ArrayList<Condition> ();
	for (int i = 0; i < body.length; i++)
	{
	    final LispList arg = (LispList)body[i];
	    final Symbol key = coerceSymbol (arg.get (0), true);
	    if (key == PRECONDITION_SYMBOL)
	    {
		for (final Object c : arg.subList (1, arg.size ()))
		{
		    final Condition condition = getCondition (false, (List<?>)c);
		    preconditions.add (condition);
		}
	    }
	    else if (key == POSTCONDITION_SYMBOL)
	    {
		for (final Object c : arg.subList (1, arg.size ()))
		{
		    final Condition condition = getCondition (false, (List<?>)c);
		    postconditions.add (condition);
		}
	    }
	    else if (key == CONSTRAINTS_SYMBOL)
	    {
		for (final Object c : arg.subList (1, arg.size ()))
		{
		    final Condition condition = getCondition (false, (List<?>)c);
		    constraints.add (condition);
		}
	    }
	}
	final Action action = new Action (name, preconditions, postconditions, constraints);
	name.setValue (action);
	Action.getActions ().add (action);
	return name;
    }

    private Condition getCondition (final boolean negated, final List<?> condition)
    {
	final Symbol predicate = (Symbol)condition.get (0);
	if (predicate.is ("not"))
	{
	    final List<?> subcondition = (List<?>)condition.get (1);
	    return getCondition (!negated, subcondition);
	}
	else
	{

	    @SuppressWarnings ("unchecked")
	    final List<Symbol> terms = (List<Symbol>)condition.subList (1, condition.size ());
	    return new Condition (negated, predicate, terms);
	}
    }

    /**
     * Create a plan from nodes and before link expressions.
     *
     * @param context
     */
    @DefineLisp (special = true, name = "plan")
    public Object createPlan (final LexicalContext context, final Symbol name, final Object... body)
    {
	final Plan plan = new Plan (name);
	name.setValue (plan);
	LOGGER.info (new LogString ("Create plan %s", name));
	for (final Object c : body)
	{
	    @SuppressWarnings ("unchecked")
	    final List<Object> clause = (List<Object>)c;
	    final Symbol key = coerceSymbol (clause.get (0), true);
	    if (key.is ("node"))
	    {
		LOGGER.fine (new LogString (" Node %s", clause));
		final Node node = createPlanNode (clause);
		LOGGER.fine (new LogString ("  Node %s", node));
		plan.addNode (node);
	    }
	    else if (key.is ("before"))
	    {
		final Symbol c1 = coerceSymbol (clause.get (1), true);
		final Symbol c2 = coerceSymbol (clause.get (2), true);
		final Node n1 = plan.getNode (c1);
		final Node n2 = plan.getNode (c2);
		LOGGER.fine (new LogString ("  Before %s %s", n1.getName (), n2.getName ()));
		n1.addSuccessor (n2);
	    }
	    else
	    {
		throw new IllegalArgumentException ("Unknown clause in plan definition: " + clause);
	    }
	}
	LOGGER.info (new LogString ("Plan %s", plan));
	return plan;
    }

    private Node createPlanNode (final List<Object> nodeDef)
    {
	final Symbol name = coerceSymbol (nodeDef.get (1), true);
	LOGGER.fine (new LogString (" Plan node %s", name));
	final Node result = new Node (name);
	for (final Object c : nodeDef.subList (2, nodeDef.size ()))
	{
	    @SuppressWarnings ("unchecked")
	    final List<Object> clause = (List<Object>)c;
	    final Symbol key = coerceSymbol (clause.get (0), true);
	    if (key.is ("add"))
	    {
		for (final Object expression : clause.subList (1, clause.size ()))
		{
		    @SuppressWarnings ("unchecked")
		    final List<Object> expr = (List<Object>)expression;
		    final Condition condition = new Condition (expr);
		    LOGGER.finer (new LogString (" Add %s", condition));
		    result.getAddConditions ().add (condition);
		}
	    }
	    else if (key.is ("delete"))
	    {
		for (final Object expression : clause.subList (1, clause.size ()))
		{
		    @SuppressWarnings ("unchecked")
		    final List<Object> expr = (List<Object>)expression;
		    final Condition condition = new Condition (expr);
		    LOGGER.finer (new LogString (" Delete %s", condition));
		    result.getDeleteConditions ().add (condition);
		}
	    }
	    else if (key.is ("goal"))
	    {
		for (final Object expression : clause.subList (1, clause.size ()))
		{
		    @SuppressWarnings ("unchecked")
		    final List<Object> expr = (List<Object>)expression;
		    final Condition condition = new Condition (expr);
		    LOGGER.finer (new LogString (" Goal %s", condition));
		    result.getGoalConditions ().add (condition);
		}
	    }
	    else
	    {
		throw new IllegalArgumentException ("Unknown clause in node definition: " + clause);
	    }
	}
	name.setValue (result);
	return result;
    }

    @DefineLisp
    public LispList match (final List<Object> p, final List<Object> l)
    {
	final LispList pattern = new LispList (p);
	final LispList literal = new LispList (l);
	final Map<Symbol, Symbol> bindings = matcher.match (pattern, literal);
	final LispList result = matcher.bindingsToLisp (bindings);
	return result;
    }

    @DefineLisp
    public Node createNode (final Symbol name)
    {
	final Node node = new Node (name);
	name.setValue (node);
	return node;
    }

    @DefineLisp
    public List<Sprite> layout (final Plan plan)
    {
	final PlanLayout planLayout = new PlanLayout ();
	final Rectangle r = new Rectangle (0, 0, 700, 400);
	final List<Sprite> result = planLayout.getLayout (plan, r);
	return result;
    }

    @DefineLisp
    public Plan view (final Plan plan)
    {
	PlanView.makeView (plan);
	return plan;
    }

    @DefineLisp
    public Plan planTree (final Plan plan)
    {
	PlanTreeDemo.displayPlan (plan);
	return plan;
    }

    @DefineLisp
    public BestFirstSearch bfs (final Plan plan)
    {
	final BestFirstSearch result = new BestFirstSearch ();
	result.add (plan);
	return result;
    }

    @DefineLisp
    public Plan execute (final Plan plan)
    {
	if (plan != null)
	{
	    final World world = new Blocksworld ();
	    Simulator.makeView (world, plan);
	}
	return plan;
    }
}
