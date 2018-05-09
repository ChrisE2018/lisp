
package plan;

import java.awt.Rectangle;
import java.util.*;

import lisp.*;
import lisp.eval.*;
import plan.gui.*;

public class PlanFunctions extends Definer
{
    private final Matcher matcher = new Matcher ();

    public static void initialize () throws NoSuchMethodException, SecurityException
    {
	Primitives.initialize ();
	final PlanFunctions planFunctions = new PlanFunctions ();
	planFunctions.defineLispFunctions ();
    }

    private PlanFunctions ()
    {
	super (PackageFactory.getPackage ("user"));
    }

    private void defineLispFunctions () throws NoSuchMethodException, SecurityException
    {
	defspecial ("defstate", "defstateEvaluator");
	defspecial ("defaction", "defactionEvaluator");
	define ("match", "matchEvaluator");
	define ("determine-truth1", "determineTruth1Evaluator");
	define ("determine-truth", "determineTruthEvaluator");
	define ("node", "createNode");
	defspecial ("plan", "createPlan");
	define ("layout", "createPlanLayoutEvaluator");
	define ("view", "createPlanViewEvaluator");
    }

    /**
     * @param interpreter
     */
    public Object defstateEvaluator (final Interpreter interpreter, final List<Object> arguments)
    {
	final Symbol name = (Symbol)arguments.get (1);
	final LispList facts = new LispList ();
	for (int i = 2; i < arguments.size (); i++)
	{
	    facts.add (arguments.get (i));
	}
	final State state = new State (facts);
	name.setValue (state);
	return name;
    }

    private final Symbol NOT_SYMBOL = getPackage ().internPublic ("not");
    private final Symbol PRECONDITION_SYMBOL = getPackage ().internPublic ("precondition");
    private final Symbol POSTCONDITION_SYMBOL = getPackage ().internPublic ("postcondition");

    /**
     * @param interpreter
     */
    public Object defactionEvaluator (final Interpreter interpreter, final List<Object> arguments)
    {
	final Symbol name = (Symbol)arguments.get (1);
	final List<Condition> preconditions = new ArrayList<Condition> ();
	final List<Condition> postconditions = new ArrayList<Condition> ();
	for (int i = 2; i < arguments.size (); i++)
	{
	    final LispList arg = (LispList)arguments.get (i);
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
	}
	final Action action = new Action (name, preconditions, postconditions);
	name.setValue (action);
	return name;
    }

    private Condition getCondition (final boolean negated, final List<?> condition)
    {
	if (condition.get (0) == NOT_SYMBOL)
	{
	    final List<?> subcondition = (List<?>)condition.get (1);
	    return getCondition (!negated, subcondition);
	}
	else
	{
	    final Symbol predicate = (Symbol)condition.get (0);
	    @SuppressWarnings ("unchecked")
	    final List<Symbol> terms = (List<Symbol>)condition.subList (1, condition.size ());
	    return new Condition (negated, predicate, terms);
	}
    }

    public Object matchEvaluator (final List<Object> arguments)
    {
	final LispList pattern = (LispList)arguments.get (0);
	final LispList literal = (LispList)arguments.get (1);
	final Map<Symbol, Symbol> bindings = matcher.match (pattern, literal);
	final LispList result = matcher.bindingsToLisp (bindings);
	return result;
    }

    public Object determineTruth1Evaluator (final List<Object> arguments)
    {
	final State state = (State)arguments.get (0);
	final LispList pattern = (LispList)arguments.get (1);
	LispList result = null;
	for (final Object fact : state.getFacts ())
	{
	    final Map<Symbol, Symbol> bindings = matcher.match (pattern, (LispList)fact);
	    if (bindings != null)
	    {
		System.out.printf ("Bindings %s %n", bindings);
		final Object binds = matcher.bindingsToLisp (bindings);
		if (result == null)
		{
		    result = new LispList ();
		}
		result.add (binds);
	    }
	}

	return result;
    }

    public Object determineTruthEvaluator (final List<Object> arguments)
    {
	final LispList result = new LispList ();
	final State state = (State)arguments.get (0);
	final Bindings bindings = new Bindings ();
	dte (result, state, arguments, 1, bindings);
	return result;
    }

    private void dte (final LispList result, final State state, final List<Object> arguments, final int i,
            final Bindings bindings)
    {
	if (i < arguments.size ())
	{
	    final LispList pattern = (LispList)arguments.get (i);
	    for (final Object fact : state.getFacts ())
	    {
		final Bindings b = matcher.match (pattern, (LispList)fact, bindings);
		if (b != null)
		{
		    dte (result, state, arguments, i + 1, b);
		}
	    }
	}
	else
	{
	    final Object binds = matcher.bindingsToLisp (bindings);
	    result.add (binds);
	}
    }

    public Object createNode (final List<Object> arguments)
    {
	final Symbol name = coerceSymbol (arguments.get (0), true);
	final Node node = new Node (name);
	name.setValue (node);
	return name;
    }

    /**
     * Create a plan from nodes and before link expressions.
     *
     * @param interpreter
     */
    public Object createPlan (final Interpreter interpreter, final List<Object> arguments)
    {
	final Symbol name = coerceSymbol (arguments.get (1), true);
	final Plan plan = new Plan (null, name);
	name.setValue (plan);
	System.out.printf ("Create plan %s %n", name);
	for (final Object c : arguments.subList (2, arguments.size ()))
	{
	    @SuppressWarnings ("unchecked")
	    final List<Object> clause = (List<Object>)c;
	    final Symbol key = coerceSymbol (clause.get (0), true);
	    if (key.is ("node"))
	    {
		// System.out.printf (" Node %s %n", clause);
		final Node node = createPlanNode (clause);
		System.out.printf ("  Node %s %n", node);
		plan.addNode (node);
	    }
	    else if (key.is ("before"))
	    {
		final Symbol c1 = coerceSymbol (clause.get (1), true);
		final Symbol c2 = coerceSymbol (clause.get (2), true);
		final Node n1 = plan.getNode (c1);
		final Node n2 = plan.getNode (c2);
		System.out.printf ("  Before %s %s %n", n1.getName (), n2.getName ());
		n1.addSuccessor (n2);
	    }
	    else
	    {
		throw new IllegalArgumentException ("Unknown clause in plan definition: " + clause);
	    }
	}
	System.out.printf ("Plan %s %n", plan);
	return plan;
    }

    private Node createPlanNode (final List<Object> nodeDef)
    {
	final Symbol name = coerceSymbol (nodeDef.get (1), true);
	// System.out.printf (" Plan node %s %n", name);
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
		    // System.out.printf (" Add %s %n", condition);
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
		    // System.out.printf (" Delete %s %n", condition);
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
		    // System.out.printf (" Goal %s %n", condition);
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

    public Object createPlanLayoutEvaluator (final List<Object> arguments)
    {
	final Plan plan = (Plan)arguments.get (0);
	final PlanLayout planLayout = new PlanLayout ();
	final Rectangle r = new Rectangle (0, 0, 700, 400);
	final List<Sprite> result = planLayout.getLayout (plan, r);
	return result;
    }

    public Object createPlanViewEvaluator (final List<Object> arguments)
    {
	final Plan plan = (Plan)arguments.get (0);
	PlanView.makeView (plan);
	return plan;
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
