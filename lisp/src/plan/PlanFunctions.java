
package plan;

import java.util.*;

import lisp.*;

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
	define ("plan", "createPlan");
    }

    public Object defstateEvaluator (final List<Object> arguments)
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

    private final Symbol NOT_SYMBOL = getPackage ().intern ("not");
    private final Symbol PRECONDITION_SYMBOL = getPackage ().intern ("precondition");
    private final Symbol POSTCONDITION_SYMBOL = getPackage ().intern ("postcondition");

    public Object defactionEvaluator (final List<Object> arguments)
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
	final Map<Symbol, Symbol> bindings = new HashMap<Symbol, Symbol> ();
	dte (result, state, arguments, 1, bindings);
	return result;
    }

    private void dte (final LispList result, final State state, final List<Object> arguments, final int i,
            final Map<Symbol, Symbol> bindings)
    {
	if (i < arguments.size ())
	{
	    final LispList pattern = (LispList)arguments.get (i);
	    for (final Object fact : state.getFacts ())
	    {
		final Map<Symbol, Symbol> b = matcher.match (pattern, (LispList)fact, bindings);
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

    public Object createPlan (final List<Object> arguments)
    {
	final Symbol name = coerceSymbol (arguments.get (0), true);
	Plan parent = null;
	if (arguments.size () > 1)
	{
	    parent = (Plan)arguments.get (1);
	}
	final Plan plan = new Plan (name, parent);
	name.setValue (plan);
	return name;
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
