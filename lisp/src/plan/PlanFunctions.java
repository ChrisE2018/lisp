
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
    }

    public Object defstateEvaluator (final List<Object> arguments)
    {
	final Symbol name = (Symbol)arguments.get (1);
	final LispList facts = new LispParenList ();
	for (int i = 2; i < arguments.size (); i++)
	{
	    facts.add (arguments.get (i));
	}
	final State state = new State (facts);
	name.setValue (state);
	return name;
    }

    public Object defactionEvaluator (final List<Object> arguments)
    {
	final Symbol name = (Symbol)arguments.get (1);
	final LispList precondition = (LispList)arguments.get (2);
	final LispList postcondition = (LispList)arguments.get (3);
	final Action action = new Action (name, precondition, postcondition);
	name.setValue (action);
	return name;
    }

    public Object matchEvaluator (final List<Object> arguments)
    {
	final LispList pattern = (LispList)arguments.get (0);
	final LispList literal = (LispList)arguments.get (1);
	final Map<Symbol, Symbol> bindings = matcher.match (pattern, literal);
	final LispParenList result = matcher.bindingsToLisp (bindings);
	return result;
    }

    public Object determineTruth1Evaluator (final List<Object> arguments)
    {
	final State state = (State)arguments.get (0);
	final LispList pattern = (LispList)arguments.get (1);
	LispParenList result = null;
	for (final Object fact : state.getFacts ())
	{
	    final Map<Symbol, Symbol> bindings = matcher.match (pattern, (LispList)fact);
	    if (bindings != null)
	    {
		System.out.printf ("Bindings %s %n", bindings);
		final Object binds = matcher.bindingsToLisp (bindings);
		if (result == null)
		{
		    result = new LispParenList ();
		}
		result.add (binds);
	    }
	}

	return result;
    }

    public Object determineTruthEvaluator (final List<Object> arguments)
    {
	final LispParenList result = new LispParenList ();
	final State state = (State)arguments.get (0);
	final Map<Symbol, Symbol> bindings = new HashMap<Symbol, Symbol> ();
	dte (result, state, arguments, 1, bindings);
	return result;
    }

    private void dte (final LispParenList result, final State state, final List<Object> arguments, final int i,
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
