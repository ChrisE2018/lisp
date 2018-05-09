
package plan;

import java.util.*;

import lisp.*;

public class Condition implements Describer
{
    private static final char QUESTION_MARK = '?';

    private final boolean negated;
    private final Symbol predicate;
    private final List<Symbol> terms;

    public Condition (final boolean negated, final Symbol predicate, final List<Symbol> terms)
    {
	this.negated = negated;
	this.predicate = predicate;
	this.terms = terms;
    }

    public Condition (final Symbol predicate, final List<Symbol> terms)
    {
	negated = false;
	this.predicate = predicate;
	this.terms = terms;
    }

    public Condition (final List<Object> expression)
    {
	List<Object> expr = expression;
	final Symbol start = (Symbol)expression.get (0);
	negated = start.is ("not");
	if (negated)
	{
	    @SuppressWarnings ("unchecked")
	    final List<Object> e = (List<Object>)expression.get (1);
	    expr = e;
	}
	predicate = (Symbol)expr.get (0);
	terms = new ArrayList<Symbol> ();
	for (final Object term : expr.subList (1, expr.size ()))
	{
	    terms.add ((Symbol)term);
	}
    }

    public boolean isNegated ()
    {
	return negated;
    }

    public Symbol getPredicate ()
    {
	return predicate;
    }

    public List<Symbol> getTerms ()
    {
	return terms;
    }

    public Condition negate ()
    {
	return new Condition (!negated, predicate, terms);
    }

    public boolean causes (final Condition c)
    {
	if (predicate == c.predicate)
	{
	    if (negated == c.negated)
	    {
		return same (terms, c.terms);
	    }
	}
	return false;
    }

    public boolean deleteCauses (final Condition c)
    {
	if (predicate == c.predicate)
	{
	    if (negated == !c.negated)
	    {
		return same (terms, c.terms);
	    }
	}
	return false;
    }

    public boolean conflicts (final Condition c)
    {
	return deleteCauses (c);
    }

    public boolean deleteConflicts (final Condition c)
    {
	return causes (c);
    }

    @Override
    public int hashCode ()
    {
	final int prime = 31;
	int result = 1;
	result = prime * result + (negated ? 1231 : 1237);
	result = prime * result + ((predicate == null) ? 0 : predicate.hashCode ());
	result = prime * result + ((terms == null) ? 0 : terms.hashCode ());
	return result;
    }

    @Override
    public boolean equals (final Object o)
    {
	if (o instanceof Condition)
	{
	    final Condition c = (Condition)o;
	    if (negated == c.negated)
	    {
		if (predicate == c.predicate)
		{
		    return same (terms, c.terms);
		}
	    }
	}
	return false;
    }

    private boolean same (final List<Symbol> a, final List<Symbol> b)
    {
	final int size = a.size ();
	if (size != b.size ())
	{
	    return false;
	}
	for (int i = 0; i < size; i++)
	{
	    final Symbol var = a.get (i);
	    final Symbol val = b.get (i);
	    if (var != val)
	    {
		return false;
	    }
	}
	return true;
    }

    public Bindings matches (final Condition condition)
    {
	final Bindings result = new Bindings ();
	if (negated != condition.negated)
	{
	    return null;
	}
	if (predicate != condition.predicate)
	{
	    return null;
	}
	final int size = terms.size ();
	if (size != condition.terms.size ())
	{
	    return null;
	}
	for (int i = 0; i < size; i++)
	{
	    final Symbol var = terms.get (i);
	    final Symbol val = condition.terms.get (i);
	    if (isVariable (var))
	    {
		final Symbol binding = result.get (var);
		if (binding == null)
		{
		    result.put (var, val);
		}
		else if (binding != val)
		{
		    // Fail
		    return null;
		}
	    }
	    else if (var != val)
	    {
		// Fail
		return null;
	    }
	}
	return result;
    }

    private boolean isVariable (final Symbol symbol)
    {
	return symbol.getName ().charAt (0) == QUESTION_MARK;
    }

    public Condition bind (final Bindings match)
    {
	return bind (false, match);
    }

    public Condition bind (final boolean negate, final Bindings match)
    {
	final List<Symbol> boundTerms = new ArrayList<Symbol> ();
	for (final Symbol s : terms)
	{
	    if (isVariable (s))
	    {
		Symbol bs = match.get (s);
		if (bs == null)
		{
		    bs = s;
		}
		boundTerms.add (bs);
	    }
	    else
	    {
		boundTerms.add (s);
	    }
	}

	return new Condition (negate ? !negated : negated, predicate, boundTerms);
    }

    public void print (final StringBuilder buffer)
    {
	if (negated)
	{
	    buffer.append ("(not ");
	}
	buffer.append ('(');
	buffer.append (predicate.getName ());
	for (final Symbol term : terms)
	{
	    buffer.append (' ');
	    buffer.append (term.getName ());
	}
	buffer.append (')');
	if (negated)
	{
	    buffer.append (')');
	}
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Negated", negated);
	result.put ("Predicate", predicate);
	result.put ("Terms", terms);
	return result;
    }
}