
package plan;

import java.util.List;

import lisp.Symbol;

public class Condition
{
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
}
