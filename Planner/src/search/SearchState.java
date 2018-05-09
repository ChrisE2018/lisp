
package search;

import java.util.*;
import java.util.Map.Entry;

public class SearchState implements Comparable<SearchState>
{
    /** Previous search state. */
    private final SearchState parentState;

    /** Domain problem state. */
    private final ProblemState problemState;

    /** Actual cost so far. */
    private double cost;

    /** Estimated total cost to reach the goal. */
    private final double estimate;

    public SearchState (final SearchState parentState, final ProblemState problemState, final double increment)
    {
	this.parentState = parentState;
	this.problemState = problemState;
	if (parentState == null)
	{
	    cost = increment;
	}
	else
	{
	    cost = parentState.getCost () + increment;
	}
	estimate = cost + problemState.estimate ();
    }

    public ProblemState getProblemState ()
    {
	return problemState;
    }

    public SearchState getParentState ()
    {
	return parentState;
    }

    public double getCost ()
    {
	return cost;
    }

    public boolean solved ()
    {
	return problemState.solved ();
    }

    public List<SearchState> expand ()
    {
	final List<SearchState> result = new ArrayList<SearchState> ();
	final Map<ProblemState, Double> children = problemState.expand ();
	for (final Entry<ProblemState, Double> entry : children.entrySet ())
	{
	    final ProblemState child = entry.getKey ();
	    final double increment = entry.getValue ();
	    final SearchState s = new SearchState (this, child, increment);
	    result.add (s);
	}
	return result;
    }

    @Override
    public int compareTo (final SearchState o)
    {
	if (estimate < o.estimate)
	{
	    return -1;
	}
	if (estimate > o.estimate)
	{
	    return 1;
	}
	return 0;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (problemState);
	buffer.append (" ");
	buffer.append (cost);
	buffer.append (" ");
	buffer.append (estimate);
	buffer.append (">");
	return buffer.toString ();
    }
}
