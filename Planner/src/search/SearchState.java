
package search;

import java.util.*;
import java.util.Map.Entry;

public class SearchState implements Comparable<SearchState>
{
    /** Previous search state. */
    private final SearchState parentState;

    /** Domain problem state. */
    private final ProblemState problemState;

    /** Incremental cost from parent. */
    private final double increment;

    /** Actual cost so far. */
    private double cost;

    /** Estimated total cost to reach the goal. */
    private final double estimate;

    public SearchState (final SearchState parentState, final ProblemState problemState, final double increment)
    {
	this.parentState = parentState;
	this.problemState = problemState;
	this.increment = increment;
	if (parentState == null)
	{
	    cost = increment;
	}
	else
	{
	    cost = parentState.getCost () + increment;
	}
	estimate = cost + problemState.estimateRemainingCost ();
	problemState.setSearchState (this);
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

    public double getEstimate ()
    {
	return estimate;
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
	buffer.append (dd (cost));
	buffer.append ("=");
	if (parentState != null)
	{
	    buffer.append (dd (parentState.getCost ()));
	    buffer.append ("+");
	}
	buffer.append (dd (increment));
	buffer.append (" ?");
	buffer.append (dd (estimate));
	buffer.append (" ");
	buffer.append (problemState);
	buffer.append (">");
	return buffer.toString ();
    }

    private String dd (final double d)
    {
	return String.format ("%.1f", d);
    }
}
