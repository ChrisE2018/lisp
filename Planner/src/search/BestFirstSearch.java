
package search;

import java.util.*;

/** Best first search. */
public class BestFirstSearch
{
    private final PriorityQueue<SearchState> open = new PriorityQueue<SearchState> ();
    private final List<SearchState> closed = new ArrayList<SearchState> ();
    private final PriorityQueue<SearchState> solutions = new PriorityQueue<SearchState> ();

    private Integer searchLimit = null;
    private int count = 0;

    /**
     * Turn on tracing. [TODO] Use a logger.
     */
    private boolean trace = false;

    public SearchState solve ()
    {
	while (open.size () > 0)
	{
	    final SearchState state = open.poll ();
	    closed.add (state);
	    if (state.solved ())
	    {
		solutions.add (state);
		if (trace)
		{
		    System.out.printf ("Solution %s found %n", state);
		    // return state;
		}
	    }
	    else
	    {
		if (trace)
		{
		    System.out.printf ("[%d] Expanding %s %n", count, state);
		}
		final List<SearchState> children = state.expand ();
		for (final SearchState child : children)
		{
		    if (!filterPreviousState (child))
		    {
			open.add (child);
			if (trace)
			{
			    System.out.printf ("   Child %s %n", child);
			}
		    }
		}
		// System.out.printf ("[%d] Open %s %n", count, open);
	    }
	    count++;
	    if (searchLimit != null)
	    {
		if (count > searchLimit)
		{
		    System.out.printf ("Search limit %s exceeded %n", searchLimit);
		    return null;
		}
	    }
	}
	return getBestSolution ();
    }

    /** Reject any ProblemState that has already been reached by a lower cost path. */
    private boolean filterPreviousState (final SearchState child)
    {
	final ProblemState p = child.getProblemState ();
	final double cost = child.getCost ();
	for (final SearchState s : closed)
	{
	    if (p == s.getProblemState ())
	    {
		if (s.getCost () <= cost)
		{
		    return true;
		}
	    }
	}
	return false;
    }

    /** Add a ProblemState to the open list. */
    public void add (final ProblemState state)
    {
	open.add (new SearchState (null, state, 0));
    }

    /** Get the known solutions. */
    public PriorityQueue<SearchState> getSolutions ()
    {
	return solutions;
    }

    public SearchState getBestSolution ()
    {
	return solutions.peek ();
    }

    public void setSearchLimit (final Integer limit)
    {
	searchLimit = limit;
    }

    public void setTrace (final boolean trace)
    {
	this.trace = trace;
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
