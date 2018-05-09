
package search;

import java.util.*;

/** Breadth first search. */
public class BreadthFirstSearch
{
    private final List<SearchState> open = new ArrayList<SearchState> ();
    private final List<SearchState> closed = new ArrayList<SearchState> ();
    private final List<SearchState> solutions = new ArrayList<SearchState> ();

    private final int searchLimit = 100;
    private int count = 0;

    public SearchState solve ()
    {
	while (open.size () > 0)
	{
	    final SearchState state = open.remove (0);
	    closed.add (state);
	    if (state.solved ())
	    {
		solutions.add (state);
		System.out.printf ("Solution %s found %n", state);
		// return state;
	    }
	    count++;
	    if (count > searchLimit)
	    {
		System.out.printf ("Search limit %s exceeded %n", searchLimit);
		return null;
	    }
	    System.out.printf ("[%d] Expanding %s %n", count, state);
	    final List<SearchState> children = state.expand ();
	    for (final SearchState child : children)
	    {
		if (!filterPreviousState (child))
		{
		    open.add (0, child);
		    System.out.printf ("   Child %s %n", child);
		}
	    }
	}
	return null;
    }

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

    public void add (final ProblemState state)
    {
	open.add (new SearchState (null, state, 0));
    }

    public List<SearchState> getSolutions ()
    {
	return solutions;
    }

    public SearchState getBestSolution ()
    {
	SearchState result = null;
	for (final SearchState solution : solutions)
	{
	    if (result == null || solution.getCost () < result.getCost ())
	    {
		result = solution;
	    }
	}
	return result;
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

// (defun bfs ()
// (make-demo)
// (setq *solution-state* nil)
// (loop with limit = 500
// with count = 0
// with closed = nil
// while (not (null *open*))
// for state = (pop *open*)
// do
// (push state closed)
// (when (solved state)
// (format t "~VTSolution ~s found in ~s steps~&~%" (level state) (name state) count)
// (setq *solution-state* state)
// (print-solution state)
// (return '*))
// (incf count)
// (when (> count limit)
// (break "Search limit exceeded")
// (incf limit 500))
// (setq *open* (nconc (expand state) *open*))
// (setq *open* (sort *open* #'< :key #'quality))
// (format t "~VTStates open: ~s expanded: ~s ~&"
// (level state) (length *open*) count)
// (when (and (< (length *open*) 6) (< (length closed) 6))
// (format t "~VTStates open: ~s expanded: ~s ~&"
// (level state)
// (loop for i upfrom 1
// while (< i 6)
// for s in *open*
// collect (name s))
// (loop for i upfrom 1
// while (< i 6)
// for s in closed
// collect (name s))))
// ))
