
package search;

import java.util.Map;

public interface ProblemState
{
    /** Return true if this is a solution state. */
    public boolean solved ();

    /** Return a map from child states to the incremental cost of reaching each child. */
    public Map<ProblemState, Integer> expand ();

    /** Return an estimate of the cost of reaching a solution state. */
    public double estimate ();
}
