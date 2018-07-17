
package plan.gui;

import java.util.Collection;

import lisp.lang.Symbol;
import plan.Condition;

public interface World
{
    public void setSimulator (final Simulator simulator);

    public int getBlockSpacing ();

    public void setBlockSpacing (final int blockSpacing);

    public int getBlockWidth ();

    public void setBlockWidth (final int blockWidth);

    public int getBlockHeight ();

    public void setBlockHeight (final int blockHeight);

    public int getTableY ();

    public void setTableY (final int tableY);

    /** Determine if a predicate can change value as a result of plan actions. */
    public boolean canChange (Condition c);

    /** Get displayable simulation actions from a condition. */
    public Collection<? extends Symbol> getObjects (Condition c);

    /** Change display to depict a condition becoming true. */
    public void animateCondition (final Condition c);
}
