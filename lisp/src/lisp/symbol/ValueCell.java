/**
 * Copyright © 2018 Christopher Eliot.
 * All rights reserved.
 */

package lisp.symbol;

public interface ValueCell
{
    /** Get the current stored value. */
    public Object getValue ();

    /**
     * Change the current stored value. CONSIDER Should this return the previous value?
     */
    public void setValue (Object value);

    /** Allowed value types. */
    public Class<?> getValueType ();
}
