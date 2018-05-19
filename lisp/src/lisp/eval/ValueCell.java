
package lisp.eval;

public interface ValueCell
{
    /** Get the current stored value. */
    public Object getValue ();

    /**
     * Change the current stored value. [TODO] Should this return the previous value?
     */
    public void setValue (Object value);
}
