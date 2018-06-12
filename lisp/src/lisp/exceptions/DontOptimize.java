
package lisp.exceptions;

/**
 * This can be thrown by optimization methods that are called for complex cases, provided that no
 * code has already been output. The compiler will assume that the optimization was not possible and
 * proceed to produce normal, unoptimized code.
 *
 * @author cre
 */
public class DontOptimize extends Throwable
{
    public DontOptimize ()
    {
	super ("Attempted optimization wasn't possible. Normal compilation is expected to proceed.");
    }

    public DontOptimize (final String format, final Object... args)
    {
	super (String.format (format, args));
    }
}
