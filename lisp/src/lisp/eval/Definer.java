
package lisp.eval;

import lisp.Package;
import lisp.Symbol;

/** Helper class (used as a base class) for defining functions. */
public class Definer extends BasicDefiner
{
    private final Package pkg;

    public Definer (final Package pkg, final Object source)
    {
	super (source);
	this.pkg = pkg;
    }

    public Definer (final Package pkg)
    {
	super ();
	this.pkg = pkg;
    }

    /**
     * Bind a symbol to a java implementation method. <br>
     * [TODO] Should be able to specify the number and type of the arguments.
     *
     * @param symbolName
     * @param methodName
     */
    public void define (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.internPublic (symbolName);
	define (symbol, methodName);
    }

    /**
     * Bind a symbol to a java implementation method. <br>
     * The method must be a unique local method of the source class and the number and type of
     * arguments come from it.
     *
     * @param symbolName
     * @param methodName
     */
    public void defineTyped (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.internPublic (symbolName);
	defineTyped (symbol, methodName);
    }

    /**
     * Bind a symbol to a java implementation method for a special form. <br>
     * [TODO] Should be able to specify the number and type of the arguments.
     *
     * @param symbolName
     * @param methodName
     */
    public void defspecial (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.internPublic (symbolName);
	defspecial (symbol, methodName);
    }

    /**
     * Bind a symbol to a java implementation method for a macro. <br>
     * [TODO] Should be able to specify the number and type of the arguments.
     *
     * @param symbolName
     * @param methodName
     */
    public void defmacro (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.internPublic (symbolName);
	defmacro (symbol, methodName);
    }

    /** Package functions are defined in. */
    public Package getPackage ()
    {
	return pkg;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (pkg);
	buffer.append (">");
	return buffer.toString ();
    }
}
