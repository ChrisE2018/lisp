
package lisp;

import java.lang.reflect.Method;
import java.util.List;

public class Definer
{
    private final Package pkg;
    private final Object source;

    public Definer (final Package pkg, final Object source)
    {
	this.pkg = pkg;
	this.source = source;
    }

    public void define (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new StandardFunctionCell (source, method));
    }

    public void defspecial (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new SpecialFunctionCell (source, method));
    }

    public void defmacro (final String symbolName, final String methodName) throws NoSuchMethodException, SecurityException
    {
	final Symbol symbol = pkg.intern (symbolName);
	final Method method = source.getClass ().getMethod (methodName, List.class);
	symbol.setFunction (new MacroFunctionCell (source, method));
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
