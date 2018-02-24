
package lisp;

public class Symbol implements Lisp
{
    private final Package pkg;

    private final String name;

    private Lisp value;

    private FunctionCell function;

    private Lisp plist;

    public Symbol (final String name)
    {
	pkg = null;
	this.name = name;
    }

    public Symbol (final Package pkg, final String name)
    {
	this.pkg = pkg;
	this.name = name;
    }

    public Package getPackage ()
    {
	return pkg;
    }

    public String getName ()
    {
	return name;
    }

    public Lisp getValue ()
    {
	return value;
    }

    public void setValue (final Lisp value)
    {
	this.value = value;
    }

    public FunctionCell getFunction ()
    {
	return function;
    }

    public void setFunction (final FunctionCell function)
    {
	this.function = function;
    }

    public Lisp getPlist ()
    {
	return plist;
    }

    public void setPlist (final Lisp plist)
    {
	this.plist = plist;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	// [TODO] Include package information
	buffer.append (name);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	if (pkg != null)
	{
	    buffer.append (pkg.getName ());
	    buffer.append ('.');
	}
	buffer.append (name);
	buffer.append (">");
	return buffer.toString ();
    }
}
