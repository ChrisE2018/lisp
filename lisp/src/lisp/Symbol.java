
package lisp;

import java.util.*;

public class Symbol implements Lisp
{
    private final Package symbolPackage;

    private final String symbolName;

    private Lisp symbolValue;

    private FunctionCell symbolFunction;

    /** Symbol properties with lazy initialization. */
    private Map<Symbol, Lisp> symbolPlist = null;

    /** When true it is illegal to set the value of this symbol. */
    private boolean constantValue = false;

    /** When true it is illegal to set the function of this symbol. */
    private boolean constantFunction = false;

    public Symbol (final String name)
    {
	symbolPackage = null;
	symbolName = name;
    }

    public Symbol (final Package pkg, final String name)
    {
	symbolPackage = pkg;
	symbolName = name;
    }

    public Package getPackage ()
    {
	return symbolPackage;
    }

    public String getName ()
    {
	return symbolName;
    }

    public Lisp getValue ()
    {
	return symbolValue;
    }

    public void setValue (final Lisp value)
    {
	if (constantValue)
	{
	    throw new IllegalStateException ("Symbol value is constant " + symbolName);
	}
	symbolValue = value;
    }

    /** When true it is illegal to set the value of this symbol. */
    public void setConstantValue (final boolean constantValue)
    {
	this.constantValue = constantValue;
    }

    public FunctionCell getFunction ()
    {
	return symbolFunction;
    }

    public void setFunction (final FunctionCell function)
    {
	if (constantFunction)
	{
	    throw new IllegalStateException ("Symbol function is constant " + symbolName);
	}
	symbolFunction = function;
    }

    /** When true it is illegal to set the function of this symbol. */
    public void setConstantFunction (final boolean constantFunction)
    {
	this.constantFunction = constantFunction;
    }

    public Lisp get (final Symbol key)
    {
	Lisp result = null;
	if (symbolPlist != null)
	{
	    result = symbolPlist.get (key);
	}
	return result;
    }

    public void put (final Symbol key, final Lisp val)
    {
	if (symbolPlist == null)
	{
	    symbolPlist = new HashMap<Symbol, Lisp> ();
	}
	symbolPlist.put (key, val);
    }

    public Lisp remove (final Symbol key)
    {
	Lisp result = null;
	if (symbolPlist != null)
	{
	    result = symbolPlist.remove (key);
	    // Could null plist here if it is empty, but it is likely to get populated again.
	}
	return result;
    }

    /**
     * Provide a method for iterating through plist entries.
     *
     * @return
     */
    public Set<Symbol> getKeys ()
    {
	return symbolPlist.keySet ();
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	if (symbolPackage != null)
	{
	    buffer.append (symbolPackage.getName ());
	    buffer.append ('.');
	}
	buffer.append (symbolName);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	if (symbolPackage != null)
	{
	    buffer.append (symbolPackage.getName ());
	    buffer.append ('.');
	}
	buffer.append (symbolName);
	buffer.append (">");
	return buffer.toString ();
    }
}
