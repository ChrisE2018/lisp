
package lisp;

import java.util.*;

import lisp.eval.FunctionCell;

/** Unique named structure associated with a package. */
public class Symbol implements Describer
{
    /** Character to separate a package prefix from a symbol name. */
    public static final char PACKAGE_SEPARATOR = ':';

    private final Package symbolPackage;

    private final String symbolName;

    private Object symbolValue;

    private FunctionCell symbolFunction;

    /** Symbol properties with lazy initialization. */
    private Map<Symbol, Object> symbolPlist = null;

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

    public Object getValue ()
    {
	return symbolValue;
    }

    public void setValue (final Object value)
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
	if (symbolFunction != null)
	{
	    System.out.printf ("%%Refining %s\n", this);
	}
	symbolFunction = function;
    }

    /** When true it is illegal to set the function of this symbol. */
    public void setConstantFunction (final boolean constantFunction)
    {
	this.constantFunction = constantFunction;
    }

    public Object get (final Symbol key)
    {
	Object result = null;
	if (symbolPlist != null)
	{
	    result = symbolPlist.get (key);
	}
	return result;
    }

    public void put (final Symbol key, final Object val)
    {
	if (symbolPlist == null)
	{
	    symbolPlist = new HashMap<Symbol, Object> ();
	}
	symbolPlist.put (key, val);
    }

    public Object remove (final Symbol key)
    {
	Object result = null;
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
	if (symbolPackage != null && symbolPackage != PackageFactory.getDefaultPackage ())
	{
	    buffer.append (symbolPackage.getName ());
	    buffer.append (PACKAGE_SEPARATOR);
	}
	buffer.append (symbolName);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer.toString ();
    }

    @Override
    public Map<String, Object> getDescriberValues (final Object target)
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	result.put ("Package", symbolPackage);
	result.put ("Public", symbolPackage.isPublic (this));
	if (symbolValue != null)
	{
	    result.put ("Value", symbolValue);
	}
	if (symbolFunction != null)
	{
	    result.put ("Function", symbolFunction);
	}
	if (symbolPlist != null)
	{
	    result.put ("Plist", symbolPlist);
	}
	return result;
    }
}
