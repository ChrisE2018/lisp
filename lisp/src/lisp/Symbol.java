
package lisp;

import java.util.*;

import lisp.eval.FunctionCell;

/** Unique named structure associated with a package. */
public class Symbol implements Describer
{
    /** Character to separate a package prefix from a symbol name. */
    public static final char PACKAGE_SEPARATOR = ':';

    /** The package containing this symbol. It may be public (external) or private (internal). */
    private final Package symbolPackage;

    /** The name of the symbol. */
    private final String symbolName;

    /** The global value of the symbol. */
    private Object symbolValue;

    /** The global function definition for a symbol. */
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

    /** The package containing this symbol. It may be public (external) or private (internal). */
    public Package getPackage ()
    {
	return symbolPackage;
    }

    /** The name of the symbol. */
    public String getName ()
    {
	return symbolName;
    }

    /** The name of the symbol. */
    public boolean is (final String s)
    {
	return symbolName.equals (s);
    }

    /** The global value of the symbol. */
    public Object getValue ()
    {
	return symbolValue;
    }

    /** The global value of the symbol. */
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

    /** The global function definition for a symbol. */
    public FunctionCell getFunction ()
    {
	return symbolFunction;
    }

    /** The global function definition for a symbol. */
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

    /** Symbol properties with lazy initialization. */
    public Object get (final Symbol key)
    {
	Object result = null;
	if (symbolPlist != null)
	{
	    result = symbolPlist.get (key);
	}
	return result;
    }

    /** Symbol properties with lazy initialization. */
    public void put (final Symbol key, final Object val)
    {
	if (symbolPlist == null)
	{
	    symbolPlist = new HashMap<Symbol, Object> ();
	}
	symbolPlist.put (key, val);
    }

    /** Symbol properties with lazy initialization. */
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

    public Symbol gensym ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (symbolName);
	int length = buffer.length ();
	while (Character.isDigit (buffer.charAt (length - 1)))
	{
	    buffer.setLength (--length);
	}
	for (int i = 1; true; i++)
	{
	    buffer.append (i);
	    final String name = buffer.toString ();
	    final Symbol oldSymbol = symbolPackage.findPrivate (name);
	    if (oldSymbol == null)
	    {
		return symbolPackage.internPrivate (name);
	    }
	    buffer.setLength (length);
	}
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
