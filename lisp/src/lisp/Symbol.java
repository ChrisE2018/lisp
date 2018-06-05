
package lisp;

import java.util.*;

import javax.lang.model.type.NullType;

import lisp.eval.UnboundVariableError;
import lisp.symbol.*;

/** Unique named structure associated with a package. */
public class Symbol implements Describer
{
    private static final char QUESTION_MARK = '?';

    /** Character to separate a package prefix from a symbol name. */
    public static final char PACKAGE_SEPARATOR = '.';

    /** The package containing this symbol. It may be public (external) or private (internal). */
    private final Package symbolPackage;

    /** The name of the symbol. */
    private final String symbolName;

    /** The global value of the symbol. */
    private ValueCell symbolValue;

    /** The global function definition for a symbol. */
    private FunctionCell symbolFunction;

    /** Symbol properties with lazy initialization. */
    private Map<Symbol, Object> symbolPlist = null;

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

    /** Get the low level value cell for this symbol. The result may be null. */
    public ValueCell getValueCell ()
    {
	return symbolValue;
    }

    /**
     * Set the low level value cell for this symbol. The value may be null. No error checking is
     * performed. This is intended to support the interpreter saving and restoring global values
     * while binding function arguments.
     */
    public void setValueCell (final ValueCell value)
    {
	symbolValue = value;
    }

    /** The global value of the symbol. */
    public Object getValue ()
    {
	if (symbolValue != null)
	{
	    return symbolValue.getValue ();
	}
	// Throw an unbound variable exception here.
	throw new UnboundVariableError (this);
    }

    /** The global value of the symbol. */
    public Object getValue (final Object defaultValue)
    {
	if (symbolValue != null)
	{
	    return symbolValue.getValue ();
	}
	return defaultValue;
    }

    /** Determine if the value has been set. */
    public boolean hasValue ()
    {
	return symbolValue != null;
    }

    /** The global value of the symbol. */
    public void setValue (final Object value)
    {
	if (symbolValue != null)
	{
	    symbolValue.setValue (value);
	}
	else
	{
	    symbolValue = new SimpleValueCell (value);
	}
    }

    /**
     * If the current value of this symbol exists and can be coerced to an boolean, return that.
     * Otherwise return the defaultValue.
     *
     * @param defaultValue
     * @return
     */
    public boolean getBooleanValue (final boolean defaultValue)
    {
	if (symbolValue != null)
	{
	    final Object value = symbolValue.getValue ();
	    if (value instanceof Boolean)
	    {
		return (Boolean)value;
	    }
	}
	return defaultValue;
    }

    /**
     * If the current value of this symbol exists and can be coerced to an int, return that.
     * Otherwise return the defaultValue.
     *
     * @param defaultValue
     * @return
     */
    public int getIntValue (final int defaultValue)
    {
	if (symbolValue != null)
	{
	    final Object value = symbolValue.getValue ();
	    if (value instanceof Integer)
	    {
		return (int)value;
	    }
	}
	return defaultValue;
    }

    /**
     * If the current value of this symbol exists and can be coerced to an double, return that.
     * Otherwise return the defaultValue.
     *
     * @param defaultValue
     * @return
     */
    public double getDoubleValue (final double defaultValue)
    {
	if (symbolValue != null)
	{
	    final Object value = symbolValue.getValue ();
	    if (value instanceof Double)
	    {
		return (double)value;
	    }
	}
	return defaultValue;
    }

    /**
     * If the current value of this symbol exists and can be coerced to a String, return that.
     * Otherwise return the defaultValue.
     *
     * @param defaultValue
     * @return
     */
    public String getStringValue (final String defaultValue)
    {
	if (symbolValue != null)
	{
	    final Object value = symbolValue.getValue ();
	    if (value instanceof String)
	    {
		return (String)value;
	    }
	}
	return defaultValue;
    }

    /** When true it is illegal to set the value of this symbol. */
    public void setConstantValue (final boolean constantValue)
    {
	if (symbolValue != null)
	{
	    if (symbolValue instanceof ConstantValueCell)
	    {
		if (!constantValue)
		{
		    final ConstantValueCell cvc = (ConstantValueCell)symbolValue;
		    final Object value = cvc.getValue ();
		    symbolValue = new TypedValueCell (cvc.getValueType (), value);
		}
	    }
	    else if (constantValue)
	    {
		final Object value = symbolValue.getValue ();
		if (value == null)
		{
		    symbolValue = new ConstantValueCell (NullType.class, value);
		}
		else
		{
		    symbolValue = new ConstantValueCell (value.getClass (), value);
		}
	    }
	}
	else if (constantValue)
	{
	    throw new UnboundVariableError (this);
	}

    }

    /** The global function definition for a symbol. */
    public FunctionCell getFunction ()
    {
	return symbolFunction;
    }

    /**
     * The global function definition for a symbol. If this symbol does not current have a
     * FunctionCell, create a DefaultFunctionCell and install it. The DefaultFunctionCell will use
     * Java reflection to attempt to call a class method if it is called. The process is not
     * efficient, but it provides an easy to use way of calling Java methods from Lisp.
     */
    public FunctionCell getDefaultHandlerFunction ()
    {
	if (symbolFunction == null)
	{
	    symbolFunction = new DefaultFunctionCell (this, true);
	}
	return symbolFunction;
    }

    /** The global function definition for a symbol. */
    public void setFunction (final FunctionCell function)
    {
	// Check the function value cell to determine if changes are allowed
	if (symbolFunction != null)
	{
	    if (!symbolFunction.isAllowRedefinition ())
	    {
		throw new IllegalStateException (
		        String.format ("Can't Redefine %s from %s to %s\n", this, symbolFunction, function));
	    }
	    System.out.printf ("%%Redefining %s from %s to %s\n", this, symbolFunction, function);
	}
	symbolFunction = function;
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
    public Object put (final Symbol key, final Object val)
    {
	if (symbolPlist == null)
	{
	    symbolPlist = new HashMap<Symbol, Object> ();
	}
	return symbolPlist.put (key, val);
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

    public Map<Symbol, Object> getPlist ()
    {
	return symbolPlist;
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
	    final Symbol oldSymbol = symbolPackage.findSymbol (name);
	    if (oldSymbol == null)
	    {
		return symbolPackage.internSymbol (name);
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

    @Override
    public int hashCode ()
    {
	return System.identityHashCode (this);
    }

    @Override
    public boolean equals (final Object obj)
    {
	if (this == obj)
	{
	    return true;
	}
	return false;
    }

    public boolean isVariable ()
    {
	return symbolName.charAt (0) == QUESTION_MARK;
    }

    /** Print value to a buffer. */
    public void print (final StringBuilder buffer)
    {
	if (symbolPackage != null && symbolPackage != PackageFactory.getDefaultPackage ())
	{
	    final LispReader lispReader = LispReader.getLispThreadReader ();
	    if (lispReader == null || lispReader.findImportedSymbol (symbolName) != this)
	    {
		buffer.append (symbolPackage.getName ());
		buffer.append (PACKAGE_SEPARATOR);
	    }
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
	// result.put ("Public", symbolPackage.isPublic (this));
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

    /**
     * Read a symbol from a string, using a LispReader in the default package. This is slower than
     * directly interning the symbol in a package, but easier to write into code. Using a LispReader
     * allows package prefix notation to be used.
     */
    public static Symbol named (final String name)
    {
	final LispReader lispReader = new LispReader ();
	return lispReader.readSymbol (PackageFactory.getDefaultPackage (), name);
    }

    /**
     * Simple way to intern a symbol in a package.
     */
    public static Symbol named (final String packageName, final String name)
    {
	return PackageFactory.getPackage (packageName).internSymbol (name);
    }

    /**
     * Quick way for Java code to get at Lisp symbol values. This uses a LispReader so package
     * prefix notation can be used.
     *
     * @throws UnboundVariableError
     */
    public static Object value (final String name)
    {
	return named (name).getValue ();
    }

    /**
     * Quick way for Java code to get at Lisp symbol values. This uses a LispReader so package
     * prefix notation can be used.
     */
    public static Object value (final String name, final Object defaultValue)
    {
	return named (name).getValue (defaultValue);
    }

    /** Get a boolean value from a named flag. */
    public static boolean test (final String name, final boolean defaultValue)
    {
	return named (name).getBooleanValue (defaultValue);
    }
}
