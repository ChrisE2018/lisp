
package lisp.cc;

import java.util.*;
import java.util.Map.Entry;

import lisp.lang.Symbol;

public class LispClassLoader extends ClassLoader
{
    private final Map<Symbol, Object> quotedReferences = new LinkedHashMap<Symbol, Object> ();

    protected Class<?> defineClass (final String name, final byte[] b) throws ClassFormatError
    {
	return super.defineClass (name, b, 0, b.length);
    }

    public void setQuotedReferences (final Map<Symbol, Object> quotedReferences)
    {
	this.quotedReferences.putAll (quotedReferences);
    }

    public Map<Symbol, Object> getQuotedData ()
    {
	return quotedReferences;
    }

    /**
     * Get the name from created names to quoted objects. This only works because compiled code is
     * being loaded into the same environment where it is compiled. To save compiled code to a file
     * would required building the structure in the init method when the class is loaded.
     */
    public Map<String, Object> getQuotedReferences ()
    {
	final Map<String, Object> result = new LinkedHashMap<String, Object> ();
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    result.put (entry.getKey ().getName (), entry.getValue ());
	}
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
