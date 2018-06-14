
package lisp.cc;

import java.util.*;

public class LispClassLoader extends ClassLoader
{
    private final Map<String, Object> quotedReferences = new LinkedHashMap<String, Object> ();

    protected Class<?> defineClass (final String name, final byte[] b) throws ClassFormatError
    {
	return super.defineClass (name, b, 0, b.length);
    }

    public void setQuotedReferences (final Map<String, Object> quotedReferences)
    {
	this.quotedReferences.putAll (quotedReferences);
    }

    public Map<String, Object> getQuotedReferences ()
    {
	return quotedReferences;
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
