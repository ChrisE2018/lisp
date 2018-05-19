
package lisp.eval;

import java.lang.reflect.Method;

public class ObjectMethod
{
    private Object object;
    private Method method;

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
