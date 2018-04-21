
package lisp;

public interface Lisp
{
    public void print (final StringBuilder buffer);

    public default StringBuilder toJavaBuffer ()
    {
	final StringBuilder buffer = new StringBuilder ();
	print (buffer);
	return buffer;
    }

    public default String toJavaString ()
    {
	return toJavaBuffer ().toString ();
    }
}
