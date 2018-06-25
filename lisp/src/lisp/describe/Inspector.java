
package lisp.describe;

public class Inspector
{
    private DescriberFactory factory;

    public Inspector ()
    {
	factory = new DescriberFactory ();
    }

    public Inspector (final DescriberFactory factory)
    {
	this.factory = factory != null ? factory : new DescriberFactory ();
    }

    public DescriberFactory getFactory ()
    {
	return factory;
    }

    public void setFactory (final DescriberFactory factory)
    {
	this.factory = factory;
    }

    public Object inspect (final Object arg)
    {
	return arg;
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
