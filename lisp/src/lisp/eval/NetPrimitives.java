
package lisp.eval;

import java.net.*;

public class NetPrimitives extends Definer
{
    // (url "http://www.apple.com")
    @DefineLisp
    public URL url (final String address) throws MalformedURLException
    {
	return new URL (address);
    }

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