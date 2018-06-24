
package lisp.primitives;

import java.net.*;

import lisp.eval.*;

public class NetPrimitives extends Definer
{
    // (url "http://www.apple.com")
    @DefineLisp
    public URL url (final String address) throws MalformedURLException
    {
	return new URL (address);
    }
}
