
package org.objectweb.asm;

public class GetPackageInternals
{
    /** Provide access to package access field */
    public static int getLabelInputStackSize (final Label label)
    {
	return label.inputStackSize;
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
