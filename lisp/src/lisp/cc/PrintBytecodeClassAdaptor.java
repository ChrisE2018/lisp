
package lisp.cc;

import java.io.*;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.util.*;

public class PrintBytecodeClassAdaptor extends ClassVisitor
{
    private final StringWriter sw;

    public PrintBytecodeClassAdaptor (final int api, final ClassVisitor cv, final StringWriter sw)
    {
	super (api, new TraceClassVisitor (cv, new Textifier (), new PrintWriter (sw)));
	this.sw = sw;
    }

    @Override
    public void visitEnd ()
    {
	cv.visitEnd ();
	System.out.println (sw.toString ());
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
