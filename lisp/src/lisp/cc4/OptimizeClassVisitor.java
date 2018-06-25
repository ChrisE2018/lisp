
package lisp.cc4;

import org.objectweb.asm.*;

import lisp.asm.RemoveDeadCodeAdapter;

public class OptimizeClassVisitor extends ClassVisitor
{
    private String classInternalName = null;

    public OptimizeClassVisitor (final int api, final ClassVisitor classVisitor)
    {
	super (api, classVisitor);
    }

    @Override
    public void visit (final int version, final int access, final String name, final String signature, final String superName,
            final String[] interfaces)
    {
	classInternalName = name;
	super.visit (version, access, name, signature, superName, interfaces);
    }

    @Override
    public MethodVisitor visitMethod (final int access, final String methodName, final String descriptor,
            final String methodSignature, final String[] exceptions)
    {
	final MethodVisitor mv = super.visitMethod (access, methodName, descriptor, methodSignature, exceptions);
	if (methodName.equals ("<init>"))
	{
	    return mv;
	}
	else
	{
	    return new RemoveDeadCodeAdapter (api, classInternalName, access, methodName, descriptor, mv);
	}

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
