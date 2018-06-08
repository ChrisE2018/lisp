
package lisp.asm.instructions;

import org.objectweb.asm.Handle;

public class InvokeDynamicInsnNode extends org.objectweb.asm.tree.InvokeDynamicInsnNode
{
    public InvokeDynamicInsnNode (final String name, final String descriptor, final Handle bootstrapMethodHandle,
            final Object[] bootstrapMethodArguments)
    {
	super (name, descriptor, bootstrapMethodHandle, bootstrapMethodArguments);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (name);
	// buffer.append (" ");
	// buffer.append (descriptor);
	// buffer.append (" ");
	// buffer.append (bootstrapMethodHandle);
	return buffer.toString ();
    }
}
