
package lisp.cc4;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.commons.AdviceAdapter;

class AddTimerAdapter extends AdviceAdapter
{
    private final String owner;

    /**
     * Demo of AdviceAdaptor that can collect runtime. Not really useful except as a demo. Timer
     * value is not valid during a method call.
     * 
     * @see From ASM guide section 3.3.4 p.71.
     * @param access
     * @param owner The name of a class with a static long field called timer to hold the results.
     * @param name
     * @param desc
     * @param mv
     */
    public AddTimerAdapter (final int access, final String owner, final String name, final String desc, final MethodVisitor mv)
    {
	super (ASM5, mv, access, name, desc);
	this.owner = owner;
    }

    @Override
    protected void onMethodEnter ()
    {
	mv.visitFieldInsn (GETSTATIC, owner, "timer", "J");
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J", false);
	mv.visitInsn (LSUB);
	mv.visitFieldInsn (PUTSTATIC, owner, "timer", "J");
    }

    @Override
    protected void onMethodExit (final int opcode)
    {
	mv.visitFieldInsn (GETSTATIC, owner, "timer", "J");
	mv.visitMethodInsn (INVOKESTATIC, "java/lang/System", "currentTimeMillis", "()J", false);
	mv.visitInsn (LADD);
	mv.visitFieldInsn (PUTSTATIC, owner, "timer", "J");
    }

    @Override
    public void visitMaxs (final int maxStack, final int maxLocals)
    {
	super.visitMaxs (maxStack + 4, maxLocals);
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
