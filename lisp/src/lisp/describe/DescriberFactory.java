
package lisp.describe;

import java.util.List;

import org.objectweb.asm.tree.*;

import lisp.lang.*;

public class DescriberFactory
{
    public Describer getDescriber (final Object arg)
    {
	if (arg == null)
	{
	    return new NullDescriber ();
	}
	if (arg instanceof Describer)
	{
	    return (Describer)arg;
	}
	if (arg instanceof List)
	{
	    return new LispList ();
	}
	if (arg instanceof Class)
	{
	    return new ClassDescriber ();
	}
	else if (arg instanceof ClassNode)
	{
	    return new ClassNodeDescriber ();
	}
	else if (arg instanceof MethodNode)
	{
	    return new MethodNodeDescriber ();
	}
	else if (arg instanceof InsnList)
	{
	    return new InsnListDescriber ();
	}
	else if (arg instanceof AbstractInsnNode)
	{
	    return new AbstractInsnNodeDescriber ();
	}
	else
	{
	    final Class<?> cls = arg.getClass ();
	    if (cls.isArray ())
	    {
		return new ArrayDescriber ();
	    }
	    else
	    {
		return new ObjectDescriber ();
	    }
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
