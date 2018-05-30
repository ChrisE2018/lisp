
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.LogicDefiner;

public class The extends LogicDefiner implements Opcodes
{

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
