
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.eval.Definer;

/**
 * Evaluator for quoted forms. The interpreted definition is required early in startup so it is
 * defined in Interpreter.java.
 */
public class Quote extends Definer implements Opcodes
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
