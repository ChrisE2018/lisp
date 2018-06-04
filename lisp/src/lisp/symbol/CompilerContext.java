
package lisp.symbol;

import java.util.*;

import org.objectweb.asm.Label;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.cc.CompilerGenerator;

public class CompilerContext
{
    private final CompilerGenerator generator = null;
    private final GeneratorAdapter mv;

    private final Map<Label, List<Class<?>>> returnBranches = new HashMap<Label, List<Class<?>>> ();

    public CompilerContext (final GeneratorAdapter mv)
    {
	this.mv = mv;
    }

    public GeneratorAdapter getGeneratorAdapter ()
    {
	return mv;
    }

    public Map<Label, List<Class<?>>> getReturnBranches ()
    {
	return returnBranches;
    }

    /**
     * Generate instructions to produce a default value of the currently required return type. If no
     * value is expected this will produce nothing.
     */
    public void compileDefaultValue ()
    {
	// generator.pushDefaultValue (mv, valueClass, booleanDefault);
    }

    /** Compile an expression for side effect only. */
    public void compile2void (final Object expression)
    {
    }

    /** Compile an expression to produce a value. */
    public void compile2value (final Object expression)
    {
    }

    public void compileConstant (final boolean value)
    {
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
