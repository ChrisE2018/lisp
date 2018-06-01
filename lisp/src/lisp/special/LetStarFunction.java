
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.*;
import lisp.symbol.LispFunction;

public class LetStarFunction extends LispFunction implements Opcodes
{
    public LetStarFunction (final Symbol symbol)
    {
	super (symbol);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (let* ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let* ((a 1) (b 2)) a))
	// (define foo () (let* ((a b) (b a)) b))
	// (define bar () (let ((a b) (b a)) b))
	// (define foo (x) (let* ((a b) (b a)) (if x a b)))
	// final LocalVariablesSorter lvs = (LocalVariablesSorter)mv;

	// Compile expression values onto the stack in order
	// Bind the variables as each value is computed
	final Map<Symbol, LocalBinding> savedLocalVariableMap = generator.getLocalBindingContext ();
	final Map<Symbol, LocalBinding> localVariableMap = new LinkedHashMap<Symbol, LocalBinding> (savedLocalVariableMap);
	generator.setLocalBindingContext (localVariableMap);
	final LispList args = (LispList)e.get (1);
	for (final Object clause : args)
	{
	    final LispList c = (LispList)clause;
	    final Object varSpec = c.get (0);
	    final Symbol var = CompileSupport.getNameVariable (varSpec);
	    final Class<?> varClass = CompileSupport.getNameType (varSpec);
	    final Type varType = Type.getType (varClass);
	    generator.compileExpression (mv, c.get (1), varClass, false, false);
	    final int localRef = mv.newLocal (varType);
	    mv.storeLocal (localRef);
	    final LocalBinding lb = new LocalBinding (var, varClass, localRef);
	    localVariableMap.put (var, lb);
	}

	// Evaluate optional body forms
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Evaluate last (required) body form
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);

	// Restore original local variables map
	generator.setLocalBindingContext (savedLocalVariableMap);
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (getSymbol ());
	buffer.append (">");
	return buffer.toString ();
    }
}
