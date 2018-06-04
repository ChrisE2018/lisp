
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.*;
import lisp.cc4.LispTreeWalker;
import lisp.symbol.*;

public class LetFunction extends LispFunction implements Opcodes, LispTreeWalker
{
    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	final LispList bindings = (LispList)expression.get (1);
	// [TODO] visit variable binding
	for (int i = 0; i < bindings.size (); i++)
	{
	    final LispList clause = (LispList)bindings.get (i);
	    final Symbol var = bindings.head ();
	    visitor.visitValue (clause.get (1));
	}
	for (int i = 2; i < expression.size () - 1; i++)
	{
	    visitor.visitIgnored (expression.get (i));
	}
	visitor.visitValue (expression.last ());
	visitor.visitEnd (expression);
    }

    @Override
    public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e, final Class<?> valueType,
            final boolean allowNarrowing, final boolean liberalTruth)
    {
	// (define foo (x) (let ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let ((a 1) (b 2)) a))
	// (define foo () (let ((a b) (b a)) a))
	// (define foo (x) (let ((a b) (b a)) (if x a b)))

	// Compile expression values onto the stack in order
	final LispList args = (LispList)e.get (1);
	final Map<Symbol, LocalBinding> savedLocalVariableMap = generator.getLocalBindingContext ();
	final Map<Symbol, LocalBinding> newLocalVariableMap = new LinkedHashMap<Symbol, LocalBinding> (savedLocalVariableMap);
	for (int i = 0; i < args.size (); i++)
	{
	    final Object clause = args.get (i);
	    final LispList c = (LispList)clause;
	    final Object varSpec = c.get (0);
	    final Symbol var = CompileSupport.getNameVariable (varSpec);
	    final Class<?> varClass = CompileSupport.getNameType (varSpec);
	    final Type varType = Type.getType (varClass);
	    final int localRef = mv.newLocal (varType);
	    generator.compileExpression (mv, c.get (1), varClass, false, false);
	    mv.storeLocal (localRef);
	    final LocalBinding lb = new LocalBinding (var, varClass, localRef);
	    newLocalVariableMap.put (var, lb);
	}
	generator.setLocalBindingContext (newLocalVariableMap);

	// Evaluate optional body forms
	for (int i = 2; i < e.size () - 1; i++)
	{
	    generator.compileExpression (mv, e.get (i), null, false, false);
	}
	// Evaluate last (required) body form
	generator.compileExpression (mv, e.last (), valueType, allowNarrowing, liberalTruth);
	// Restore original local variables map
	// localVariableMap = savedLocalVariableMap;
	generator.setLocalBindingContext (savedLocalVariableMap);
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
