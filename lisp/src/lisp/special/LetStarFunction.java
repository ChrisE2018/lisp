
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.cc.*;
import lisp.cc3.*;
import lisp.cc4.*;
import lisp.lang.LispList;
import lisp.lang.Symbol;
import lisp.symbol.LispVisitor;

public class LetStarFunction implements LispCCFunction, LispTreeFunction, Opcodes, LispTreeWalker
{

    /** Call visitor on all directly nested subexpressions. */
    @Override
    public void walker (final LispVisitor visitor, final LispList expression)
    {
	visitor.visitStart (expression);
	final LispList bindings = (LispList)expression.get (1);
	// CONSIDER visit variable binding
	for (int i = 0; i < bindings.size (); i++)
	{
	    final LispList clause = (LispList)bindings.get (i);
	    // varSpec = clause.get (0)
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
    public CompileResults compile (final TreeCompilerContext context, final LispList expression, final boolean resultDesired)
    {
	// (define foo (x) (let* ((a 1) (b 2)) (+ a b x)))
	// (define foo () (let* ((a 1) (b 2)) a))
	// (define foo () (let* ((a b) (b a)) b))
	// (define bar () (let ((a b) (b a)) b))
	// (define foo (x) (let* ((a b) (b a)) (if x a b)))
	//
	// (setq a 1)
	// (define foo () (let* ((a 4) (c a)) c)) now (foo) = 4
	// (define foo () (let ((a 4) (c a)) c)) now (foo) = 1

	TreeCompilerContext innerContext = context;
	final LispList bindings = (LispList)expression.get (1);
	for (int i = 0; i < bindings.size (); i++)
	{
	    final LispList clause = (LispList)bindings.get (i);
	    final Object varSpec = clause.get (0);
	    final Symbol varName = NameSpec.getVariableName (varSpec);
	    final Class<?> varClass = NameSpec.getVariableClass (varSpec);
	    final Object valueExpression = clause.get (1);
	    final CompileResults valueResult = innerContext.compile (valueExpression, true);
	    innerContext.convert (valueResult, varClass, false, false);
	    innerContext = innerContext.bindVariable (varName, varClass);
	    final LexicalBinding binding = innerContext.getLocalVariableBinding (varName);
	    binding.store (innerContext);
	}
	for (int i = 2; i < expression.size () - 1; i++)
	{
	    final CompileResults r = innerContext.compile (expression.get (i), false);
	    // Do something with r to throw away garbage if required
	    innerContext.convert (r, void.class, false, false);
	}

	final CompileResults result = innerContext.compile (expression.last (), true);
	return result;
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
	final Map<Symbol, LexicalBinding> savedLocalVariableMap = generator.getLocalBindingContext ();
	final Map<Symbol, LexicalBinding> localVariableMap = new LinkedHashMap<Symbol, LexicalBinding> (savedLocalVariableMap);
	generator.setLocalBindingContext (localVariableMap);
	final LispList args = (LispList)e.get (1);
	for (final Object clause : args)
	{
	    final LispList c = (LispList)clause;
	    final Object varSpec = c.get (0);
	    final Symbol var = NameSpec.getVariableName (varSpec);
	    final Class<?> varClass = NameSpec.getVariableClass (varSpec);
	    final Type varType = Type.getType (varClass);
	    generator.compileExpression (mv, c.get (1), varClass, false, false);
	    final int localRef = mv.newLocal (varType);
	    mv.storeLocal (localRef);
	    final LexicalBinding lb = new LexicalVariable (var, varClass, localRef);
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
	buffer.append (System.identityHashCode (this));
	buffer.append (">");
	return buffer.toString ();
    }
}
