
package lisp.special;

import java.util.*;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.*;
import lisp.eval.*;

public class Let extends LogicDefiner implements Opcodes
{
    // [CONSIDER] Change reader so that a:b reads as (the a b) and define:
    // (the <type> <reference>)
    // to be a type declaration. Then:
    // (define int:foo (int : a int : b) (+ a b))
    // becomes integer addition. (Problem, colon is already used as a package separator.
    // Maybe change that to dot to be be Java.)

    // [IDEA] Eliminate package public/private distinction. Make import affect a reader
    // (like Java). If you write a fully qualified name, you get it (always). Define default
    // imports for a reader. Import functions should allow for import of all functions, or
    // symbols with specified attributes. (selective-import <attributes> "org.foo.*")

    // [IDEA] Implement a defpackage function or package manipulation functions. Define
    // default imports for any reader that is in a package.

    // [IDEA] Symbols could have attributes (like, public, protected, private) that affect
    // things.
    @DefineLisp (special = true, name = "let")
    public Object letEvaluator (final LexicalContext context, final LispList arglist, final Object body1, final Object... body)
            throws Exception
    {
	final LexicalContext newContext = new LexicalContext (context);
	for (final Object c : arglist)
	{
	    final LispList clause = (LispList)c;
	    final Symbol var = (Symbol)clause.get (0);
	    final Object expr = clause.get (1);
	    // Evaluate expressions in the original context and bind in the newContext
	    newContext.bind (var, context.eval (expr));
	}
	// Evaluate body expressions in the newContext
	Object result = newContext.eval (body1);
	for (final Object expr : body)
	{
	    result = newContext.eval (expr);
	}
	return result;
    }

    @DefineLisp (special = true, name = "let*")
    public Object letStarEvaluator (final LexicalContext context, final LispList arglist, final Object body1,
            final Object... body) throws Exception
    {
	final LexicalContext newContext = new LexicalContext (context);
	for (final Object c : arglist)
	{
	    final LispList clause = (LispList)c;
	    final Symbol var = (Symbol)clause.get (0);
	    final Object expr = clause.get (1);
	    // Evaluate expressions in the newContext and bind in the newContext
	    newContext.bind (var, newContext.eval (expr));
	}
	// Evaluate body expressions in the newContext
	Object result = newContext.eval (body1);
	for (final Object expr : body)
	{
	    result = newContext.eval (expr);
	}
	return result;
    }

    @DefineLisp (special = true, name = "let", compiler = true)
    public void compileLet (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
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
	    final LocalBinding lb = new LocalBinding (var, varType, localRef);
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

    @DefineLisp (special = true, name = "let*", compiler = true)
    public void compileLetStar (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList e,
            final Class<?> valueType, final boolean allowNarrowing, final boolean liberalTruth)
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
	    final LocalBinding lb = new LocalBinding (var, varType, localRef);
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
