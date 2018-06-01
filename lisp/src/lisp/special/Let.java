
package lisp.special;

import org.objectweb.asm.Opcodes;

import lisp.*;
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
    @DefineLisp (special = true, name = "let", classname = "lisp.special.LetFunction")
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

    @DefineLisp (special = true, name = "let*", classname = "lisp.special.LetStarFunction")
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
