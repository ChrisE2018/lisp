
package lisp.eval;

import java.util.*;

import lisp.LispList;

/**
 * Bind java functions to lisp symbols.
 * <p>
 * Note that special forms like quote and setq are passed the interpreter as a first parameter and
 * the entire expression (including the function name) as the arguments. The real arguments start at
 * the second position. This is convenient because the interpreter is already processing the whole
 * expression and would have to allocate a new object to remove the function from the start of the
 * expression.
 * </p>
 * <p>
 * Normal functions are just passed the evaluated arguments. The real arguments start at the
 * beginning. This is convenient since the interpreter creates a new list for the arguments anyhow.
 * </p>
 */
public class ListPrimitives extends Definer
{
    // [TODO] Maps, sets,
    // [done] union, intersection, difference
    // [TODO] Package creation, uses, export, import
    // [TODO] File functions

    @DefineLisp
    public Object list (final Object... arguments)
    {
	return new LispList (arguments);
    }

    @DefineLisp
    public LinkedHashSet<Object> set (final Object... arguments)
    {
	final LinkedHashSet<Object> result = new LinkedHashSet<Object> ();
	for (final Object x : arguments)
	{
	    result.add (x);
	}
	return result;
    }

    @DefineLisp
    public Set<Object> intersection (final Set<Object> a, final Set<Object> b)
    {
	final LinkedHashSet<Object> result = new LinkedHashSet<Object> ();
	for (final Object x : a)
	{
	    if (b.contains (x))
	    {
		result.add (x);
	    }
	}
	return result;
    }

    @DefineLisp
    public Set<Object> union (final Set<Object> a, final Set<Object> b)
    {
	final LinkedHashSet<Object> result = new LinkedHashSet<Object> ();
	result.addAll (a);
	result.addAll (b);
	return result;
    }

    @DefineLisp
    public Set<Object> difference (final Set<Object> a, final Set<Object> b)
    {
	final LinkedHashSet<Object> result = new LinkedHashSet<Object> ();
	result.addAll (a);
	result.removeAll (b);
	return result;
    }

    @DefineLisp
    public Object nth (final List<?> arg, final int n)
    {
	return arg.get (n);
    }

    @DefineLisp
    public Object car (final List<?> arg)
    {
	return arg.get (0);
    }

    @DefineLisp
    public Object cadr (final List<?> arg)
    {
	return arg.get (1);
    }

    @DefineLisp
    public Object caddr (final List<?> arg)
    {
	return arg.get (2);
    }

    @DefineLisp
    public Object cadddr (final List<?> arg)
    {
	return arg.get (3);
    }

    @DefineLisp
    public Object caddddr (final List<?> arg)
    {
	return arg.get (4);
    }

    @DefineLisp
    public Object first (final List<?> arg)
    {
	return arg.get (0);
    }

    @DefineLisp
    public Object second (final List<?> arg)
    {
	return arg.get (1);
    }

    @DefineLisp
    public Object third (final List<?> arg)
    {
	return arg.get (2);
    }

    @DefineLisp
    public Object fourth (final List<?> arg)
    {
	return arg.get (3);
    }

    @DefineLisp
    public Object fifth (final List<?> arg)
    {
	return arg.get (4);
    }

    /** Roughly similar to the traditional Lisp cons function but does not share tail elements. */
    @DefineLisp
    public LispList cons (final Object x, final List<?> arg)
    {
	final LispList result = new LispList ();
	result.add (x);
	result.addAll (arg);
	return result;
    }

    @DefineLisp
    public boolean member (final Object x, final Collection<?> c)
    {
	return c.contains (x);
    }

    // @DefineLisp
    // public boolean remove (final Object x, final Collection<?> c)
    // {
    // return c.remove (x);
    // }

    @DefineLisp
    public int count (final Object x, final Collection<?> c)
    {
	int result = 0;
	for (final Object e : c)
	{
	    if (e.equals (x))
	    {
		result++;
	    }
	}
	return result;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (">");
	return buffer.toString ();
    }
}
