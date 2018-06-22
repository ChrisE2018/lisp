
package lisp.cc;

import java.io.IOException;
import java.lang.reflect.*;
import java.util.logging.Logger;

import org.objectweb.asm.tree.ClassNode;

import lisp.eval.*;
import lisp.lang.*;
import lisp.symbol.*;

public class DefinePrimitives extends Definer
{
    private static final Logger LOGGER = Logger.getLogger (DefinePrimitives.class.getName ());

    @DefineLisp
    public void undefine (final Symbol fn)
    {
	fn.setFunction (null);
    }

    @DefineLisp
    public void undefine (final Symbol fn, final boolean force)
    {
	fn.undefine (force);
    }

    // (define foo (x) alpha)
    // (define foo (a b) (+ 3 4))
    @DefineLisp (special = true, name = "define")
    public Object define (@SuppressWarnings ("unused") final LexicalContext context, final Object nameSpec, final LispList args,
            final Object... forms)
    {
	try
	{
	    // If first form is a string it should be saved as documentation. The
	    // FunctionCell should have a field to store documentation.
	    // Before the functionName we should require a return type. The package system
	    // should mirror the java package system, so java.lang.Object would be an allowed type,
	    // with abbreviation of Object allowed. The Symbol class should have a getClass method
	    // that returns the corresponding Java type. Declarations associated with the function
	    // definition should determine if a compiled call will strongly depend on the current
	    // definition.
	    // TODO Function calls should not create new argument arrays.
	    // TODO Use exception handling
	    // DONE Implement proper method overloading based on argument types
	    // to detect the case where a function definition changes since it was compiled.
	    final LispList body = new LispList ();
	    for (final Object f : forms)
	    {
		body.add (f);
	    }
	    // If functionName looks like (the <type> <name>) then declare a return type.
	    final Symbol functionName = NameSpec.getVariableName (nameSpec);
	    final Class<?> returnType = NameSpec.getVariableClass (nameSpec);
	    LOGGER.info (String.format ("Compiling %s %s: %s", returnType, functionName, body));
	    final Class<?> c = createCompiledFunction (returnType, functionName, args, body);
	    LOGGER.info (String.format ("Compiled %s", functionName));
	    LOGGER.info (String.format ("List of Declared Methods"));
	    for (final Method method : c.getDeclaredMethods ())
	    {
		LOGGER.info (String.format ("* Method: %s", method));
	    }
	    LOGGER.info ("");
	    return functionName;
	}
	catch (final Exception e)
	{
	    VerifyPrimitives.incrementReplErrorCount (e.toString ());
	    e.printStackTrace ();
	}
	return false;
    }

    /**
     * Create a compiled function definition from lisp sources.
     *
     * @throws IOException
     */
    private Class<?> createCompiledFunction (final Class<?> returnType, final Symbol symbol, final LispList args,
            final LispList body) throws NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException,
            IllegalArgumentException, InvocationTargetException, IOException
    {

	final String methodName = symbol.gensym ().getName ();
	final String documentation = (body.get (0) instanceof String) ? (String)(body.get (0)) : "";

	final CompilerFactory compilerFactory = new CompilerFactory ();
	final Compiler cl = compilerFactory.getCompiler (returnType, methodName, args, body);
	final Class<?> cls = cl.compile ();

	// Call int constructor to make an instance
	final Class<?>[] types = {int.class};
	final Constructor<?> con = cls.getConstructor (types);
	// System.out.printf ("Calling newInstance(1)%n");
	final Object instance = con.newInstance (1);

	// Locate method matching the compiled function
	final Class<?>[] parameterTypes = new Class<?>[args.size ()];
	for (int i = 0; i < parameterTypes.length; i++)
	{
	    // parameterTypes[i] = java.lang.Object.class;
	    parameterTypes[i] = NameSpec.getVariableClass (args.get (i));
	}
	final Method method = cls.getDeclaredMethod (methodName, parameterTypes);
	FunctionCell function = symbol.getFunction ();
	final LispList source = new LispList ();
	final ClassNode cn = cl.getClassNode ();
	source.add (Symbol.named ("system", "define"));
	source.add (symbol);
	source.add (args);
	for (final Object form : body)
	{
	    source.add (form);
	}
	// Overloading requires adding the method to an existing function cell
	if (function != null && !(function instanceof DefFunctionCell))
	{
	    function.overload (instance, method, documentation, source, cn);
	}
	else
	{
	    function = new StandardFunctionCell (symbol);
	    symbol.setFunction (function);
	    function.overload (instance, method, documentation, source, cn);
	}
	return cls;
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
