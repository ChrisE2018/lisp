
package lisp.cc4;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.LispList;
import lisp.Symbol;
import lisp.cc.LocalBinding;
import lisp.symbol.*;
import lisp.util.LogString;

public class TreeCompilerContext implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (TreeCompilerContext.class.getName ());

    private static final TreeConverter converter = new TreeConverter ();

    private static final TreeBoxer boxer = new TreeBoxer ();

    private final TreeCompiler treeCompiler;

    private final MethodNode mn;

    private final InsnList il;

    // Can we use mn.locals instead of our own structure?
    private final Map<Symbol, LocalBinding> locals;

    public TreeCompilerContext (final TreeCompiler treeCompiler, final MethodNode mn, final Map<Symbol, LocalBinding> locals)
    {
	this.treeCompiler = treeCompiler;
	this.mn = mn;
	il = mn.instructions;
	this.locals = locals;
    }

    /** Setup the binding for a new local variable. */
    public TreeCompilerContext bindVariable (final Symbol var, final Class<?> varClass)
    {
	// [TODO] Can we use mn.locals instead of our own structure?
	final Type varType = Type.getType (varClass);
	final List<LocalVariableNode> lvl = mn.localVariables;
	final String name = var.getName ();
	final String descriptor = varType.getDescriptor ();
	final String signature = null;
	final LabelNode start = new LabelNode ();
	final LabelNode end = new LabelNode ();
	final int index = lvl.size () + treeCompiler.getArgCount () + 1;
	final LocalVariableNode local = new LocalVariableNode (name, descriptor, signature, start, end, index);
	lvl.add (local);
	final Map<Symbol, LocalBinding> newLocals = new HashMap<Symbol, LocalBinding> (locals);
	newLocals.put (var, new LocalBinding (var, varClass, index));
	return new TreeCompilerContext (treeCompiler, mn, newLocals);
    }

    /** Setup the bindings for several new local variables. */
    public TreeCompilerContext bindVariables (final Map<Symbol, Class<?>> bindings)
    {
	// [TODO] Can we use mn.locals instead of our own structure?
	final List<LocalVariableNode> lvl = mn.localVariables;
	final Map<Symbol, LocalBinding> newLocals = new HashMap<Symbol, LocalBinding> (locals);
	for (final Entry<Symbol, Class<?>> entry : bindings.entrySet ())
	{
	    final Symbol var = entry.getKey ();
	    final Class<?> varClass = entry.getValue ();
	    final Type varType = Type.getType (varClass);
	    final String name = var.getName ();
	    final String descriptor = varType.getDescriptor ();
	    final String signature = null;
	    final LabelNode start = new LabelNode ();
	    final LabelNode end = new LabelNode ();
	    final int index = lvl.size () + treeCompiler.getArgCount () + 1;
	    final LocalVariableNode local = new LocalVariableNode (name, descriptor, signature, start, end, index);
	    lvl.add (local);
	    newLocals.put (var, new LocalBinding (var, varClass, index));
	}
	return new TreeCompilerContext (treeCompiler, mn, newLocals);
    }

    public LocalBinding getLocalVariableBinding (final Symbol var)
    {
	return locals.get (var);
    }

    public TreeCompiler getTreeCompiler ()
    {
	return treeCompiler;
    }

    public InsnList getInstructions ()
    {
	return il;
    }

    public Map<Symbol, LocalBinding> getLocals ()
    {
	return locals;
    }

    public void add (final AbstractInsnNode insnNode)
    {
	if (insnNode != null)
	{
	    il.add (insnNode);
	}
    }

    public void add (final List<LabelNode> nodes)
    {
	for (final AbstractInsnNode node : nodes)
	{
	    if (node != null)
	    {
		il.add (node);
	    }
	}
    }

    public void convertIfTrue (final CompileResultSet testResultSet, final boolean allowNarrowing, final boolean liberalTruth,
            final LabelNode lTrue)
    {
	converter.convertIfTrue (il, testResultSet, allowNarrowing, liberalTruth, lTrue);
    }

    public void convertIfFalse (final CompileResultSet testResultSet, final boolean allowNarrowing, final boolean liberalTruth,
            final LabelNode lFalse)
    {
	converter.convertIfFalse (il, testResultSet, allowNarrowing, liberalTruth, lFalse);
    }

    /**
     * Fall through if false. Otherwise leave value on the stack and jump to one of the lTrue
     * labels.
     */
    public CompileResultSet convert2true (final CompileResultSet testResultSet)
    {
	return converter.convert2true (il, testResultSet);
    }

    public void convert (final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	converter.convert (il, fromClass, toClass, allowNarrowing, liberalTruth);
    }

    public void convert (final CompileResultSet fromClass, final Class<?> toClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	if (fromClass == null)
	{
	    converter.convert (il, void.class, toClass, allowNarrowing, liberalTruth);
	    return;
	}
	// [TODO] If toClass is void (or null) then collapse all cases of the same size
	final LabelNode l = new LabelNode ();
	final List<CompileResult> results = fromClass.getResults ();
	for (int i = 0; i < results.size (); i++)
	{
	    final CompileResult cr = results.get (i);
	    for (final LabelNode label : cr.getLabels ())
	    {
		add (label);
	    }
	    if (cr instanceof ExplicitCompileResult)
	    {
		final Class<?> fc = (((ExplicitCompileResult)cr).getResultClass ());
		converter.convert (il, fc, toClass, allowNarrowing, liberalTruth);
	    }
	    else if (cr instanceof ImplicitCompileResult)
	    {
		final ImplicitCompileResult icr = (ImplicitCompileResult)cr;
		final Class<?> fc = icr.getResultClass ();
		add (new LdcInsnNode (icr.getValue ()));
		converter.convert (il, fc, toClass, allowNarrowing, liberalTruth);
	    }
	    // Jump to exit label if required
	    if (results.size () > 1 && i + 1 < results.size ())
	    {
		add (new JumpInsnNode (GOTO, l));
	    }
	}
	if (results.size () > 1)
	{
	    add (l);
	}
    }

    public CompileResultSet compile (final Object expr, final boolean resultDesired)
    {
	if (expr instanceof List)
	{
	    return compileFunctionCall ((LispList)expr, resultDesired);
	}
	else if (resultDesired)
	{
	    if (expr instanceof Symbol)
	    {
		// Variable reference
		return compileSymbolReference ((Symbol)expr);
	    }
	    else
	    {
		// Top result class is the same as the expr, perhaps converted to a primitive type.
		final Class<?> ec = expr.getClass ();
		final Class<?> p = boxer.getUnboxedClass (ec);
		// return p != null ? p : ec;
		return new CompileResultSet (new ImplicitCompileResult (expr));
	    }
	}
	return null;
    }

    private CompileResultSet compileFunctionCall (final LispList expression, final boolean resultDesired)
    {
	final Symbol symbol = expression.head ();
	final FunctionCell function = symbol.getFunction ();
	if (function != null)
	{
	    if (function.getLispFunction () instanceof LispTreeFunction)
	    {
		// Some 'normal' functions need special coding, i.e, arithmetic and comparisons, so
		// this is called for any function with a compiler, not just special forms.
		return compileSpecialLispFunction (expression, resultDesired);
	    }
	    if (function instanceof SpecialFunctionCell)
	    {
		// Since this is not a known special function, we are stuck and can't proceed
		throw new IllegalArgumentException ("Unrecognized special form " + symbol);
	    }
	    if (function instanceof MacroFunctionCell)
	    {
		// Expand and replace
		final MacroFunctionCell macro = (MacroFunctionCell)function;
		try
		{
		    final Object replacement = macro.expand (expression);
		    return compile (replacement, resultDesired);
		}
		catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e1)
		{
		    throw new Error ("Error expanding macro " + symbol, e1);
		}
	    }
	    if (optimizeFunctionCall (expression))
	    {
		return compileDirectFunctionCall (expression, resultDesired);
	    }
	}
	return compileDefaultFunctionCall (expression);
    }

    private CompileResultSet compileSpecialLispFunction (final LispList expression, final boolean resultDesired)
    {
	final Symbol symbol = expression.head ();
	final FunctionCell function = symbol.getFunction ();
	if (function != null)
	{
	    final LispFunction compiler = function.getLispFunction ();
	    if (compiler instanceof LispTreeFunction)
	    {
		final LispTreeFunction c = (LispTreeFunction)compiler;
		return c.compile (this, expression, resultDesired);
	    }
	}
	return null;
    }

    private boolean optimizeFunctionCall (final LispList expression)
    {
	return false;
    }

    private CompileResultSet compileDirectFunctionCall (final LispList expression, final boolean resultDesired)
    {
	return null;
    }

    /**
     * Compile a function call and add it to the instruction list. This creates a generic function
     * call to any normal function. <br/>
     * [TODO] Special forms and macros need to be handled. <br/>
     * [TODO] Optimized calls to known functions should also be produced.
     *
     * @param il The instruction list.
     * @param locals Local variable binding information.
     * @param expression The function call expression.
     * @return The class of the return value.
     */
    private CompileResultSet compileDefaultFunctionCall (final LispList expression)
    {
	final Symbol symbol = expression.head ();
	treeCompiler.addSymbolReference (symbol);
	LOGGER.finer (new LogString ("Function symbol reference to %s", symbol));
	il.add (new VarInsnNode (ALOAD, 0));
	final Type classType = treeCompiler.getClassType ();
	final String classInternalName = classType.getInternalName ();
	il.add (new FieldInsnNode (GETFIELD, classInternalName, treeCompiler.createJavaSymbolName (symbol), "Llisp/Symbol;"));

	// Get the FunctionCell from the function symbol.
	// The call to getDefaultHandlerFunction will return a DefaultHandler that tries to invoke
	// the java method on arg 1 if the function has not been given any other definition.
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "getDefaultHandlerFunction", "()Llisp/symbol/FunctionCell;",
	        false));

	// Compile the arguments. Pass all the arguments as elements of a single array of Objects.
	final int argCount = expression.size () - 1;
	il.add (new LdcInsnNode (argCount));
	il.add (new TypeInsnNode (ANEWARRAY, "java/lang/Object"));
	for (int i = 0; i < argCount; i++)
	{
	    il.add (new InsnNode (DUP));
	    il.add (new LdcInsnNode (i));
	    final CompileResultSet resultDescriptor = compile (expression.get (i + 1), true);
	    convert (resultDescriptor, Object.class, false, false);
	    il.add (new InsnNode (AASTORE));
	}

	// Call invoke on the method retrieved from the FunctionCell of the function symbol.
	// Assume the function will still be defined when we execute this code.
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/symbol/FunctionCell", "apply", "([Ljava/lang/Object;)Ljava/lang/Object;",
	        false));
	return new CompileResultSet (new ExplicitCompileResult (Object.class));
    }

    /**
     * Compile an expression to calculate the value of a symbol. This determines if the symbol is an
     * argument, local variable or global symbol and calculates the correct value. <br/>
     * [TODO] Constants and typed variables should be handled specially.
     *
     * @param mv The bytecode generator.
     * @param symbol The symbol value to calculate.
     * @return The class of the result produced.
     */
    private CompileResultSet compileSymbolReference (final Symbol symbol)
    {
	if (locals.containsKey (symbol))
	{
	    // Reference to a local lexical variable
	    final LocalBinding lb = locals.get (symbol);
	    final int localRef = lb.getLocalRef ();
	    final Type varType = lb.getType ();
	    il.add (new VarInsnNode (varType.getOpcode (ILOAD), localRef));
	    final Class<?> fromClass = lb.getVariableClass ();
	    // return new Object[][] {{null, fromClass}};
	    return new CompileResultSet (new ExplicitCompileResult (fromClass));
	}
	else if (symbol.is ("true"))
	{
	    return new CompileResultSet (new ImplicitCompileResult (true));
	}
	else if (symbol.is ("false"))
	{
	    return new CompileResultSet (new ImplicitCompileResult (false));
	}
	else
	{
	    // Reference to a global variable
	    treeCompiler.addGlobalReference (symbol); // Log message
	    treeCompiler.addSymbolReference (symbol); // Make symbol available at execution time
	    // [TODO] If the symbol valueCell is constant, use the current value.
	    // [TODO] If the valueCell is a TypedValueCell, use the type information.
	    il.add (new VarInsnNode (ALOAD, 0));
	    final Type classType = treeCompiler.getClassType ();
	    final String classInternalName = classType.getInternalName ();
	    il.add (new FieldInsnNode (GETFIELD, classInternalName, treeCompiler.createJavaSymbolName (symbol), "Llisp/Symbol;"));
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;", false));
	    // return new Object[][] {{null, Object.class}};
	    return new CompileResultSet (new ExplicitCompileResult (Object.class));
	}
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
