
package lisp.cc4;

import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;

import lisp.asm.instructions.FieldInsnNode;
import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.JumpInsnNode;
import lisp.asm.instructions.LabelNode;
import lisp.asm.instructions.LdcInsnNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.TypeInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.cc.*;
import lisp.exceptions.DontOptimize;
import lisp.lang.*;
import lisp.lang.Symbol;
import lisp.symbol.*;
import lisp.util.*;

public class TreeCompilerContext implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (TreeCompilerContext.class.getName ());

    private static final TreeConverter converter = new TreeConverter ();

    private static final TreeBoxer boxer = new TreeBoxer ();

    private static JavaName javaName = new JavaName ();

    private static MethodSignature methSignature = new MethodSignature ();

    private static SelectMethod selectMethod = new SelectMethod ();

    private static Symbol DOT_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("dot");

    private final TreeCompilerInterface treeCompiler;

    private final QuotedData quotedData;

    private final MethodNode mn;

    private final InsnList il;

    /** Return value type of the method being compiled. */
    private final Class<?> returnClass;

    /** Arguments of the method being compiled. */
    private final Type[] argumentTypes;
    private final int argumentCount;

    // Can we use mn.locals instead of our own structure?
    private final Map<Symbol, LexicalBinding> locals;

    private final List<BlockBinding> blockBindings;
    private final Set<Symbol> globalReferences = new HashSet<> ();

    public TreeCompilerContext (final TreeCompilerInterface treeCompiler, final QuotedData quotedData, final Class<?> returnClass,
            final MethodNode mn, final Map<Symbol, LexicalBinding> locals, final List<BlockBinding> blockBindings)
    {
	this.treeCompiler = treeCompiler;
	this.quotedData = quotedData;
	this.returnClass = returnClass;
	this.mn = mn;
	il = mn.instructions;
	this.locals = locals;
	this.blockBindings = blockBindings;
	argumentTypes = Type.getArgumentTypes (mn.desc);
	argumentCount = argumentTypes.length;
    }

    /** Setup the binding for a new local variable. */
    public TreeCompilerContext bindVariable (final Symbol var, final Class<?> varClass)
    {
	// CONSIDER Can we use mn.locals instead of our own structure?
	final Type varType = Type.getType (varClass);
	final List<LocalVariableNode> lvl = mn.localVariables;
	final String name = var.getName ();
	final String descriptor = varType.getDescriptor ();
	final String signature = null;
	final LabelNode start = new LabelNode ();
	final LabelNode end = new LabelNode ();
	final int index = lvl.size () + argumentCount + 1;
	final LocalVariableNode local = new LocalVariableNode (name, descriptor, signature, start, end, index);
	lvl.add (local);
	final Map<Symbol, LexicalBinding> newLocals = new HashMap<Symbol, LexicalBinding> (locals);
	newLocals.put (var, new LexicalVariable (var, varClass, index));
	final List<BlockBinding> newBlockBindings = new ArrayList<> (blockBindings);
	return new TreeCompilerContext (treeCompiler, quotedData, returnClass, mn, newLocals, newBlockBindings);
    }

    /** Setup the bindings for several new local variables. */
    public TreeCompilerContext bindVariables (final Map<Symbol, Class<?>> bindings)
    {
	// CONSIDER Can we use mn.locals instead of our own structure?
	final List<LocalVariableNode> lvl = mn.localVariables;
	final Map<Symbol, LexicalBinding> newLocals = new HashMap<Symbol, LexicalBinding> (locals);
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
	    final int index = lvl.size () + argumentCount + 1;
	    final LocalVariableNode local = new LocalVariableNode (name, descriptor, signature, start, end, index);
	    lvl.add (local);
	    newLocals.put (var, new LexicalVariable (var, varClass, index));
	}
	final List<BlockBinding> newBlockBindings = new ArrayList<> (blockBindings);
	return new TreeCompilerContext (treeCompiler, quotedData, returnClass, mn, newLocals, newBlockBindings);
    }

    /** Setup the binding for a new block named var. */
    public TreeCompilerContext bindBlock (final BlockBinding binding)
    {
	final Map<Symbol, LexicalBinding> newLocals = new HashMap<Symbol, LexicalBinding> (locals);
	final List<BlockBinding> newBlockBindings = new ArrayList<> (blockBindings);
	newBlockBindings.add (binding);
	return new TreeCompilerContext (treeCompiler, quotedData, returnClass, mn, newLocals, newBlockBindings);
    }

    public LexicalBinding getLocalVariableBinding (final Symbol var)
    {
	return locals.get (var);
    }

    public BlockBinding getBlockBinding (final Symbol name)
    {
	for (final BlockBinding bb : blockBindings)
	{
	    if (bb.getBlockName () == name)
	    {
		return bb;
	    }
	}
	return null;
    }

    public TreeCompilerInterface getTreeCompiler ()
    {
	return treeCompiler;
    }

    public QuotedData getQuotedData ()
    {
	return quotedData;
    }

    public InsnList getInstructions ()
    {
	return il;
    }

    public Map<Symbol, LexicalBinding> getLocals ()
    {
	return locals;
    }

    public void add (final AbstractInsnNode node)
    {
	if (node != null)
	{
	    il.add (node);
	    LOGGER.finer (new LogString ("%s instr: %s", il.size (), node));
	}
    }

    public void add (final List<? extends org.objectweb.asm.tree.LabelNode> nodes)
    {
	for (final AbstractInsnNode node : nodes)
	{
	    if (node != null)
	    {
		il.add (node);
		LOGGER.finer (new LogString ("%s instr: %s", il.size (), node));
	    }
	}
    }

    public void add (final TryCatchBlockNode node)
    {
	if (node != null)
	{
	    mn.tryCatchBlocks.add (node);
	    LOGGER.finer (new LogString ("Try Catch Block %s", node));
	}
    }

    public void convertIfTrue (final CompileResults testResultSet, final boolean allowNarrowing, final boolean liberalTruth,
            final org.objectweb.asm.tree.LabelNode lTrue)
    {
	converter.convertIfTrue (il, testResultSet, allowNarrowing, liberalTruth, lTrue);
    }

    public void convertIfFalse (final CompileResults testResultSet, final boolean allowNarrowing, final boolean liberalTruth,
            final org.objectweb.asm.tree.LabelNode lFalse)
    {
	converter.convertIfFalse (il, testResultSet, allowNarrowing, liberalTruth, lFalse);
    }

    /**
     * Fall through if false. Otherwise leave value on the stack and jump to one of the lTrue
     * labels.
     */
    public CompileResults convert2true (final CompileResults testResultSet)
    {
	return converter.convert2true (il, testResultSet);
    }

    public void convert (final Class<?> fromClass, final Class<?> toClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	converter.convert (il, fromClass, toClass, allowNarrowing, liberalTruth);
    }

    public void convert (final CompileResults fromClass, final Class<?> toClass, final boolean allowNarrowing,
            final boolean liberalTruth)
    {
	if (fromClass == null)
	{
	    converter.convert (il, void.class, toClass, allowNarrowing, liberalTruth);
	    return;
	}
	// FIXME If toClass is void (or null) then collapse all cases of the same size
	final LabelNode lExit = new LabelNode ();
	final List<CompileResult> results = fromClass.getResults ();
	for (int i = 0; i < results.size (); i++)
	{
	    final CompileResult cr = results.get (i);
	    add (cr.getLabels ());
	    if (cr instanceof ExplicitResult)
	    {
		final Class<?> fc = (((ExplicitResult)cr).getResultClass ());
		converter.convert (il, fc, toClass, allowNarrowing, liberalTruth);
	    }
	    else if (cr instanceof ImplicitResult)
	    {
		final ImplicitResult icr = (ImplicitResult)cr;
		final Class<?> fc = icr.getResultClass ();
		final Object x = icr.getValue ();
		if (x == null)
		{
		    add (new InsnNode (ACONST_NULL));
		}
		else if (validLdcInsnParam (x))
		{
		    add (new LdcInsnNode (icr.getValue ()));
		    converter.convert (il, fc, toClass, allowNarrowing, liberalTruth);
		}
		else
		{
		    // Need new special case for standard class, like java.lang.System
		    final Symbol s = quotedData.addQuotedConstant (x);
		    final Class<?> quotedClass = x.getClass ();
		    final String typeDescriptor = Type.getType (quotedClass).getDescriptor ();
		    il.add (new VarInsnNode (ALOAD, 0));
		    final String classInternalName = treeCompiler.getClassType ().getInternalName ();
		    il.add (new FieldInsnNode (GETFIELD, classInternalName, s.getName (), typeDescriptor));
		    convert (quotedClass, toClass, allowNarrowing, liberalTruth);
		}
	    }
	    // Jump to exit label
	    add (new JumpInsnNode (GOTO, lExit));
	}
	add (lExit);
    }

    // public void addZ (final ImplicitCompileResult icr)
    // {
    // final Object x = icr.getValue ();
    // if (x == null)
    // {
    // il.add (new InsnNode (ACONST_NULL));
    // }
    // else if (validLdcInsnParam (x))
    // {
    // il.add (new LdcInsnNode (x));
    // final Class<?> ec = x.getClass ();
    // final Class<?> p = boxer.getUnboxedClass (ec);
    // final Class<?> actual = p != null ? p : ec;
    // convert (actual, returnClass, false, false);
    // }
    // else
    // {
    // final Symbol s = quotedData.addQuotedConstant (x);
    // final Class<?> quotedClass = x.getClass ();
    // final String typeDescriptor = Type.getType (quotedClass).getDescriptor ();
    // il.add (new VarInsnNode (ALOAD, 0));
    // final String classInternalName = treeCompiler.getClassType ().getInternalName ();
    // il.add (new FieldInsnNode (GETFIELD, classInternalName, s.getName (), typeDescriptor));
    // convert (quotedClass, quotedClass, false, false);
    // }
    // }

    public void add (final ImplicitResult icr, final Class<?> toClass)
    {
	final Object x = icr.getValue ();
	if (x == null)
	{
	    il.add (new InsnNode (ACONST_NULL));
	}
	else if (validLdcInsnParam (x))
	{
	    il.add (new LdcInsnNode (x));
	    final Class<?> ec = x.getClass ();
	    final Class<?> p = boxer.getUnboxedClass (ec);
	    final Class<?> actual = p != null ? p : ec;
	    convert (actual, toClass, false, false);
	}
	else
	{
	    final Symbol s = quotedData.addQuotedConstant (x);
	    final Class<?> quotedClass = x.getClass ();
	    final String typeDescriptor = Type.getType (quotedClass).getDescriptor ();
	    il.add (new VarInsnNode (ALOAD, 0));
	    final String classInternalName = treeCompiler.getClassType ().getInternalName ();
	    il.add (new FieldInsnNode (GETFIELD, classInternalName, s.getName (), typeDescriptor));
	    convert (quotedClass, toClass, false, false);
	}
    }

    /**
     * Test for {@link Integer}, a {@link Float}, a {@link Long}, a {@link Double} or a
     * {@link String}. {@link Boolean} also works.
     */
    private boolean validLdcInsnParam (final Object x)
    {
	return x instanceof Boolean || x instanceof Integer || x instanceof Float || x instanceof Long || x instanceof Double
	       || x instanceof String;
    }

    public CompileResults compile (final Object expression, final boolean resultDesired)
    {
	if (expression instanceof List)
	{
	    // FIXME This fails if the expression starts with a method
	    // Try: 'java.lang.String.format
	    return compileFunctionCall ((LispList)expression, resultDesired);
	}
	else if (resultDesired)
	{
	    if (expression instanceof Symbol)
	    {
		// Variable reference
		return compileSymbolReference ((Symbol)expression);
	    }
	    else
	    {
		final LabelNode ll = new LabelNode ();
		add (new JumpInsnNode (GOTO, ll));
		return new CompileResults (new ImplicitResult (ll, expression));
	    }
	}
	return null;
    }

    private CompileResults compileFunctionCall (final LispList expression, final boolean resultDesired)
    {
	final Object fn = expression.get (0);
	if (!(fn instanceof Symbol))
	{
	    if (fn instanceof List)
	    {
		final List<?> f = (List<?>)fn;
		if (f.size () > 0 && f.get (0) == DOT_SYMBOL)
		{
		    try
		    {
			return compileDotFunctionCall (expression);
		    }
		    catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e)
		    {
			e.printStackTrace ();
		    }
		}
	    }
	    throw new Error (String.format ("Object %s is not a function", fn));
	}
	final Symbol symbol = (Symbol)fn;
	final FunctionCell function = symbol.getFunction ();
	if (function != null)
	{
	    final CompileResults result = compileDefinedFunctionCall (symbol, expression, resultDesired);
	    if (result != null)
	    {
		return result;
	    }
	}
	// Function call always returns a value whether we want it or not
	return compileDefaultFunctionCall (expression);
    }

    private CompileResults compileDefinedFunctionCall (final Symbol symbol, final LispList expression,
            final boolean resultDesired)
    {
	final FunctionCell function = symbol.getFunction ();
	try
	{
	    final LispFunction lispFunction = function.getLispFunction ();
	    if (lispFunction instanceof LispTreeFunction)
	    {
		// Some 'normal' functions need special coding, i.e, arithmetic and comparisons,
		// so this is called for any function with a compiler, not just special forms.
		final LispTreeFunction compiler = (LispTreeFunction)lispFunction;
		return compiler.compile (this, expression, resultDesired);
	    }
	}
	catch (final DontOptimize e)
	{
	    // Ignore and continue
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
		// FIXME Test macros
		final Object replacement = macro.expand (expression);
		return compile (replacement, resultDesired);
	    }
	    catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e1)
	    {
		throw new Error ("Error expanding macro " + symbol, e1);
	    }
	}
	return compileOptimizedFunctionCall (expression, resultDesired);
    }

    /**
     * Compile a static method call.
     *
     * @throws SecurityException
     * @throws NoSuchFieldException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     */
    private CompileResults compileDotFunctionCall (final LispList expression)
            throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException
    {
	// (define foo (x) (java.lang.String.format "foo %s bar %n" x))
	// (define foo (x) (System.out.println x))
	final LispList dot = expression.getSublist (0);
	final Object target = dot.get (1);
	final String methodName = (String)dot.get (2);

	if (target instanceof Class)
	{
	    final Class<?> claz = (Class<?>)target;
	    final Method selectedMethod = selectMethod.selectStaticMethod (claz, methodName, locals, expression);
	    if (selectedMethod == null)
	    {
		throw new Error (String.format ("No %s method found on %s for %s", methodName, claz, expression));
	    }
	    else if (selectedMethod.isVarArgs ())
	    {
		// Function call always returns a value whether we want it or not
		return compileVarArgsFunctionCall (null, selectedMethod, expression);
	    }
	    else
	    {
		// Function call always returns a value whether we want it or not
		return compileFixedFunctionCall (null, selectedMethod, expression);
	    }
	}
	else if (target instanceof List)
	{
	    // target is (field <class> <fieldName>)
	    final List<?> fieldForm = (List<?>)target;
	    final Class<?> fieldOf = (Class<?>)fieldForm.get (1);
	    final Type fieldOfType = Type.getType (fieldOf);
	    final String fieldName = (String)fieldForm.get (2);
	    final Field field = fieldOf.getField (fieldName);
	    final Object fieldValue = field.get (fieldOf);
	    final Class<?> claz = fieldValue.getClass ();
	    final Type fieldType = Type.getType (claz);
	    final Method selectedMethod = selectMethod.selectMethod (claz, methodName, locals, expression);
	    if (selectedMethod == null)
	    {
		throw new Error (String.format ("No %s method found on %s for %s", methodName, claz, expression));
	    }
	    else if (selectedMethod.isVarArgs ())
	    {
		// Function call always returns a value whether we want it or not
		il.add (new FieldInsnNode (GETSTATIC, fieldOfType.getInternalName (), fieldName, fieldType.getDescriptor ()));
		// Need version that does not compile the target
		return compileVarArgsFunctionCallNoTarget (selectedMethod, expression);
	    }
	    else
	    {
		// (define foo (x) (System.out.println x))
		// Function call always returns a value whether we want it or not
		il.add (new FieldInsnNode (GETSTATIC, fieldOfType.getInternalName (), fieldName, fieldType.getDescriptor ()));
		return compileFixedFunctionCallNoTarget (selectedMethod, expression);
	    }
	}
	// mv.visitMethodInsn(INVOKESTATIC, "java/lang/String", "format",
	// "(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;", false);
	throw new Error ("Could not compile " + expression);
    }

    private CompileResults compileOptimizedFunctionCall (final LispList expression, final boolean resultDesired)
    {
	final Symbol symbol = expression.head ();
	final FunctionCell function = symbol.getFunction ();
	if (function != null)
	{
	    if (!(function instanceof DefaultFunctionCell))
	    {
		final Overload objectMethod = function.selectMethod (locals, expression);
		if (objectMethod != null)
		{
		    try
		    {
			final LispFunction lispFunction = objectMethod.getLispFunction ();
			if (lispFunction instanceof LispTreeFunction)
			{
			    // Some 'normal' functions need special coding, i.e, arithmetic and
			    // comparisons,
			    // so this is called for any function with a compiler, not just special
			    // forms.
			    final LispTreeFunction compiler = (LispTreeFunction)lispFunction;
			    return compiler.compile (this, expression, resultDesired);
			}
		    }
		    catch (final DontOptimize e)
		    {
			// Ignore and continue
		    }

		    if (symbol.getPackage ().getName ().equals ("lisp.lang") && Symbol.test ("optimizeFunctionCalls", true))
		    {
			if (objectMethod.isVarArgs ())
			{
			    // Function call always returns a value whether we want it or not
			    return compileVarArgsFunctionCall (objectMethod, expression);
			}
			else
			{
			    // Function call always returns a value whether we want it or not
			    return compileFixedFunctionCall (objectMethod, expression);
			}
		    }
		}
	    }
	}
	return null;
    }

    /** Compile a function all into to directly call the given method. */
    private CompileResults compileVarArgsFunctionCall (final Overload objectMethod, final LispList expression)
    {
	// (setq showBytecode t)
	// (define foo (a b) (+ a b))
	return compileVarArgsFunctionCall (objectMethod.getObject (), objectMethod.getMethod (), expression);
    }

    /** Compile a function all into to directly call the given method. */
    private CompileResults compileFixedFunctionCall (final Overload objectMethod, final LispList expression)
    {
	// (setq showBytecode t)
	// (define foo () (getDefaultPackage))
	// (define foo (x) (1+ x))
	// (define foo (x) (not x))
	// (define foo (a b) (rem a b))

	return compileFixedFunctionCall (objectMethod.getObject (), objectMethod.getMethod (), expression);
    }

    /** Compile a function all into to directly call the given method. */
    // private CompileResultSet compileVarArgsFunctionCall (final Object target, final Method
    // method, final LispList expression)
    // {
    // LOGGER.fine (new LogString ("Optimized call to %s using %s on %s", expression.get (0),
    // method, target));
    // final String methodSignature = TreeCompilerContext.methSignature.getMethodSignature (method);
    // final Label l1 = new Label ();
    // add (new LabelNode (l1));
    //
    // final String classInternalName = treeCompiler.getClassType ().getInternalName ();
    // if (target == null)
    // {
    // add (new InsnNode (ACONST_NULL));
    // }
    // else
    // {
    // final Type objectType = Type.getType (target.getClass ());
    // final Symbol reference = quotedData.addQuotedConstant (target);
    // add (new VarInsnNode (ALOAD, 0));
    // add (new FieldInsnNode (GETFIELD, classInternalName, reference.getName (),
    // objectType.getDescriptor ()));
    // }
    // // Compile arguments here
    // final Class<?>[] params = method.getParameterTypes ();
    // for (int i = 0; i < params.length - 1; i++)
    // {
    // final Object arg = expression.get (i + 1);
    // final Class<?> argClass = params[i];
    // final CompileResultSet rs = compile (arg, true);
    // convert (rs, argClass, false, false);
    // }
    // final Class<?> argClass = params[params.length - 1].getComponentType ();
    // final Type argType = Type.getType (argClass);
    // final int optcount = expression.size () - params.length;
    // il.add (new LdcInsnNode (optcount));
    // il.add (new TypeInsnNode (ANEWARRAY, argType.getInternalName ()));
    // for (int i = 0; i < optcount; i++)
    // {
    // il.add (new InsnNode (DUP));
    // il.add (new LdcInsnNode (i));
    // final Object arg = expression.get (i + params.length);
    // final CompileResultSet rs = compile (arg, true);
    // convert (rs, argClass, false, false);
    // il.add (new InsnNode (AASTORE));
    // }
    // if (Modifier.isStatic (method.getModifiers ()))
    // {
    // final String owner = Type.getType (method.getDeclaringClass ()).getInternalName ();
    // add (new MethodInsnNode (INVOKESTATIC, owner, method.getName (), methodSignature, false));
    // }
    // else
    // {
    // final Type objectType = Type.getType (target.getClass ());
    // final String objectClassInternalName = objectType.getInternalName ();
    // add (new MethodInsnNode (INVOKEVIRTUAL, objectClassInternalName, method.getName (),
    // methodSignature, false));
    // }
    // final Class<?> methodValueClass = method.getReturnType ();
    // final CompileResultSet result = new CompileResultSet ();
    // final LabelNode ll = new LabelNode ();
    // result.addExplicitCompileResult (ll, methodValueClass);
    // add (new JumpInsnNode (GOTO, ll));
    // return result;
    // }

    /** Compile a function all into to directly call the given method. */
    private CompileResults compileVarArgsFunctionCall (final Object target, final Method method, final LispList expression)
    {
	LOGGER.fine (new LogString ("Optimized call to %s using %s on %s", expression.get (0), method, target));
	// final String methodSignature = TreeCompilerContext.methSignature.getMethodSignature
	// (method);
	// final Label l1 = new Label ();
	// add (new LabelNode (l1));

	final String classInternalName = treeCompiler.getClassType ().getInternalName ();
	if (target == null)
	{
	    add (new InsnNode (ACONST_NULL));
	}
	else
	{
	    final Type objectType = Type.getType (target.getClass ());
	    final Symbol reference = quotedData.addQuotedConstant (target);
	    add (new VarInsnNode (ALOAD, 0));
	    add (new FieldInsnNode (GETFIELD, classInternalName, reference.getName (), objectType.getDescriptor ()));
	}
	// final Type targetType = null;// Type.getType (target.getClass ());
	return compileVarArgsFunctionCallNoTarget (method, expression);
    }

    /**
     * Compile a function all into to directly call the given method. This version does not produce
     * instructions to load the target onto the stack.
     */
    private CompileResults compileVarArgsFunctionCallNoTarget (final Method method, final LispList expression)
    {
	LOGGER.fine (new LogString ("Optimized call to %s using %s", expression.get (0), method));
	final String methodSignature = TreeCompilerContext.methSignature.getMethodSignature (method);

	// Compile arguments here
	final Class<?>[] params = method.getParameterTypes ();
	for (int i = 0; i < params.length - 1; i++)
	{
	    final Object arg = expression.get (i + 1);
	    final Class<?> argClass = params[i];
	    final CompileResults rs = compile (arg, true);
	    convert (rs, argClass, false, false);
	}
	final Class<?> argClass = params[params.length - 1].getComponentType ();
	final Type argType = Type.getType (argClass);
	final int optcount = expression.size () - params.length;
	il.add (new LdcInsnNode (optcount));
	il.add (new TypeInsnNode (ANEWARRAY, argType.getInternalName ()));
	for (int i = 0; i < optcount; i++)
	{
	    il.add (new InsnNode (DUP));
	    il.add (new LdcInsnNode (i));
	    final Object arg = expression.get (i + params.length);
	    final CompileResults rs = compile (arg, true);
	    convert (rs, argClass, false, false);
	    il.add (new InsnNode (AASTORE));
	}
	final String ownerType = Type.getType (method.getDeclaringClass ()).getInternalName ();
	if (Modifier.isStatic (method.getModifiers ()))
	{
	    add (new MethodInsnNode (INVOKESTATIC, ownerType, method.getName (), methodSignature, false));
	}
	else
	{
	    // final Type objectType = Type.getType (target.getClass ());
	    // final String objectClassInternalName = targetType.getInternalName ();
	    add (new MethodInsnNode (INVOKEVIRTUAL, ownerType, method.getName (), methodSignature, false));
	}
	final Class<?> methodValueClass = method.getReturnType ();
	final CompileResults result = new CompileResults ();
	final LabelNode ll = new LabelNode ();
	result.addExplicitCompileResult (ll, methodValueClass);
	add (new JumpInsnNode (GOTO, ll));
	return result;
    }

    /** Compile a function all into to directly call the given method. */
    private CompileResults compileFixedFunctionCall (final Object target, final Method method, final LispList expression)
    {
	LOGGER.fine (new LogString ("Optimized call to %s using %s on %s", expression.get (0), method, target));
	// final String methodSignature = TreeCompilerContext.methSignature.getMethodSignature
	// (method);
	// If we are compiling for speed and can assume that the current definition won't
	// change, then compile a direct call to the current function method.
	// TODO If we know argument types of the function we are about to call we can try to
	// compile the expression more efficiently.
	// final Label l1 = new Label ();
	// add (new LabelNode (l1));

	// final Symbol reference = symbol.gensym ();
	// final String methodSignature = objectMethod.getSignature ();
	if (target == null)
	{
	    add (new InsnNode (ACONST_NULL));
	}
	else
	{
	    final Type objectType = Type.getType (target.getClass ());
	    final Symbol reference = quotedData.addQuotedConstant (target);
	    add (new VarInsnNode (ALOAD, 0));
	    final String classInternalName = treeCompiler.getClassType ().getInternalName ();
	    add (new FieldInsnNode (GETFIELD, classInternalName, reference.getName (), objectType.getDescriptor ()));
	}
	return compileFixedFunctionCallNoTarget (method, expression);
    }

    private CompileResults compileFixedFunctionCallNoTarget (final Method method, final LispList expression)
    {
	// Compile arguments here
	final Class<?>[] params = method.getParameterTypes ();
	for (int i = 0; i < params.length; i++)
	{
	    final Object arg = expression.get (i + 1);
	    final Class<?> argType = params[i];
	    final CompileResults rs = compile (arg, true);
	    convert (rs, argType, false, false);
	}
	final String methodSignature = TreeCompilerContext.methSignature.getMethodSignature (method);
	final String ownerType = Type.getType (method.getDeclaringClass ()).getInternalName ();
	if (Modifier.isStatic (method.getModifiers ()))
	{
	    add (new MethodInsnNode (INVOKESTATIC, ownerType, method.getName (), methodSignature, false));
	}
	else
	{
	    // final Type objectType = Type.getType (target.getClass ());
	    // final String objectClassInternalName = objectType.getInternalName ();
	    add (new MethodInsnNode (INVOKEVIRTUAL, ownerType, method.getName (), methodSignature, false));
	}
	final Class<?> methodValueClass = method.getReturnType ();
	final CompileResults result = new CompileResults ();
	final LabelNode ll = new LabelNode ();
	result.addExplicitCompileResult (ll, methodValueClass);
	add (new JumpInsnNode (GOTO, ll));
	return result;
    }

    /**
     * Compile a function call and add it to the instruction list. This creates a generic function
     * call to any normal Lisp function. This involves loading the FunctionCell from the function
     * symbol, evaluating the arguments and calling the FunctionCell.
     * <p>
     * This has a lot of efficiency problems. Getting to the FunctionCell requires some pointer
     * chasing. The arguments must be passed by VarArgs semantics since we don't know for sure how
     * many there are, so the calling code produced here will allocate an array for the arguments.
     * The function cell must unpack the array and may need to create a new one, if the actual
     * underlying primitive expects a different VarArgs calling sequence. And, since all arguments
     * must be objects there may be wrapping and unwrapping of primitive types involved. Without
     * knowing the number and type of the arguments expected by the function we are calling there is
     * not much to be done to improve the situation without creating a new set of calling
     * conventions different than the Java VarArgs convention.
     * </p>
     * <p>
     * One option would be to push the arguments onto the stack in reverse order, followed by an
     * argument count. The function being called could then pick up fixed arguments at known
     * locations relative to the top of the stack. Optional or VarArgs arguments could be referenced
     * as a virtual array on the stack. This array would silently go away when the function returns
     * so it could not be made into a real pointer.
     * </p>
     *
     * @param il The instruction list.
     * @param locals Local variable binding information.
     * @param expression The function call expression.
     * @return The class of the return value.
     */
    private CompileResults compileDefaultFunctionCall (final LispList expression)
    {
	final Symbol symbol = expression.head ();
	quotedData.addSymbolReference (symbol);
	LOGGER.finer (new LogString ("Function symbol reference to %s", symbol));
	il.add (new VarInsnNode (ALOAD, 0));
	final Type classType = treeCompiler.getClassType ();
	final String classInternalName = classType.getInternalName ();
	il.add (new FieldInsnNode (GETFIELD, classInternalName, javaName.createJavaSymbolName (symbol), "Llisp/lang/Symbol;"));

	// Get the FunctionCell from the function symbol.
	// The call to getDefaultHandlerFunction will return a DefaultHandler that tries to invoke
	// the java method on arg 1 if the function has not been given any other definition.
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/lang/Symbol", "getDefaultHandlerFunction",
	        "()Llisp/symbol/FunctionCell;", false));

	// Compile the arguments. Pass all the arguments as elements of a single array of Objects.
	final int argCount = expression.size () - 1;
	il.add (new LdcInsnNode (argCount));
	il.add (new TypeInsnNode (ANEWARRAY, "java/lang/Object"));
	for (int i = 0; i < argCount; i++)
	{
	    il.add (new InsnNode (DUP));
	    il.add (new LdcInsnNode (i));
	    final CompileResults resultDescriptor = compile (expression.get (i + 1), true);
	    convert (resultDescriptor, Object.class, false, false);
	    il.add (new InsnNode (AASTORE));
	}

	// Call invoke on the method retrieved from the FunctionCell of the function symbol.
	// Assume the function will still be defined when we execute this code.
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/symbol/FunctionCell", "apply", "([Ljava/lang/Object;)Ljava/lang/Object;",
	        false));
	final LabelNode ll = new LabelNode ();
	il.add (new JumpInsnNode (GOTO, ll));
	return new CompileResults (new ExplicitResult (ll, Object.class));
    }

    /**
     * Compile an expression to calculate the value of a symbol. This determines if the symbol is an
     * argument, local variable or global symbol and calculates the correct value. <br/>
     * FIXME Constants and typed variables should be handled specially.
     *
     * @param mv The bytecode generator.
     * @param symbol The symbol value to calculate.
     * @return The class of the result produced.
     */
    private CompileResults compileSymbolReference (final Symbol symbol)
    {
	if (locals.containsKey (symbol))
	{
	    // Reference to a local lexical variable
	    final LexicalBinding lb = locals.get (symbol);
	    // final int localRef = lb.getLocalRef ();
	    // final Type varType = lb.getType ();
	    // il.add (new VarInsnNode (varType.getOpcode (ILOAD), localRef));
	    lb.loadValue (il);
	    final Class<?> fromClass = lb.getVariableClass ();
	    final LabelNode ll = new LabelNode ();
	    il.add (new JumpInsnNode (GOTO, ll));
	    return new CompileResults (new ExplicitResult (ll, fromClass));
	}
	// Handle some simple constants
	else if (symbol.is ("true") || symbol.is ("t"))
	{
	    final LabelNode ll = new LabelNode ();
	    add (new JumpInsnNode (GOTO, ll));
	    return new CompileResults (new ImplicitResult (ll, true));
	}
	else if (symbol.is ("false") || symbol.is ("f"))
	{
	    final LabelNode ll = new LabelNode ();
	    add (new JumpInsnNode (GOTO, ll));
	    return new CompileResults (new ImplicitResult (ll, false));
	}
	// TODO Handle references to standard Java classes (like java.lang.*)
	else
	{
	    // Reference to a global variable
	    addGlobalReference (symbol); // Log message
	    quotedData.addSymbolReference (symbol); // Make symbol available at execution time
	    // FIXME If the symbol valueCell is constant, use the current value.
	    // FIXME If the valueCell is a TypedValueCell, use the type information.
	    il.add (new VarInsnNode (ALOAD, 0));
	    final Type classType = treeCompiler.getClassType ();
	    final String classInternalName = classType.getInternalName ();
	    il.add (new FieldInsnNode (GETFIELD, classInternalName, javaName.createJavaSymbolName (symbol),
	            "Llisp/lang/Symbol;"));
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/lang/Symbol", "getValue", "()Ljava/lang/Object;", false));

	    final LabelNode ll = new LabelNode ();
	    il.add (new JumpInsnNode (GOTO, ll));
	    return new CompileResults (new ExplicitResult (ll, Object.class));
	}
    }

    /**
     * Keep track of a symbol that has a global reference. This is only used to produce a log
     * message. globalReferences does nothing else.
     */
    public void addGlobalReference (final Symbol symbol)
    {
	if (!globalReferences.contains (symbol))
	{
	    globalReferences.add (symbol);
	    LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
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
