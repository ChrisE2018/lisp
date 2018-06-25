
package lisp.cc4;

import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.LabelNode;

import lisp.asm.instructions.FieldInsnNode;
import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.LdcInsnNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.TypeInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.cc.*;
import lisp.lang.*;
import lisp.lang.Symbol;
import lisp.util.LogString;

public class TreeCompiler extends ClassNode implements Opcodes, TreeCompilerInterface, QuotedData
{
    private static final Logger LOGGER = Logger.getLogger (TreeCompiler.class.getName ());
    private static JavaName javaName = new JavaName ();
    private static Symbol QUOTE_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("quote");
    private final CompileLoaderV4 compileLoader;
    private final Type returnType;
    private final Class<?> methodReturnClass;
    private final String methodName;
    private final LispList methodBody;

    private final List<Symbol> methodArgs = new ArrayList<Symbol> ();
    private final List<Class<?>> methodArgClasses = new ArrayList<Class<?>> ();
    private final List<Type> methodArgTypes = new ArrayList<Type> ();

    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();
    private final Map<Symbol, Object> quotedReferences;

    public TreeCompiler (final ClassVisitor cv, final CompileLoaderV4 compileLoader, final Class<?> methodReturnClass,
            final String methodName, final LispList methodArgs, final LispList methodBody)
    {
	// '(setq system.compilerVersion "V4")' "(setq system.showBytecode t)"
	// (define foo () 1)
	super (Opcodes.ASM5);
	this.cv = cv;

	this.compileLoader = compileLoader;
	this.methodReturnClass = methodReturnClass;
	this.methodName = methodName;
	this.methodBody = methodBody;
	quotedReferences = compileLoader.getQuotedData ();

	returnType = Type.getType (methodReturnClass);
	for (final Object arg : methodArgs)
	{
	    final Symbol variable = NameSpec.getVariableName (arg);
	    final Class<?> varClass = NameSpec.getVariableClass (arg);
	    final Type varType = Type.getType (varClass);
	    this.methodArgs.add (variable);
	    methodArgClasses.add (varClass);
	    methodArgTypes.add (varType);
	}
    }

    /** The number of arguments to the function currently being compiled. */
    public int getArgCount ()
    {
	return methodArgs.size ();
    }

    /**
     * The return value class of the function currently being compiled.
     *
     * @return The class of the function return value.
     */
    public Class<?> getMethodReturnClass ()
    {
	return methodReturnClass;
    }

    /** The ASM type of the class enclosing the function currently being compiled. */
    public Type getClassType ()
    {
	return compileLoader.getClassType ();
    }

    /** Keep track of a symbol that needs to be available as a class field. */
    public void addSymbolReference (final Symbol symbol)
    {
	if (!symbolReferences.contains (symbol))
	{
	    symbolReferences.add (symbol);
	}
    }

    // private final Set<Symbol> globalReferences = new HashSet<Symbol> ();

    // /**
    // * Keep track of a symbol that has a global reference. This is only used to produce a log
    // * message. globalReferences does nothing else.
    // */
    // private void addGlobalReference (final Symbol symbol)
    // {
    // if (!globalReferences.contains (symbol))
    // {
    // globalReferences.add (symbol);
    // LOGGER.finer (new LogString ("Compiled global assignment to %s", symbol));
    // }
    // }

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param quoted The quoted data to be stored.
     * @return The symbol that will name the data field. This is a generated unique symbol.
     */
    @Override
    public Symbol addQuotedConstant (final Object quoted)
    {
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    if (quoted.equals (entry.getValue ()))
	    {
		return entry.getKey ();
	    }
	}
	final Symbol reference = QUOTE_SYMBOL.gensym ();
	quotedReferences.put (reference, quoted);
	return reference;
    }

    /** Get the saved quoted data. */
    @Override
    public Map<Symbol, Object> getQuotedData ()
    {
	return quotedReferences;
    }

    @Override
    public void visitEnd ()
    {
	// put your transformation code here
	final MethodNode userMethod = getCompiledMethod ();
	final MethodNode initMethod = getInitMethod ();

	// Create field definitions for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String typeDescriptor = Type.getType (symbol.getClass ()).getDescriptor ();
	    fields.add (new FieldNode (ACC_PRIVATE, javaName.createJavaSymbolName (symbol), typeDescriptor, null, null));
	}
	addRequiredFields (this);

	methods.add (initMethod);
	if (symbolReferences.size () > 0)
	{
	    final MethodNode symbolMethod = getGetSymbolMethod ();
	    methods.add (symbolMethod);
	}
	methods.add (userMethod);

	/** May want to save the whole ClassNode instead. */
	compileLoader.setClassNode (this);
	compileLoader.setMethodNode (userMethod);

	// Chain transformation
	accept (cv);
    }

    @Override
    public void addRequiredFields (final ClassNode cn)
    {
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    final String reference = entry.getKey ().getName ();
	    final Object quoted = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    cn.fields.add (new FieldNode (ACC_PRIVATE, reference, typeDescriptor, null, null));
	}
    }

    private MethodNode getInitMethod ()
    {
	final MethodNode mn = new MethodNode (ACC_PUBLIC, "<init>", "(I)V", null, null);
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (ALOAD, 0));
	il.add (new MethodInsnNode (INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false));

	// Do other things here...
	final String classInternalName = getClassType ().getInternalName ();

	final Type objectType = Type.getType (Object.class);
	final Type symbolType = Type.getType (Symbol.class);
	final String symbolTypeDescriptor = symbolType.getDescriptor ();
	final Type stringType = Type.getType (String.class);
	final Type classLoaderType = Type.getType (CompileLoaderV4.class);
	final String classLoaderInternalName = classLoaderType.getInternalName ();

	// Create initialization code for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String javaSymbolName = javaName.createJavaSymbolName (symbol);
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new LdcInsnNode (symbol.getPackage ().getName ()));
	    il.add (new LdcInsnNode (symbol.getName ()));
	    il.add (new MethodInsnNode (INVOKESPECIAL, classInternalName, "getSymbol",
	            Type.getMethodDescriptor (symbolType, stringType, stringType), false));
	    il.add (new FieldInsnNode (PUTFIELD, classInternalName, javaSymbolName, symbolTypeDescriptor));

	    LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaSymbolName, symbol));
	}
	for (final Entry<Symbol, Object> entry : quotedReferences.entrySet ())
	{
	    // (define foo () (quote bar))
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new InsnNode (DUP));
	    final Type classType = Type.getType (Class.class);
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, objectType.getInternalName (), "getClass",
	            Type.getMethodDescriptor (classType), false));
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, classType.getInternalName (), "getClassLoader",
	            "()Ljava/lang/ClassLoader;", false));
	    il.add (new TypeInsnNode (CHECKCAST, classLoaderInternalName));
	    il.add (new MethodInsnNode (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;",
	            false));

	    final String reference = entry.getKey ().getName ();
	    final Object quoted = entry.getValue ();
	    il.add (new LdcInsnNode (reference));
	    il.add (new MethodInsnNode (INVOKEINTERFACE, "java/util/Map", "get",
	            Type.getMethodDescriptor (objectType, objectType), true));
	    final Type quotedType = Type.getType (quoted.getClass ());
	    final String typeDescriptor = quotedType.getDescriptor ();
	    il.add (new TypeInsnNode (CHECKCAST, quotedType.getInternalName ()));
	    il.add (new FieldInsnNode (PUTFIELD, classInternalName, reference, typeDescriptor));
	}

	// Standard init method return
	il.add (new InsnNode (RETURN));
	mn.maxStack = 0;
	mn.maxLocals = 1;
	return mn;
    }

    /** Create a method to locate a Symbol at runtime. */
    private MethodNode getGetSymbolMethod ()
    {
	final MethodNode mn =
	    new MethodNode (ACC_PRIVATE, "getSymbol", "(Ljava/lang/String;Ljava/lang/String;)Llisp/lang/Symbol;", null, null);
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (ALOAD, 1));
	il.add (new MethodInsnNode (INVOKESTATIC, "lisp/lang/PackageFactory", "getPackage",
	        "(Ljava/lang/String;)Llisp/lang/Package;", false));
	il.add (new VarInsnNode (ALOAD, 2));
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/lang/Package", "findSymbol", "(Ljava/lang/String;)Llisp/lang/Symbol;",
	        false));
	il.add (new InsnNode (ARETURN));

	mn.maxStack = 0;
	mn.maxLocals = 0;
	return mn;
    }

    /* Create a method to implement the user function being defined. */
    private MethodNode getCompiledMethod ()
    {
	final MethodNode mn = new MethodNode (ACC_PUBLIC, methodName, getMethodSignature (), null, null);
	final Map<Symbol, LexicalBinding> locals = new LinkedHashMap<Symbol, LexicalBinding> ();
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    final Symbol arg = methodArgs.get (i);
	    locals.put (arg, new LexicalVariable (arg, methodArgClasses.get (i), i + 1));
	}
	// Should pass mn to the TreeCompilerContext so it can get at the method locals.
	final List<BlockBinding> bbs = new ArrayList<> ();
	final LabelNode l1 = new LabelNode ();
	final BlockBinding bb = new BlockBinding (null, methodReturnClass, l1);
	bbs.add (bb);
	final TreeCompilerContext context = new TreeCompilerContext (this, this, methodReturnClass, mn, locals, bbs);
	for (int i = 0; i < methodBody.size () - 1; i++)
	{
	    final Object expr = methodBody.get (i);
	    final CompileResultSet resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	final CompileResultSet resultClass = context.compile (methodBody.last (), true);
	// (define double:foo (int:n) 1 2 n)
	for (final CompileResult resultKind : resultClass.getResults ())
	{
	    // TODO This should collect crs of the same class and make them jump to the same place
	    // instead of duplicating code
	    context.add (resultKind.getLabels ());
	    if (resultKind instanceof ExplicitCompileResult)
	    {
		context.convert (((ExplicitCompileResult)resultKind).getResultClass (), methodReturnClass, false, false);
	    }
	    else
	    {
		final ImplicitCompileResult icr = (ImplicitCompileResult)resultKind;
		context.add (icr, methodReturnClass);
	    }
	    context.add (new InsnNode (returnType.getOpcode (IRETURN)));
	}
	// Need to optimize away unreachable code
	context.add (l1);
	context.add (new InsnNode (returnType.getOpcode (IRETURN)));
	// Better not get here
	mn.maxStack = 0;
	mn.maxLocals = 0;
	return mn;
    }

    private String getMethodSignature ()
    {
	final String returnTypeDescriptor = returnType.getDescriptor ();
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("(");
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    // buffer.append (CompileSupport.getNameTypeDescriptor (methodArgTypes.get (i)));
	    buffer.append (Type.getType (methodArgClasses.get (i)).getDescriptor ());
	}
	buffer.append (")");
	buffer.append (returnTypeDescriptor);
	return buffer.toString ();
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

    @Override
    public void addHiddenConstructorSteps (final Type classType, final MethodVisitor mv)
    {
	throw new UnsupportedOperationException ();
    }
}
