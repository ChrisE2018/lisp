
package lisp.cc4;

import java.util.*;
import java.util.Map.Entry;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.tree.*;

import lisp.*;
import lisp.Symbol;
import lisp.asm.instructions.FieldInsnNode;
import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.LdcInsnNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.TypeInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.cc.*;
import lisp.util.LogString;

public class TreeCompiler extends ClassNode implements Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (TreeCompiler.class.getName ());
    private static Symbol QUOTE_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("quote");
    private final CompileLoader compileLoader;
    private final Type returnType;
    private final Class<?> methodReturnClass;
    private final String methodName;
    private final LispList methodBody;
    private final Map<String, Object> quotedReferences;

    private final List<Symbol> methodArgs = new ArrayList<Symbol> ();
    private final List<Class<?>> methodArgClasses = new ArrayList<Class<?>> ();
    private final List<Type> methodArgTypes = new ArrayList<Type> ();
    private final Set<Symbol> globalReferences = new HashSet<Symbol> ();
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();

    public TreeCompiler (final ClassVisitor cv, final CompileLoader compileLoader, final Class<?> methodReturnClass,
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
	quotedReferences = compileLoader.getQuotedReferences ();

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

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param reference The symbol that will name the data field.
     * @param quoted The quoted data to be stored.
     */
    public void addQuotedConstant (final Symbol reference, final Object quoted)
    {
	quotedReferences.put (reference.getName (), quoted);
    }

    /**
     * Arrange for a field to be added to the compilation class containing quoted data.
     *
     * @param quoted The quoted data to be stored.
     * @return The symbol that will name the data field. This is a generated unique symbol.
     */
    public Symbol addQuotedConstant (final Object quoted)
    {
	final Symbol reference = QUOTE_SYMBOL.gensym ();
	quotedReferences.put (reference.getName (), quoted);
	return reference;
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
	    fields.add (new FieldNode (ACC_PRIVATE, createJavaSymbolName (symbol), typeDescriptor, null, null));
	}
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    fields.add (new FieldNode (ACC_PRIVATE, reference, typeDescriptor, null, null));
	}

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
	final Type classLoaderType = Type.getType (CompileLoader.class);
	final String classLoaderInternalName = classLoaderType.getInternalName ();

	// Create initialization code for all entries in symbolReferences.
	for (final Symbol symbol : symbolReferences)
	{
	    final String javaName = createJavaSymbolName (symbol);
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new VarInsnNode (ALOAD, 0));
	    il.add (new LdcInsnNode (symbol.getPackage ().getName ()));
	    il.add (new LdcInsnNode (symbol.getName ()));
	    il.add (new MethodInsnNode (INVOKESPECIAL, classInternalName, "getSymbol",
	            Type.getMethodDescriptor (symbolType, stringType, stringType), false));
	    il.add (new FieldInsnNode (PUTFIELD, classInternalName, javaName, symbolTypeDescriptor));

	    LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaName, symbol));
	}
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    // (define foo () (quote bar))
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
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
	    new MethodNode (ACC_PRIVATE, "getSymbol", "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", null, null);
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (ALOAD, 1));
	il.add (new MethodInsnNode (INVOKESTATIC, "lisp/PackageFactory", "getPackage", "(Ljava/lang/String;)Llisp/Package;",
	        false));
	il.add (new VarInsnNode (ALOAD, 2));
	il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Package", "findSymbol", "(Ljava/lang/String;)Llisp/Symbol;", false));
	il.add (new InsnNode (ARETURN));

	mn.maxStack = 0;
	mn.maxLocals = 0;
	return mn;
    }

    // /** Create a method to get a Symbol value at runtime. */
    // private MethodNode getGetSymbolValueMethod ()
    // {
    // final MethodNode mn =
    // new MethodNode (ACC_PRIVATE, "getSymbolValue",
    // "(Ljava/lang/String;Ljava/lang/String;)Llisp/Symbol;", null, null);
    // final InsnList il = mn.instructions;
    // il.add (new VarInsnNode (ALOAD, 1));
    // il.add (new MethodInsnNode (INVOKESTATIC, "lisp/PackageFactory", "getPackage",
    // "(Ljava/lang/String;)Llisp/Package;",
    // false));
    // il.add (new VarInsnNode (ALOAD, 2));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Package", "findSymbol",
    // "(Ljava/lang/String;)Llisp/Symbol;", false));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "lisp/Symbol", "getValue", "()Ljava/lang/Object;",
    // false));
    // il.add (new InsnNode (ARETURN));
    //
    // mn.maxStack = 0;
    // mn.maxLocals = 0;
    // return mn;
    // }

    /* Create a method to implement the user function being defined. */
    private MethodNode getCompiledMethod ()
    {
	final MethodNode mn = new MethodNode (ACC_PUBLIC, methodName, getMethodSignature (), null, null);
	final Map<Symbol, LocalBinding> locals = new LinkedHashMap<Symbol, LocalBinding> ();
	for (int i = 0; i < methodArgs.size (); i++)
	{
	    final Symbol arg = methodArgs.get (i);
	    locals.put (arg, new LocalBinding (arg, methodArgClasses.get (i), i + 1));
	}
	// Should pass mn to the TreeCompilerContext so it can get at the method locals.
	final TreeCompilerContext context = new TreeCompilerContext (this, mn, locals);
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
		context.add (icr);
	    }
	    context.add (new InsnNode (returnType.getOpcode (IRETURN)));
	}
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

    /**
     * Convert a lisp symbol name into a valid Java identifier String. The same symbol will always
     * convert to the same Java name. Symbols of the same name in different packages may produce
     * conflicts.
     *
     * @param symbol The symbol to extract a name from.
     * @return A valid Java identifier corresponding to the symbol.
     */
    public String createJavaSymbolName (final Symbol symbol)
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("SYM_");
	final String symbolName = symbol.getName ();
	for (int i = 0; i < symbolName.length (); i++)
	{
	    final char c = symbolName.charAt (i);
	    if (Character.isJavaIdentifierPart (c))
	    {
		buffer.append (c);
	    }
	    else
	    {
		final int codePoint = symbolName.codePointAt (i);
		final String cn = Character.getName (codePoint);
		if (cn != null)
		{
		    buffer.append (c);
		}
		else
		{
		    buffer.append (String.format ("%03d", codePoint));
		}
	    }
	}
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
}
