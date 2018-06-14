
package lisp.cc;

import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.AdviceAdapter;
import org.objectweb.asm.tree.*;

import lisp.*;
import lisp.Symbol;
import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.cc4.*;
import lisp.util.*;

public class Defclass extends ClassNode implements TreeCompilerInterface, Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (Defclass.class.getName ());
    private static Boxing boxing = new Boxing ();
    private static JavaName javaName = new JavaName ();
    private static Symbol THIS_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("this");
    private static final Type OBJECT_TYPE = Type.getType (Object.class);

    private static final Object[][] CLASS_MODIFIERS_DATA =
        {
         {"public", Opcodes.ACC_PUBLIC},
         {"private", Opcodes.ACC_PRIVATE},
         {"protected", Opcodes.ACC_PROTECTED},
         {"default", 0},
         {"abstract", Opcodes.ACC_ABSTRACT},
         {"final", Opcodes.ACC_FINAL}};

    private static final Object[][] FIELD_MODIFIERS_DATA =
        {
         {"public", Opcodes.ACC_PUBLIC},
         {"private", Opcodes.ACC_PRIVATE},
         {"protected", Opcodes.ACC_PROTECTED},
         {"default", 0},
         {"final", Opcodes.ACC_FINAL}};

    private static final Object[][] METHOD_MODIFIERS_DATA =
        {
         {"public", Opcodes.ACC_PUBLIC},
         {"private", Opcodes.ACC_PRIVATE},
         {"protected", Opcodes.ACC_PROTECTED},
         {"default", 0},
         {"abstract", Opcodes.ACC_ABSTRACT},
         {"final", Opcodes.ACC_FINAL}};

    private static final List<Object[]> CLASS_MODIFIERS = Arrays.asList (CLASS_MODIFIERS_DATA);
    private static final List<Object[]> FIELD_MODIFIERS = Arrays.asList (FIELD_MODIFIERS_DATA);
    private static final List<Object[]> CONSTRUCTOR_MODIFIERS = Arrays.asList (METHOD_MODIFIERS_DATA);
    private static final List<Object[]> METHOD_MODIFIERS = Arrays.asList (METHOD_MODIFIERS_DATA);

    private final LispClassLoader classLoader;
    private final String classSimpleName;
    private final Type classType;
    private Class<?> superclass = Object.class;
    private Type superclassType = Type.getType (Object.class);
    private final List<Class<?>> interfaceClasses = new ArrayList<Class<?>> ();
    private boolean hasConstructor = false;

    public Defclass (final int api, final LispClassLoader classLoader, final String name, final LispList[] members)
    {
	super (api);
	this.classLoader = classLoader;
	classSimpleName = name;
	classType = Type.getType ("Llisp/cc/" + classSimpleName + ";");
	parse (members);
	// parse2 (members);
	if (!hasConstructor)
	{
	    addDefaultInitMethod ();
	}
	final int hiddenFieldAccess = Opcodes.ACC_PRIVATE;
	// Create field definitions for all entries in symbolReferences.
	if (symbolReferences.size () > 0)
	{
	    for (final Symbol symbol : symbolReferences)
	    {
		final String typeDescriptor = Type.getType (symbol.getClass ()).getDescriptor ();
		fields.add (
		        new FieldNode (hiddenFieldAccess, javaName.createJavaSymbolName (symbol), typeDescriptor, null, null));
	    }
	    final MethodNode symbolMethod = getGetSymbolMethod ();
	    methods.add (symbolMethod);
	}
	for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	{
	    final String reference = entry.getKey ();
	    final Object quoted = entry.getValue ();
	    final String typeDescriptor = Type.getType (quoted.getClass ()).getDescriptor ();
	    fields.add (new FieldNode (hiddenFieldAccess, reference, typeDescriptor, null, null));
	}
	classLoader.setQuotedReferences (quotedReferences);
    }

    public String getClassSimpleName ()
    {
	return classSimpleName;
    }

    public Type getClassType ()
    {
	return classType;
    }

    public int getClassAccess ()
    {
	return access;
    }

    public Type getSuperclassType ()
    {
	return superclassType;
    }

    public List<String> getInterfaces ()
    {
	if (interfaces == null)
	{
	    interfaces = new ArrayList<String> ();
	}
	if (interfaces.size () != interfaceClasses.size ())
	{
	    interfaces.clear ();
	    for (int i = 0; i < interfaceClasses.size (); i++)
	    {
		final Class<?> ifClass = interfaceClasses.get (i);
		final Type ifType = Type.getType (ifClass);
		interfaces.add (ifType.getInternalName ());
	    }
	}
	return interfaces;
    }

    private void parse (final LispList[] members)
    {
	for (final LispList clause : members)
	{
	    final Symbol key = clause.head ();
	    if (key.is ("access"))
	    {
		parseAccessClause (clause);
	    }
	    else if (key.is ("extends"))
	    {
		parseExtendsClause (clause);
	    }
	    else if (key.is ("implements"))
	    {
		parseImplementsClause (clause);
	    }
	    else if (key.is ("field"))
	    {
		parseFieldClause (clause);
	    }
	    else if (key.is ("constructor"))
	    {
		final LispList arguments = clause.getSublist (1);
		final LispList body = clause.subList (2);
		parseConstructorClause (arguments, body);
	    }
	    else if (key.is ("method"))
	    {
		final Object nameSpec = clause.get (1);
		final Class<?> valueClass = NameSpec.getVariableClass (nameSpec);
		final Symbol methodName = NameSpec.getVariableName (nameSpec);
		final LispList arguments = clause.getSublist (2);
		final LispList body = clause.subList (3);
		parseMethodClause (valueClass, methodName, arguments, body);
	    }
	    else
	    {
		throw new Error ("Invalid %defclass clause " + key);
	    }
	}
    }

    // private void parse2 (final LispList[] members)
    // {
    // for (final LispList clause : members)
    // {
    // final Symbol key = clause.head ();
    // // if (key.is ("access"))
    // // {
    // // parseAccessClause (clause);
    // // }
    // // else if (key.is ("extends"))
    // // {
    // // parseExtendsClause (clause);
    // // }
    // // else if (key.is ("implements"))
    // // {
    // // parseImplementsClause (clause);
    // // }
    // // else if (key.is ("field"))
    // // {
    // // parseFieldClause (clause);
    // // }
    // // else
    // if (key.is ("constructor"))
    // {
    // final LispList arguments = clause.getSublist (1);
    // final LispList body = clause.subList (2);
    // parseConstructorClause (arguments, body);
    // }
    // // else if (key.is ("method"))
    // // {
    // // final Object nameSpec = clause.get (1);
    // // final Class<?> valueClass = NameSpec.getVariableClass (nameSpec);
    // // final Symbol methodName = NameSpec.getVariableName (nameSpec);
    // // final LispList arguments = clause.getSublist (2);
    // // final LispList body = clause.subList (3);
    // // parseMethodClause (valueClass, methodName, arguments, body);
    // // }
    // // else
    // // {
    // // throw new Error ("Invalid %defclass clause " + key);
    // // }
    // }
    // }

    private void parseAccessClause (final LispList clause)
    {
	for (int i = 1; i < clause.size (); i++)
	{
	    access += getAccess (CLASS_MODIFIERS, clause.get (i));
	}
    }

    private void parseExtendsClause (final LispList clause)
    {
	superclass = (Class<?>)clause.get (1);
	superclassType = Type.getType (superclass);
	superName = superclassType.getInternalName ();
    }

    private void parseImplementsClause (final LispList clause)
    {
	interfaceClasses.add ((Class<?>)clause.get (1));
    }

    private void parseFieldClause (final LispList clause)
    {
	final String fieldName = ((Symbol)clause.get (1)).getName ();
	LOGGER.info (new LogString ("Field: %s", fieldName));
	final FieldNode fn = new FieldNode (api, 0, null, null, null, null);
	fn.name = fieldName;
	for (int i = 2; i < clause.size (); i++)
	{
	    final Object m = clause.get (i);
	    if (m instanceof LispList)
	    {
		final LispList c = (LispList)m;
		final Symbol key = c.head ();
		if (key.is ("access"))
		{
		    for (int j = 1; j < c.size (); j++)
		    {
			fn.access += getAccess (FIELD_MODIFIERS, c.get (j));
		    }
		}
		else if (key.is ("type"))
		{
		    // Clause defines the field data type
		    final Class<?> dataType = (Class<?>)c.get (1);
		    final Type type = Type.getType (dataType);
		    fn.desc = type.getDescriptor ();
		}
		else if (key.is ("value"))
		{
		    // This is the initialization form
		    if (boxing.isBoxedClass (m.getClass ()))
		    {
			fn.value = m;
		    }
		    else if (m instanceof String)
		    {
			fn.value = m;
		    }
		    else
		    {
			// The init method must have code to compute and store this value
		    }
		}
	    }
	}
	fields.add (fn);
    }

    private void parseConstructorClause (final LispList arguments, final LispList body)
    {
	LOGGER.info (new LogString ("Constructor: %s", arguments));

	final MethodNode mn = new MethodNode ();
	mn.name = "<init>";
	mn.desc = getMethodSignature (Type.VOID_TYPE, getParameterTypes (arguments));
	mn.exceptions = new ArrayList<String> ();
	int headerCount = 0;
	for (int i = 0; i < body.size () && isMethodHeader (body.get (i)); i++)
	{
	    processMethodHeader (mn, CONSTRUCTOR_MODIFIERS, body.getSublist (i));
	    headerCount++;
	}
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	// TODO Need to support explicit calls to super or this constructors.
	// Currently just calls default Object constructor.
	il.add (new MethodInsnNode (Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false));
	// The AdviceAdapter will add hidden instructions here to initialize symbol and quoted
	// fields.

	final LispList bodyForms = body.subList (headerCount);
	// bodyForms is the rest of the code
	// Compile the rest of the method here
	final Map<Symbol, LocalBinding> locals = new LinkedHashMap<Symbol, LocalBinding> ();
	// Define 'this' as a local variable
	locals.put (THIS_SYMBOL, new LocalBinding (THIS_SYMBOL, superclass, 0));
	for (int i = 0; i < arguments.size (); i++)
	{
	    final Symbol arg = NameSpec.getVariableName (arguments.get (i));
	    final Class<?> argClass = NameSpec.getVariableClass (arguments.get (i));
	    locals.put (arg, new LocalBinding (arg, argClass, i + 1));
	}
	// Pass mn to the TreeCompilerContext so it can get at the method locals.
	final TreeCompilerContext context = new TreeCompilerContext (this, void.class, mn, locals);
	for (int i = 0; i < bodyForms.size (); i++)
	{
	    final Object expr = bodyForms.get (i);
	    final CompileResultSet resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	il.add (new InsnNode (Opcodes.RETURN));
	methods.add (mn);
	hasConstructor = true;
    }

    class InitModifierClassVisitor extends ClassVisitor
    {
	public InitModifierClassVisitor (final ClassVisitor classVisitor)
	{
	    super (Defclass.super.api, classVisitor);
	}

	@Override
	public MethodVisitor visitMethod (final int methodAccess, final String methodName, final String descriptor,
	        final String methodSignature, final String[] exceptions)
	{
	    return new AdviceAdapter (api, super.visitMethod (methodAccess, methodName, descriptor, methodSignature, exceptions),
	            methodAccess, methodName, descriptor)
	    {
		@Override
		protected void onMethodEnter ()
		{
		    if (methodName.equals ("<init>"))
		    {
			// mv.visitInsn (NOP);
			addHiddenConstructorSteps (mv);
			// mv.visitInsn (NOP);
		    }
		}

		@Override
		protected void onMethodExit (final int opcode)
		{
		    // if (methodName.equals ("<init>"))
		    // {
		    // mv.visitInsn (NOP);
		    // }
		}
	    };
	}
    }

    private void addHiddenConstructorSteps (final MethodVisitor mv)
    {
	final String classInternalName = getClassType ().getInternalName ();

	if (!symbolReferences.isEmpty ())
	{
	    final Type stringType = Type.getType (String.class);
	    final Type symbolType = Type.getType (Symbol.class);
	    final String symbolTypeDescriptor = symbolType.getDescriptor ();
	    // Create initialization code for all required symbols.
	    for (final Symbol symbol : symbolReferences)
	    {
		final String javaSymbolName = javaName.createJavaSymbolName (symbol);
		mv.visitVarInsn (ALOAD, 0);
		mv.visitVarInsn (ALOAD, 0);
		mv.visitLdcInsn (symbol.getPackage ().getName ());
		mv.visitLdcInsn (symbol.getName ());
		mv.visitMethodInsn (INVOKESPECIAL, classInternalName, "getSymbol",
		        Type.getMethodDescriptor (symbolType, stringType, stringType), false);
		mv.visitFieldInsn (PUTFIELD, classInternalName, javaSymbolName, symbolTypeDescriptor);

		LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaSymbolName, symbol));
	    }
	}
	if (!quotedReferences.isEmpty ())
	{
	    final Type classLoaderType = Type.getType (classLoader.getClass ());
	    final String classLoaderInternalName = classLoaderType.getInternalName ();
	    final String mapMethodDescriptor = Type.getMethodDescriptor (OBJECT_TYPE, OBJECT_TYPE);
	    // Create initialization code for all required quoted data.
	    for (final Entry<String, Object> entry : quotedReferences.entrySet ())
	    {
		// (define foo () (quote bar))
		mv.visitVarInsn (ALOAD, 0);
		mv.visitInsn (DUP);
		mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Object", "getClass", "()Ljava/lang/Class;", false);
		mv.visitMethodInsn (INVOKEVIRTUAL, "java/lang/Class", "getClassLoader", "()Ljava/lang/ClassLoader;", false);
		mv.visitTypeInsn (CHECKCAST, classLoaderInternalName);
		mv.visitMethodInsn (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences", "()Ljava/util/Map;", false);

		final String reference = entry.getKey ();
		final Object quoted = entry.getValue ();
		mv.visitLdcInsn (reference);
		mv.visitMethodInsn (INVOKEINTERFACE, "java/util/Map", "get", mapMethodDescriptor, true);
		final Type quotedType = Type.getType (quoted.getClass ());
		final String typeDescriptor = quotedType.getDescriptor ();
		mv.visitTypeInsn (CHECKCAST, quotedType.getInternalName ());
		mv.visitFieldInsn (PUTFIELD, classInternalName, reference, typeDescriptor);
	    }
	}
    }
    // @Override
    // public MethodVisitor visitMethod (final int access, final String name, final String
    // descriptor, final String signature,
    // final String[] exceptions)
    // {
    // return new AdviceAdapter (api, super.visitMethod (access, name, descriptor, signature,
    // exceptions), access, name,
    // descriptor)
    // {
    // @Override
    // protected void onMethodEnter ()
    // {
    // mv.visitInsn (NOP);
    // }
    //
    // @Override
    // protected void onMethodExit (final int opcode)
    // {
    // mv.visitInsn (NOP);
    // }
    // };
    // }

    // private void addHiddenConstructorSteps (final InsnList il)
    // {
    // final String classInternalName = getClassType ().getInternalName ();
    //
    // if (!symbolReferences.isEmpty ())
    // {
    // final Type stringType = Type.getType (String.class);
    // final Type symbolType = Type.getType (Symbol.class);
    // final String symbolTypeDescriptor = symbolType.getDescriptor ();
    // // Create initialization code for all required symbols.
    // for (final Symbol symbol : symbolReferences)
    // {
    // final String javaSymbolName = javaName.createJavaSymbolName (symbol);
    // il.add (new VarInsnNode (ALOAD, 0));
    // il.add (new VarInsnNode (ALOAD, 0));
    // il.add (new LdcInsnNode (symbol.getPackage ().getName ()));
    // il.add (new LdcInsnNode (symbol.getName ()));
    // il.add (new MethodInsnNode (INVOKESPECIAL, classInternalName, "getSymbol",
    // Type.getMethodDescriptor (symbolType, stringType, stringType), false));
    // il.add (new FieldInsnNode (PUTFIELD, classInternalName, javaSymbolName,
    // symbolTypeDescriptor));
    //
    // LOGGER.finer (new LogString ("Init: private Symbol %s %s;", javaSymbolName, symbol));
    // }
    // }
    // if (!quotedReferences.isEmpty ())
    // {
    // final Type classLoaderType = Type.getType (classLoader.getClass ());
    // final String classLoaderInternalName = classLoaderType.getInternalName ();
    // final String mapMethodDescriptor = Type.getMethodDescriptor (OBJECT_TYPE, OBJECT_TYPE);
    // // Create initialization code for all required quoted data.
    // for (final Entry<String, Object> entry : quotedReferences.entrySet ())
    // {
    // // (define foo () (quote bar))
    // il.add (new VarInsnNode (ALOAD, 0));
    // il.add (new InsnNode (DUP));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Object", "getClass",
    // "()Ljava/lang/Class;", false));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, "java/lang/Class", "getClassLoader",
    // "()Ljava/lang/ClassLoader;",
    // false));
    // il.add (new TypeInsnNode (CHECKCAST, classLoaderInternalName));
    // il.add (new MethodInsnNode (INVOKEVIRTUAL, classLoaderInternalName, "getQuotedReferences",
    // "()Ljava/util/Map;",
    // false));
    //
    // final String reference = entry.getKey ();
    // final Object quoted = entry.getValue ();
    // il.add (new LdcInsnNode (reference));
    // il.add (new MethodInsnNode (INVOKEINTERFACE, "java/util/Map", "get", mapMethodDescriptor,
    // true));
    // final Type quotedType = Type.getType (quoted.getClass ());
    // final String typeDescriptor = quotedType.getDescriptor ();
    // il.add (new TypeInsnNode (CHECKCAST, quotedType.getInternalName ()));
    // il.add (new FieldInsnNode (PUTFIELD, classInternalName, reference, typeDescriptor));
    // }
    // }
    // }

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

    private void parseMethodClause (final Class<?> valueClass, final Symbol methodName, final LispList arguments,
            final LispList body)
    {
	LOGGER.info (new LogString ("Method: %s %s", methodName, arguments));
	final MethodNode mn = new MethodNode ();
	final Type returnType = Type.getType (valueClass);
	mn.name = methodName.getName ();
	mn.desc = getMethodSignature (returnType, getParameterTypes (arguments));
	mn.exceptions = new ArrayList<String> ();
	int headerCount = 0;
	for (int i = 0; i < body.size () && isMethodHeader (body.get (i)); i++)
	{
	    processMethodHeader (mn, METHOD_MODIFIERS, body.getSublist (i));
	    headerCount++;
	}
	final LispList bodyForms = body.subList (headerCount);
	// bodyForms is the rest of the code
	final Map<Symbol, LocalBinding> locals = new LinkedHashMap<Symbol, LocalBinding> ();
	// Define 'this' as a local variable
	locals.put (THIS_SYMBOL, new LocalBinding (THIS_SYMBOL, superclass, 0));
	for (int i = 0; i < arguments.size (); i++)
	{
	    final Symbol arg = NameSpec.getVariableName (arguments.get (i));
	    final Class<?> argClass = NameSpec.getVariableClass (arguments.get (i));
	    locals.put (arg, new LocalBinding (arg, argClass, i + 1));
	}
	// Pass mn to the TreeCompilerContext so it can get at the method locals.
	final TreeCompilerContext context = new TreeCompilerContext (this, valueClass, mn, locals);
	for (int i = 0; i < bodyForms.size () - 1; i++)
	{
	    final Object expr = bodyForms.get (i);
	    final CompileResultSet resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	final CompileResultSet resultClass = context.compile (bodyForms.last (), true);
	// (define double:foo (int:n) 1 2 n)
	for (final CompileResult resultKind : resultClass.getResults ())
	{
	    // TODO This should collect crs of the same class and make them jump to the same place
	    // instead of duplicating code
	    context.add (resultKind.getLabels ());
	    if (resultKind instanceof ExplicitCompileResult)
	    {
		context.convert (((ExplicitCompileResult)resultKind).getResultClass (), valueClass, false, false);
	    }
	    else
	    {
		final ImplicitCompileResult icr = (ImplicitCompileResult)resultKind;
		context.add (icr);
	    }
	    context.add (new InsnNode (returnType.getOpcode (Opcodes.IRETURN)));
	}
	// Better not get here
	mn.maxStack = 0;
	mn.maxLocals = 0;
	methods.add (mn);
    }

    private int getAccess (final List<Object[]> modifiers, final Object a)
    {
	final String ms = ((Symbol)a).getName ();
	final Find<Object[]> finder = new Find<Object[]> ();
	final Object[] mod = finder.find (modifiers, new Predicate<Object[]> ()
	{
	    @Override
	    public boolean test (final Object[] t)
	    {
		return t[0].equals (ms);
	    }
	});
	if (mod != null)
	{
	    return (Integer)mod[1];
	}
	return 0;
    }

    private boolean isMethodHeader (final Object object)
    {
	if (object instanceof LispList)
	{
	    final LispList c = (LispList)object;
	    if (c.size () > 0)
	    {
		final Object h = c.get (0);
		if (h instanceof Symbol)
		{
		    final Symbol key = (Symbol)h;
		    return (key.is ("access") || key.is ("throws"));
		}
	    }
	}
	return false;
    }

    private void processMethodHeader (final MethodNode mn, final List<Object[]> modifiers, final LispList m)
    {
	final LispList c = m;
	final Symbol key = c.head ();
	if (key.is ("access"))
	{
	    for (int j = 1; j < c.size (); j++)
	    {
		mn.access += getAccess (modifiers, c.get (j));
	    }
	}
	else if (key.is ("throws"))
	{
	    // Clause defines the field data type
	    final Class<?> dataType = (Class<?>)c.get (1);
	    final Type type = Type.getType (dataType);
	    mn.exceptions.add (type.getInternalName ());
	}
    }

    private List<Symbol> getParameterNames (final LispList arguments)
    {
	final List<Symbol> result = new ArrayList<Symbol> ();
	for (final Object arg : arguments)
	{
	    final Symbol variable = NameSpec.getVariableName (arg);
	    result.add (variable);
	}
	return null;
    }

    private List<Type> getParameterTypes (final LispList arguments)
    {
	final List<Type> result = new ArrayList<Type> ();
	for (final Object arg : arguments)
	{
	    final Class<?> varClass = NameSpec.getVariableClass (arg);
	    final Type varType = Type.getType (varClass);
	    result.add (varType);
	}
	return result;
    }

    private String getMethodSignature (final Type returnType, final List<Type> argTypes)
    {
	final String returnTypeDescriptor = returnType.getDescriptor ();
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("(");
	for (int i = 0; i < argTypes.size (); i++)
	{
	    // buffer.append (CompileSupport.getNameTypeDescriptor (methodArgTypes.get (i)));
	    buffer.append (argTypes.get (i).getDescriptor ());
	}
	buffer.append (")");
	buffer.append (returnTypeDescriptor);
	return buffer.toString ();
    }

    /** Build empty default constructor */
    public void addDefaultInitMethod ()
    {
	final MethodNode mn = new MethodNode ();
	mn.access = Opcodes.ACC_PUBLIC;
	mn.name = "<init>";
	mn.desc = "()V";
	mn.exceptions = new ArrayList<String> ();
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	il.add (new MethodInsnNode (Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false));
	// addHiddenConstructorSteps (il);
	il.add (new InsnNode (Opcodes.RETURN));
	methods.add (mn);
	hasConstructor = true;
    }

    // /** Build empty default constructor */
    // public void addDefaultInitMethod ()
    // {
    // // @see https://dzone.com/articles/fully-dynamic-classes-with-asm
    // final MethodVisitor con = visitMethod (Opcodes.ACC_PUBLIC, // public method
    // "<init>", // method name
    // "()V", // descriptor
    // null, // signature (null means not generic)
    // null); // exceptions (array of strings)
    //
    // con.visitCode (); // Start the code for this method
    // con.visitVarInsn (Opcodes.ALOAD, 0); // Load "this" onto the stack
    // con.visitMethodInsn (Opcodes.INVOKESPECIAL, // Invoke an instance method (non-virtual)
    // "java/lang/Object", // Class on which the method is defined
    // "<init>", // Name of the method
    // "()V", // Descriptor
    // false); // Is this class an interface?
    // con.visitInsn (Opcodes.RETURN); // End the constructor method
    // con.visitMaxs (1, 1); // Specify max stack and local vars
    // hasConstructor = true;
    // }

    /** Build 'add' method */
    public void addSampleAdditionMethod ()
    {
	// @see https://dzone.com/articles/fully-dynamic-classes-with-asm
	final MethodVisitor mv = visitMethod (Opcodes.ACC_PUBLIC, // public method
	        "add", // name
	        "(II)I", // descriptor
	        null, // signature (null means not generic)
	        null); // exceptions (array of strings)
	mv.visitCode ();
	mv.visitVarInsn (Opcodes.ILOAD, 1); // Load int value onto stack
	mv.visitVarInsn (Opcodes.ILOAD, 2); // Load int value onto stack
	mv.visitInsn (Opcodes.IADD); // Integer add from stack and push to stack
	mv.visitInsn (Opcodes.IRETURN); // Return integer from top of stack
	mv.visitMaxs (2, 3); // Specify max stack and local vars
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

    private static Symbol QUOTE_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("quote");
    private final Map<String, Object> quotedReferences = new HashMap<String, Object> ();
    private final Set<Symbol> globalReferences = new HashSet<Symbol> ();
    private final List<Symbol> symbolReferences = new ArrayList<Symbol> ();

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
}