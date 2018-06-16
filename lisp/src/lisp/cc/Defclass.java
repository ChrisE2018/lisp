
package lisp.cc;

import java.lang.reflect.Constructor;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.logging.Logger;

import org.objectweb.asm.*;
import org.objectweb.asm.commons.AdviceAdapter;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.FieldInsnNode;

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
    private static ClassNamed classNamed = new ClassNamed ();
    private static SelectMethod selectMethod = new SelectMethod ();
    private static MethodSignature methSignature = new MethodSignature ();
    private static Symbol THIS_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("this");
    private static Symbol SUPER_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("super");
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
         {"final", Opcodes.ACC_FINAL},
         {"synchronized", Opcodes.ACC_SYNCHRONIZED},
         {"static", Opcodes.ACC_STATIC}};

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

    /**
     * If a constructor calls 'this' then the object initialization is not required. The signature
     * of such a constructor is added to this set so it can be skipped by the AdviceAdaptor.
     */
    private final Set<String> selfReferentialConstructors = new HashSet<String> ();

    private final Map<Symbol, FieldNode> fieldMap = new HashMap<Symbol, FieldNode> ();
    private final Map<Symbol, Class<?>> fieldClass = new HashMap<Symbol, Class<?>> ();
    private final Map<Symbol, Object> fieldValues = new HashMap<Symbol, Object> ();

    /** For each constructor this contains the classes of the parameters. */
    private final List<Class<?>[]> constructorParameters = new ArrayList<Class<?>[]> ();

    public Defclass (final int api, final LispClassLoader classLoader, final String name, final LispList[] members)
    {
	super (api);
	this.classLoader = classLoader;
	classSimpleName = name;
	classType = Type.getType ("Llisp/cc/" + classSimpleName + ";");
	parse (members);
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
		final String javaSymbolName = javaName.createJavaSymbolName (symbol);
		fields.add (new FieldNode (hiddenFieldAccess, javaSymbolName, typeDescriptor, null, null));
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
	LOGGER.info ("defclass " + name);
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
	// Process everything except constructor and method members.
	// Methods generate code that depends on knowing the fields.
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
		// Extract information about constructors so constructor chains can be resolved.
		final LispList arguments = clause.getSublist (1);
		final Class<?>[] params = getParameterClasses (arguments);
		constructorParameters.add (params);
	    }
	    else if (key.is ("method"))
	    {
	    }
	    else
	    {
		throw new Error ("Invalid %defclass clause " + key);
	    }
	}
	// Process constructor and method members.
	for (final LispList clause : members)
	{
	    final Symbol key = clause.head ();
	    if (key.is ("constructor"))
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
	}
    }

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
	final Symbol fName = (Symbol)clause.get (1);
	final String fieldName = fName.getName ();
	LOGGER.fine (new LogString ("Field: %s", fName));
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
		    final Class<?> dataType = classNamed.getClass (c.get (1));
		    final Type type = Type.getType (dataType);
		    fn.desc = type.getDescriptor ();
		    fieldClass.put (fName, dataType);
		}
		else if (key.is ("value"))
		{
		    final Object value = c.get (1);
		    // This is the initialization form
		    // FIXME The field is not initialialized
		    if (boxing.isBoxedClass (value.getClass ()))
		    {
			fn.value = value;
		    }
		    else if (value instanceof String)
		    {
			fn.value = value;
		    }
		    else
		    {
			// The init method must have code to compute and store this value
		    }
		    fieldValues.put (fName, value);
		}
	    }
	}
	fields.add (fn);
	fieldMap.put (fName, fn);
	for (int i = 2; i < clause.size (); i++)
	{
	    final Object m = clause.get (i);
	    if (m instanceof LispList)
	    {
		final LispList c = (LispList)m;
		final Symbol key = c.head ();
		if (key.is ("getter"))
		{
		    if (c.size () == 0)
		    {
			addGetterMethod (fName);
		    }
		    else
		    {
			for (int j = 1; j < c.size (); j++)
			{
			    final Symbol n = (Symbol)c.get (j);
			    addGetterMethod (fName, Opcodes.ACC_PUBLIC, n.getName ());
			}
		    }
		}
		else if (key.is ("setter"))
		{
		    if (c.size () == 0)
		    {
			addSetterMethod (fName);
		    }
		    else
		    {
			for (int j = 1; j < c.size (); j++)
			{
			    final Symbol n = (Symbol)c.get (j);
			    addSetterMethod (fName, Opcodes.ACC_PUBLIC, n.getName ());
			}
		    }
		}
	    }
	}
    }

    private void parseConstructorClause (final LispList arguments, final LispList body)
    {
	LOGGER.fine (new LogString ("Constructor: %s", arguments));

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
	final LispList restForms = body.subList (headerCount);
	boolean referencesThis = false;
	if (restForms.size () > 0)
	{
	    final Object firstStatement = restForms.get (0);
	    if (firstStatement instanceof List)
	    {
		if (((List<?>)firstStatement).get (0) == THIS_SYMBOL)
		{
		    referencesThis = true;
		    selfReferentialConstructors.add (mn.desc);
		}
	    }
	}
	final Map<Symbol, LexicalBinding> locals = new LinkedHashMap<Symbol, LexicalBinding> ();
	for (int i = 0; i < arguments.size (); i++)
	{
	    final Symbol arg = NameSpec.getVariableName (arguments.get (i));
	    final Class<?> argClass = NameSpec.getVariableClass (arguments.get (i));
	    locals.put (arg, new LexicalVariable (arg, argClass, i + 1));
	}
	final LispList bodyForms = addConstructorChain (mn, locals, body.subList (headerCount));

	// bodyForms is the rest of the code

	// The AdviceAdapter will add hidden instructions here to initialize symbol and quoted
	// fields.

	// Compile the rest of the method here
	final InsnList il = mn.instructions;
	final TreeCompilerContext context = new TreeCompilerContext (this, void.class, mn, locals);
	if (!referencesThis)
	{
	    // Determine if this constructor calls 'this' and not do this part.
	    for (final Entry<Symbol, Object> entry : fieldValues.entrySet ())
	    {
		final Symbol fName = entry.getKey ();
		final Object value = entry.getValue ();
		final Class<?> fClass = fieldClass.get (fName);
		final LexicalField lf = new LexicalField (fName, fClass, classType);
		final CompileResultSet cr = context.compile (value, true);
		context.convert (cr, fClass, false, false);
		lf.store (context);
	    }
	}
	// Create local references for all class fields
	for (final Entry<Symbol, FieldNode> entry : fieldMap.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    // final FieldNode field = entry.getValue ();
	    final Class<?> fClass = fieldClass.get (fName);
	    locals.put (fName, new LexicalField (fName, fClass, classType));
	}
	// Define 'this' as a local variable
	locals.put (THIS_SYMBOL, new LexicalVariable (THIS_SYMBOL, superclass, 0));
	// Pass mn to the TreeCompilerContext so it can get at the method locals.
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

    private LispList addConstructorChain (final MethodNode mn, final Map<Symbol, LexicalBinding> locals, final LispList bodyForms)
    {
	final InsnList il = mn.instructions;
	if (bodyForms.size () > 0)
	{
	    final Object firstForm = bodyForms.get (0);
	    // TODO Need to support explicit calls to super or this constructors.
	    if (firstForm instanceof LispList)
	    {
		final LispList first = (LispList)firstForm;
		final Symbol h = first.head ();
		if (h == THIS_SYMBOL)
		{
		    // Compile call to another constructor
		    il.add (new VarInsnNode (Opcodes.ALOAD, 0));

		    final List<Class<?>> arguments = new ArrayList<Class<?>> ();
		    for (int i = 1; i < first.size (); i++)
		    {
			arguments.add (selectMethod.predictResultClass (locals, first.get (i)));
		    }
		    final Class<?>[] constructor = selectMethod.selectConstructor (constructorParameters, arguments);
		    final TreeCompilerContext context = new TreeCompilerContext (this, void.class, mn, locals);
		    for (int i = 1; i < first.size (); i++)
		    {
			final CompileResultSet cr = context.compile (first.get (i), true);
			context.convert (cr, constructor[i - 1], false, false);
		    }
		    final String cs = methSignature.getArgumentSignature (constructor) + "V";
		    il.add (new MethodInsnNode (Opcodes.INVOKESPECIAL, classType.getInternalName (), "<init>", cs, false));
		    return bodyForms.subList (1);
		}
		else if (h == SUPER_SYMBOL)
		{
		    // Compile call to superclass constructor
		    il.add (new VarInsnNode (Opcodes.ALOAD, 0));
		    final List<Class<?>> arguments = new ArrayList<Class<?>> ();
		    for (int i = 1; i < first.size (); i++)
		    {
			arguments.add (selectMethod.predictResultClass (locals, first.get (i)));
		    }
		    final Constructor<?> constructor = selectMethod.selectConstructor (superclass, arguments);
		    final Class<?>[] params = constructor.getParameterTypes ();
		    final TreeCompilerContext context = new TreeCompilerContext (this, void.class, mn, locals);
		    for (int i = 1; i < first.size (); i++)
		    {
			final CompileResultSet cr = context.compile (first.get (i), true);
			context.convert (cr, params[i - 1], false, false);
		    }
		    final String cs = methSignature.getArgumentSignature (constructor);
		    il.add (new MethodInsnNode (Opcodes.INVOKESPECIAL, superName, "<init>", cs, false));
		    return bodyForms.subList (1);
		}
	    }
	}
	// Currently just calls default Object constructor.
	il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	il.add (new MethodInsnNode (Opcodes.INVOKESPECIAL, superName, "<init>", "()V", false));
	return bodyForms;
    }

    private List<List<Class<?>>> getConstructorSigs ()
    {
	final List<List<Class<?>>> result = new ArrayList<List<Class<?>>> ();

	return result;
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
			if (!selfReferentialConstructors.contains (descriptor))
			{
			    // mv.visitInsn (NOP);
			    addHiddenConstructorSteps (mv);
			    // mv.visitInsn (NOP);
			}
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
	LOGGER.fine (new LogString ("Method: %s %s", methodName, arguments));
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
	final Map<Symbol, LexicalBinding> locals = new LinkedHashMap<Symbol, LexicalBinding> ();
	// Define 'this' as a local variable
	locals.put (THIS_SYMBOL, new LexicalVariable (THIS_SYMBOL, superclass, 0));
	for (int i = 0; i < arguments.size (); i++)
	{
	    final Symbol arg = NameSpec.getVariableName (arguments.get (i));
	    final Class<?> argClass = NameSpec.getVariableClass (arguments.get (i));
	    locals.put (arg, new LexicalVariable (arg, argClass, i + 1));
	}
	for (final Entry<Symbol, FieldNode> entry : fieldMap.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    // final FieldNode field = entry.getValue ();
	    final Class<?> fClass = fieldClass.get (fName);
	    locals.put (fName, new LexicalField (fName, fClass, classType));
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

    private Class<?>[] getParameterClasses (final LispList arguments)
    {
	final Class<?>[] result = new Class<?>[arguments.size ()];
	for (int i = 0; i < arguments.size (); i++)
	{
	    result[i] = NameSpec.getVariableClass (arguments.get (i));
	}
	return result;
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

    private void addGetterMethod (final Symbol field)
    {
	final String root = field.getName ();
	final char ch = root.charAt (0);
	final String methodName = "get" + Character.toUpperCase (ch) + root.substring (1);
	addGetterMethod (field, Opcodes.ACC_PUBLIC, methodName);
    }

    private void addGetterMethod (final Symbol field, final int getterAccess, final String methodName)
    {
	final MethodNode mn = new MethodNode ();
	final FieldNode fn = fieldMap.get (field);
	final Class<?> cls = fieldClass.get (field);
	final Type type = Type.getType (cls);
	mn.access = getterAccess;
	mn.name = methodName;
	mn.desc = "()" + type.getDescriptor ();
	mn.exceptions = new ArrayList<String> ();
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	il.add (new FieldInsnNode (Opcodes.GETFIELD, classType.getInternalName (), fn.name, fn.desc));
	il.add (new InsnNode (type.getOpcode (Opcodes.IRETURN)));
	methods.add (mn);
    }

    private void addSetterMethod (final Symbol field)
    {
	final String root = field.getName ();
	final char ch = root.charAt (0);
	final String methodName = "set" + Character.toUpperCase (ch) + root.substring (1);
	addSetterMethod (field, Opcodes.ACC_PUBLIC, methodName);
    }

    private void addSetterMethod (final Symbol field, final int setterAccess, final String methodName)
    {
	final MethodNode mn = new MethodNode ();
	final FieldNode fn = fieldMap.get (field);
	final Class<?> cls = fieldClass.get (field);
	final Type type = Type.getType (cls);
	mn.access = setterAccess;
	mn.name = methodName;
	mn.desc = "(" + type.getDescriptor () + ")V";
	mn.exceptions = new ArrayList<String> ();
	final InsnList il = mn.instructions;
	il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	il.add (new VarInsnNode (type.getOpcode (Opcodes.ILOAD), 1));
	il.add (new FieldInsnNode (Opcodes.PUTFIELD, classType.getInternalName (), fn.name, fn.desc));
	il.add (new InsnNode (Opcodes.RETURN));
	methods.add (mn);
	hasConstructor = true;
    }

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
