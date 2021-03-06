
package lisp.cc;

import java.io.StringWriter;
import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.logging.*;

import org.objectweb.asm.*;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.AdviceAdapter;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.FieldInsnNode;

import lisp.asm.PrintBytecodeClassAdaptor;
import lisp.asm.instructions.InsnNode;
import lisp.asm.instructions.LabelNode;
import lisp.asm.instructions.MethodInsnNode;
import lisp.asm.instructions.VarInsnNode;
import lisp.cc4.*;
import lisp.lang.*;
import lisp.lang.Package;
import lisp.lang.Symbol;
import lisp.util.*;

public class Defclass extends ClassNode implements TreeCompilerInterface, Opcodes
{
    private static final Logger LOGGER = Logger.getLogger (Defclass.class.getName ());

    private static int asmApi = Opcodes.ASM5;
    private static int bytecodeVersion = Opcodes.V1_5;

    private static ClassNamed classNamed = new ClassNamed ();
    private static SelectMethod selectMethod = new SelectMethod ();
    private static MethodSignature methSignature = new MethodSignature ();
    private static Symbol THIS_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("this");
    private static Symbol SUPER_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("super");
    private static Symbol ACCESS_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("access");
    private static Symbol PUBLIC_SYMBOL = PackageFactory.getSystemPackage ().internSymbol ("public");

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
         {"final", Opcodes.ACC_FINAL},
         {"static", Opcodes.ACC_STATIC}};

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

    private static Map<String, Class<?>> classForName = new HashMap<> ();

    public static Class<?> forName (final String name)
    {
	return classForName.get (name);
    }

    protected static void forName (final String name, final Class<?> cls)
    {
	classForName.put (name, cls);
    }

    public static Collection<Class<?>> getClasses ()
    {
	return classForName.values ();
    }

    private final String classSimpleName;
    private final Type classType;
    private Class<?> superclass = Object.class;
    private Type superclassType = Type.getType (Object.class);
    private final List<Class<?>> interfaceClasses = new ArrayList<> ();
    private boolean hasConstructor = false;

    /**
     * If a constructor calls 'this' then the object initialization is not required. The signature
     * of such a constructor is added to this set so it can be skipped by the AdviceAdaptor.
     */
    private final Set<String> selfReferentialConstructors = new HashSet<> ();

    private final Map<Symbol, FieldNode> fieldMap = new HashMap<> ();
    private final Map<Symbol, Class<?>> fieldClass = new HashMap<> ();
    private final Map<Symbol, Object> fieldValues = new HashMap<> ();

    /** For each constructor this contains the classes of the parameters. */
    private final List<Class<?>[]> constructorParameters = new ArrayList<> ();

    /**
     * References to global references. This is mainly used to issue information messages about
     * globals once instead of repeatedly.
     */
    private final Set<Symbol> globalReferences = new HashSet<> ();

    /** Delegate object to handle quoted data. */
    private final QuotedData quotedData;

    /** Collect all static evaluation forms here. */
    private final LispList staticForms = new LispList ();

    public Defclass (final QuotedData quotedData, final String name, final LispList[] members) throws ClassNotFoundException
    {
	super (asmApi);
	this.quotedData = quotedData;
	classSimpleName = name;
	final Package currentPackage = PackageFactory.getCurrentPackage ();
	final String pkgName = currentPackage.getName ();
	classType = Type.getType ("L" + pkgName.replace ('.', '/') + '/' + classSimpleName + ";");
	parse (members);
	if (!hasConstructor)
	{
	    final LispList arguments = new LispList ();
	    final LispList body = new LispList ();
	    final LispList accessClause = new LispList ();
	    accessClause.add (ACCESS_SYMBOL);
	    accessClause.add (PUBLIC_SYMBOL);
	    body.add (accessClause);
	    parseConstructorClause (arguments, body);
	}
	quotedData.addRequiredFields (this);
	LOGGER.info ("defclass " + name);
    }

    @Override
    public Type getClassType ()
    {
	return classType;
    }

    public byte[] getBytecode ()
    {
	// The class access, class name, superclass and interfaces need to be determined before
	// we can visit the class node
	final String superClassInternalName = superclassType.getInternalName ();
	LOGGER.info (new LogString ("Defclass %s extends %s", classSimpleName, superclassType.getClassName ()));
	final String classInternalName = classType.getInternalName ();
	// final String classBinaryName = classType.getClassName (); // Uses dots
	final String classBinaryName = null; // Must be null or TraceClassVisitor fails
	final ClassWriter cw = new ClassWriter (ClassWriter.COMPUTE_FRAMES);
	final List<String> interfaceList = getInterfaces ();
	final String[] interfaceNames = new String[interfaceList.size ()];
	interfaceList.toArray (interfaceNames);
	final ClassNode cn = this;
	cn.visit (bytecodeVersion, access, classInternalName, classBinaryName, superClassInternalName, interfaceNames);
	cn.visitEnd ();

	// optimizers here
	final Logger pbl = Logger.getLogger (PrintBytecodeClassAdaptor.class.getName ());
	final boolean optimize = Symbol.named ("lisp.lang", "optimize").getBooleanValue (true);
	final boolean printBytecode = pbl.isLoggable (Level.INFO);

	// Form chain adding things in the middle.
	// The last thing to do is write the code using cw.
	ClassVisitor classVisitor = cw;

	if (printBytecode)
	{
	    // Install PrintBytecodeClassAdaptor before Optimizer so it runs after.
	    // Move this block lower to print code before optimization.
	    classVisitor = new PrintBytecodeClassAdaptor (asmApi, classVisitor, new StringWriter ());
	}
	if (optimize)
	{
	    classVisitor = new Optimizer (Compiler.ASM_VERSION, classVisitor);
	}

	// Put code into init method to initialize fields and quoted data
	// This should be the last operation installed (first done)
	classVisitor = new InitModifierClassVisitor (classVisitor);
	cn.accept (classVisitor);

	return cw.toByteArray ();
    }

    private void parse (final LispList[] members) throws ClassNotFoundException
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
	    else if (key.is ("annotation"))
	    {
		parseClassAnnotationClause (clause);
	    }
	    else if (key.is ("field"))
	    {
		parseFieldClause (clause);
	    }
	    else if (key.is ("static"))
	    {
		parseStaticClause (clause);
	    }
	    else if (key.is ("constructor"))
	    {
		// Extract information about constructors so constructor chains can be resolved.
		final LispList arguments = (LispList)clause.get (1);
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
		final LispList arguments = (LispList)clause.get (1);
		final LispList body = clause.subList (2);
		parseConstructorClause (arguments, body);
	    }
	    else if (key.is ("method"))
	    {
		final Object nameSpec = clause.get (1);
		final Class<?> valueClass = NameSpec.getVariableClass (nameSpec);
		final Symbol methodName = NameSpec.getVariableName (nameSpec);
		final LispList arguments = (LispList)clause.get (2);
		final LispList body = clause.subList (3);
		parseMethodClause (valueClass, methodName, arguments, body);
	    }
	}
	createStaticInit (staticForms);
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

    private List<String> getInterfaces ()
    {
	if (interfaces == null)
	{
	    interfaces = new ArrayList<> ();
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

    private void parseImplementsClause (final LispList clause)
    {
	interfaceClasses.add ((Class<?>)clause.get (1));
    }

    /**
     * @param clause
     * @throws ClassNotFoundException
     * @See AnnotationNode source for information about the values field format.
     */
    private void parseClassAnnotationClause (final LispList clause) throws ClassNotFoundException
    {
	LOGGER.info (new LogString ("Annotation: %s", clause));
	final Object annotationKind = clause.get (1);
	Class<?> annotationClass;
	if (annotationKind instanceof Class<?>)
	{
	    annotationClass = (Class<?>)annotationKind;
	}
	else if (annotationKind instanceof String)
	{
	    annotationClass = Class.forName ((String)annotationKind);
	}
	else
	{
	    throw new IllegalArgumentException (annotationKind + " is not an annotation");
	}
	final Type annotationType = Type.getType (annotationClass);
	final AnnotationNode annotation = new AnnotationNode (api, annotationType.getDescriptor ());
	for (int i = 0; i < clause.size (); i++)
	{
	    final String key = (String)clause.get (i - 1);
	    final Object value = clause.get (i);
	    annotation.values.add (key);
	    annotation.values.add (value);
	    verifyAnnotationMethod (annotationClass, key);
	}
	visibleAnnotations.add (annotation);
    }

    private void verifyAnnotationMethod (final Class<?> annotationClass, final String methodName)
    {
	final Method[] annotationMethods = annotationClass.getDeclaredMethods ();
	for (final Method method : annotationMethods)
	{
	    if (method.getName ().equals (methodName))
	    {
		return;
	    }
	}
	throw new IllegalArgumentException (
	        methodName + " is not a valid field of annotation " + annotationClass.getSimpleName ());
    }

    private void parseFieldClause (final LispList clause) throws ClassNotFoundException
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
		    fieldValues.put (fName, value);
		}
		else if (key.is ("annotation"))
		{
		    parseFieldAnnotationClause (fn, c);
		}
	    }
	}
	fields.add (fn);
	fieldMap.put (fName, fn);
	final boolean isStatic = (fn.access & ACC_STATIC) != 0;
	final int accessorAccess = ACC_PUBLIC + (isStatic ? ACC_STATIC : 0);
	for (int i = 2; i < clause.size (); i++)
	{
	    final Object m = clause.get (i);
	    if (m instanceof LispList)
	    {
		final LispList c = (LispList)m;
		final Symbol key = c.head ();
		if (key.is ("getter"))
		{
		    // Static accessors should go through the class not the instance.
		    // Static fields should be initialized in a static block
		    if (c.size () == 0)
		    {
			addGetterMethod (fName, accessorAccess, getGetterName (fName));
		    }
		    else
		    {
			for (int j = 1; j < c.size (); j++)
			{
			    final Symbol n = (Symbol)c.get (j);
			    addGetterMethod (fName, accessorAccess, n.getName ());
			}
		    }
		}
		else if (key.is ("setter"))
		{
		    if (c.size () == 0)
		    {
			addSetterMethod (fName, accessorAccess, getSetterName (fName));
		    }
		    else
		    {
			for (int j = 1; j < c.size (); j++)
			{
			    final Symbol n = (Symbol)c.get (j);
			    addSetterMethod (fName, accessorAccess, n.getName ());
			}
		    }
		}
	    }
	}
    }

    /**
     * @param clause
     * @throws ClassNotFoundException
     * @See AnnotationNode source for information about the values field format.
     */
    private void parseFieldAnnotationClause (final FieldNode fn, final LispList clause) throws ClassNotFoundException
    {
	LOGGER.info (new LogString ("Field %s Annotation: %s", fn.name, clause));
	final Object annotationKind = clause.get (1);
	Class<?> annotationClass;
	if (annotationKind instanceof Class<?>)
	{
	    annotationClass = (Class<?>)annotationKind;
	}
	else if (annotationKind instanceof String)
	{
	    annotationClass = Class.forName ((String)annotationKind);
	}
	else
	{
	    throw new IllegalArgumentException (annotationKind + " is not an annotation");
	}
	final Type annotationType = Type.getType (annotationClass);
	final AnnotationNode annotation = new AnnotationNode (api, annotationType.getDescriptor ());
	for (int i = 0; i < clause.size (); i++)
	{
	    final String key = (String)clause.get (i - 1);
	    final Object value = clause.get (i);
	    annotation.values.add (key);
	    annotation.values.add (value);
	    verifyAnnotationMethod (annotationClass, key);
	}
	fn.visibleAnnotations.add (annotation);
    }

    private void parseStaticClause (final LispList clause)
    {
	for (int i = 1; i < clause.size (); i++)
	{
	    staticForms.add (clause.get (i));
	}
    }

    private void createStaticInit (final LispList body)
    {
	if (!hasStaticFields ())
	{
	    if (body.isEmpty ())
	    {
		LOGGER.fine (new LogString ("Static initializer not needed"));
		return;
	    }
	}
	LOGGER.fine (new LogString ("Static: %s", body));

	final MethodNode mn = new MethodNode ();
	mn.name = "<clinit>";
	mn.desc = getMethodSignature (Type.VOID_TYPE, new ArrayList<Type> ());
	mn.exceptions = new ArrayList<> ();
	mn.access = ACC_STATIC;
	final Map<Symbol, LexicalBinding> locals = new LinkedHashMap<> ();

	// Compile the body forms here
	final LabelNode l1 = new LabelNode ();
	final Symbol blockName = null;
	final BlockBinding bb = new BlockBinding (blockName, void.class, l1);
	final List<BlockBinding> blocks = new ArrayList<> ();
	blocks.add (bb);
	final TreeCompilerContext context = new TreeCompilerContext (this, quotedData, void.class, mn, locals, blocks);

	for (final Entry<Symbol, Object> entry : fieldValues.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    final FieldNode fieldNode = fieldMap.get (fName);
	    final boolean isStatic = ((fieldNode.access & ACC_STATIC) != 0);
	    if (isStatic)
	    {
		final Object value = entry.getValue ();
		final Class<?> fClass = fieldClass.get (fName);
		final LexicalField lf = new LexicalField (fName, true, fClass, classType);
		final CompileResults cr = context.compile (value, true);
		context.convert (cr, fClass, false, false);
		lf.store (context);
	    }
	}
	// Create local references for all class fields
	for (final Entry<Symbol, FieldNode> entry : fieldMap.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    final FieldNode fieldNode = entry.getValue ();
	    final boolean isStatic = ((fieldNode.access & ACC_STATIC) != 0);
	    if (isStatic)
	    {
		final Class<?> fClass = fieldClass.get (fName);
		locals.put (fName, new LexicalField (fName, isStatic, fClass, classType));
	    }
	}

	// Pass mn to the TreeCompilerContext so it can get at the method locals.
	for (int i = 0; i < body.size (); i++)
	{
	    final Object expr = body.get (i);
	    final CompileResults resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	context.add (l1);
	context.add (new InsnNode (Opcodes.RETURN));
	methods.add (mn);
    }

    /** Determine if any static fields have been declared. */
    private boolean hasStaticFields ()
    {
	for (final Entry<Symbol, Object> entry : fieldValues.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    final FieldNode fieldNode = fieldMap.get (fName);
	    final boolean isStatic = ((fieldNode.access & ACC_STATIC) != 0);
	    if (isStatic)
	    {
		return true;
	    }
	}
	return false;
    }

    private void parseConstructorClause (final LispList arguments, final LispList body) throws ClassNotFoundException
    {
	LOGGER.fine (new LogString ("Constructor: %s", arguments));

	final MethodNode mn = new MethodNode ();
	mn.name = "<init>";
	mn.desc = getMethodSignature (Type.VOID_TYPE, getParameterTypes (arguments));
	mn.exceptions = new ArrayList<> ();
	int headerCount = 0;
	for (int i = 0; i < body.size () && isMethodHeader (body.get (i)); i++)
	{
	    processMethodHeader (mn, CONSTRUCTOR_MODIFIERS, (LispList)body.get (i));
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
	final Map<Symbol, LexicalBinding> locals = new LinkedHashMap<> ();
	int localRef = 1;
	for (int i = 0; i < arguments.size (); i++)
	{
	    final Symbol arg = NameSpec.getVariableName (arguments.get (i));
	    final Class<?> argClass = NameSpec.getVariableClass (arguments.get (i));
	    locals.put (arg, new LexicalVariable (arg, argClass, localRef));
	    final Type type = Type.getType (argClass);
	    localRef += type.getSize ();
	}
	final LispList bodyForms = addConstructorChain (mn, locals, body.subList (headerCount));

	// bodyForms is the rest of the code

	// The AdviceAdapter will add hidden instructions here to initialize symbol and quoted
	// fields.

	// Compile the rest of the method here
	final LabelNode l1 = new LabelNode ();
	final Symbol blockName = null;
	final BlockBinding bb = new BlockBinding (blockName, void.class, l1);
	final List<BlockBinding> blocks = new ArrayList<> ();
	blocks.add (bb);
	final TreeCompilerContext context = new TreeCompilerContext (this, quotedData, void.class, mn, locals, blocks);
	if (!referencesThis)
	{
	    // Determine if this constructor calls 'this' and not do this part.
	    for (final Entry<Symbol, Object> entry : fieldValues.entrySet ())
	    {
		final Symbol fName = entry.getKey ();
		final FieldNode fieldNode = fieldMap.get (fName);
		final boolean isStatic = ((fieldNode.access & ACC_STATIC) != 0);
		if (!isStatic)
		{
		    final Object value = entry.getValue ();
		    final Class<?> fClass = fieldClass.get (fName);
		    final LexicalField lf = new LexicalField (fName, isStatic, fClass, classType);
		    final CompileResults cr = context.compile (value, true);
		    context.convert (cr, fClass, false, false);
		    lf.store (context);
		}
	    }
	}
	// Create local references for all class fields
	for (final Entry<Symbol, FieldNode> entry : fieldMap.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    final FieldNode fieldNode = entry.getValue ();
	    final boolean isStatic = ((fieldNode.access & ACC_STATIC) != 0);
	    final Class<?> fClass = fieldClass.get (fName);
	    locals.put (fName, new LexicalField (fName, isStatic, fClass, classType));
	}
	// Define 'this' as a local variable
	locals.put (THIS_SYMBOL, new LexicalVariable (THIS_SYMBOL, superclass, 0));
	// Pass mn to the TreeCompilerContext so it can get at the method locals.
	for (int i = 0; i < bodyForms.size (); i++)
	{
	    final Object expr = bodyForms.get (i);
	    final CompileResults resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	context.add (l1);
	context.add (new InsnNode (Opcodes.RETURN));
	methods.add (mn);
	hasConstructor = true;
    }

    private LispList addConstructorChain (final MethodNode mn, final Map<Symbol, LexicalBinding> locals, final LispList bodyForms)
    {
	final InsnList il = mn.instructions;
	if (bodyForms.size () > 0)
	{
	    final Object firstForm = bodyForms.get (0);
	    // Support explicit calls to super or this constructors.
	    if (firstForm instanceof LispList)
	    {
		final LispList first = (LispList)firstForm;
		final Object h = first.get (0);
		if (h == THIS_SYMBOL)
		{
		    // Compile call to another constructor
		    il.add (new VarInsnNode (Opcodes.ALOAD, 0));

		    final List<Class<?>> arguments = new ArrayList<> ();
		    for (int i = 1; i < first.size (); i++)
		    {
			arguments.add (selectMethod.predictResultClass (locals, first.get (i)));
		    }
		    final Class<?>[] constructor = selectMethod.selectConstructor (constructorParameters, arguments);
		    final List<BlockBinding> bbs = new ArrayList<> ();
		    final TreeCompilerContext context = new TreeCompilerContext (this, quotedData, void.class, mn, locals, bbs);
		    for (int i = 1; i < first.size (); i++)
		    {
			final CompileResults cr = context.compile (first.get (i), true);
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
		    final List<Class<?>> arguments = new ArrayList<> ();
		    for (int i = 1; i < first.size (); i++)
		    {
			arguments.add (selectMethod.predictResultClass (locals, first.get (i)));
		    }
		    final Constructor<?> constructor = selectMethod.selectConstructor (superclass, arguments);
		    final Class<?>[] params = constructor.getParameterTypes ();
		    final List<BlockBinding> bbs = new ArrayList<> ();
		    final TreeCompilerContext context = new TreeCompilerContext (this, quotedData, void.class, mn, locals, bbs);
		    for (int i = 1; i < first.size (); i++)
		    {
			final CompileResults cr = context.compile (first.get (i), true);
			context.convert (cr, params[i - 1], false, false);
		    }
		    final String cs = methSignature.getMethodSignature (constructor);
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
			    // Quoted data needs to be saved to static fields so it
			    // can be seen by static methods.
			    // mv.visitInsn (NOP);
			    quotedData.addHiddenConstructorSteps (classType, mv);
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

    private void parseMethodClause (final Class<?> valueClass, final Symbol methodName, final LispList arguments,
            final LispList body) throws ClassNotFoundException
    {
	LOGGER.fine (new LogString ("Method: %s %s", methodName, arguments));
	final MethodNode mn = new MethodNode ();
	final Type returnType = Type.getType (valueClass);
	mn.name = methodName.getName ();
	mn.desc = getMethodSignature (returnType, getParameterTypes (arguments));
	mn.exceptions = new ArrayList<> ();
	int headerCount = 0;
	for (int i = 0; i < body.size () && isMethodHeader (body.get (i)); i++)
	{
	    processMethodHeader (mn, METHOD_MODIFIERS, (LispList)body.get (i));
	    headerCount++;
	}
	final boolean isStatic = (mn.access & ACC_STATIC) != 0;
	final LispList bodyForms = body.subList (headerCount);
	// bodyForms is the rest of the code
	final Map<Symbol, LexicalBinding> locals = new LinkedHashMap<> ();
	// Define 'this' as a local variable
	locals.put (THIS_SYMBOL, new LexicalVariable (THIS_SYMBOL, superclass, 0));
	int localRef = isStatic ? 0 : 1;
	for (int i = 0; i < arguments.size (); i++)
	{
	    final Symbol arg = NameSpec.getVariableName (arguments.get (i));
	    final Class<?> argClass = NameSpec.getVariableClass (arguments.get (i));
	    locals.put (arg, new LexicalVariable (arg, argClass, localRef));
	    final Type type = Type.getType (argClass);
	    localRef += type.getSize ();
	}
	// Bind references to class variables here (not if static!)
	for (final Entry<Symbol, FieldNode> entry : fieldMap.entrySet ())
	{
	    final Symbol fName = entry.getKey ();
	    final FieldNode fieldNode = entry.getValue ();
	    // If the method is static, only include references to static fields.
	    final boolean staticField = (fieldNode.access & ACC_STATIC) != 0;
	    if (!isStatic || staticField)
	    {
		final Class<?> fClass = fieldClass.get (fName);
		locals.put (fName, new LexicalField (fName, staticField, fClass, classType));
	    }
	}
	// Pass mn to the TreeCompilerContext so it can get at the method locals.
	final LabelNode l1 = new LabelNode ();
	final Symbol blockName = null;
	final BlockBinding bb = new BlockBinding (blockName, valueClass, l1);
	final List<BlockBinding> blocks = new ArrayList<> ();
	blocks.add (bb);
	final TreeCompilerContext context = new TreeCompilerContext (this, quotedData, valueClass, mn, locals, blocks);
	for (int i = 0; i < bodyForms.size () - 1; i++)
	{
	    final Object expr = bodyForms.get (i);
	    final CompileResults resultClass = context.compile (expr, false);
	    context.convert (resultClass, void.class, false, false);
	}
	final CompileResults resultClass = context.compile (bodyForms.get (bodyForms.size () - 1), true);
	// (define double:foo (int:n) 1 2 n)
	for (final CompileResult resultKind : resultClass.getResults ())
	{
	    // TODO This should collect crs of the same class and make them jump to the same place
	    // instead of duplicating code
	    context.add (resultKind.getLabels ());
	    if (resultKind instanceof ExplicitResult)
	    {
		context.convert (((ExplicitResult)resultKind).getResultClass (), valueClass, false, false);
	    }
	    else
	    {
		final ImplicitResult icr = (ImplicitResult)resultKind;
		context.add (icr, valueClass);
	    }
	    context.add (new InsnNode (returnType.getOpcode (Opcodes.IRETURN)));
	}
	context.add (l1);
	context.add (new InsnNode (returnType.getOpcode (Opcodes.IRETURN)));
	// Better not get here
	mn.maxStack = 0;
	mn.maxLocals = 0;
	methods.add (mn);
    }

    private int getAccess (final List<Object[]> modifiers, final Object a)
    {
	final String ms = ((Symbol)a).getName ();
	final Find<Object[]> finder = new Find<> ();
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
	if (object instanceof List)
	{
	    final List<?> c = (List<?>)object;
	    if (c.size () > 0)
	    {
		final Object h = c.get (0);
		if (h instanceof Symbol)
		{
		    final Symbol key = (Symbol)h;
		    return key.is ("access") || key.is ("throws") || key.is ("annotation");
		}
	    }
	}
	return false;
    }

    private void processMethodHeader (final MethodNode mn, final List<Object[]> modifiers, final LispList m)
            throws ClassNotFoundException
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
	else if (key.is ("annotation"))
	{
	    parseMethodAnnotationClause (mn, c);
	}
    }

    /**
     * @param clause
     * @throws ClassNotFoundException
     * @See AnnotationNode source for information about the values field format.
     */
    private void parseMethodAnnotationClause (final MethodNode mn, final LispList clause) throws ClassNotFoundException
    {
	LOGGER.info (new LogString ("Method %s Annotation: %s", mn.name, clause));
	final Object annotationKind = clause.get (1);
	Class<?> annotationClass;
	if (annotationKind instanceof Class<?>)
	{
	    annotationClass = (Class<?>)annotationKind;
	}
	else if (annotationKind instanceof String)
	{
	    annotationClass = Class.forName ((String)annotationKind);
	}
	else
	{
	    throw new IllegalArgumentException (annotationKind + " is not an annotation");
	}
	final Type annotationType = Type.getType (annotationClass);
	final AnnotationNode annotation = new AnnotationNode (api, annotationType.getDescriptor ());
	for (int i = 0; i < clause.size (); i++)
	{
	    final String key = (String)clause.get (i - 1);
	    final Object value = clause.get (i);
	    annotation.values.add (key);
	    annotation.values.add (value);
	    verifyAnnotationMethod (annotationClass, key);
	}
	mn.visibleAnnotations.add (annotation);
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
	final List<Type> result = new ArrayList<> ();
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
	    buffer.append (argTypes.get (i).getDescriptor ());
	}
	buffer.append (")");
	buffer.append (returnTypeDescriptor);
	return buffer.toString ();
    }

    private String getGetterName (final Symbol field)
    {
	final String root = field.getName ();
	final char ch = root.charAt (0);
	final String methodName = "get" + Character.toUpperCase (ch) + root.substring (1);
	return methodName;
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
	mn.exceptions = new ArrayList<> ();
	final InsnList il = mn.instructions;
	if ((getterAccess & ACC_STATIC) != 0)
	{
	    il.add (new FieldInsnNode (Opcodes.GETSTATIC, classType.getInternalName (), fn.name, fn.desc));
	}
	else
	{
	    il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	    il.add (new FieldInsnNode (Opcodes.GETFIELD, classType.getInternalName (), fn.name, fn.desc));
	}
	il.add (new InsnNode (type.getOpcode (Opcodes.IRETURN)));
	methods.add (mn);
    }

    private String getSetterName (final Symbol field)
    {
	final String root = field.getName ();
	final char ch = root.charAt (0);
	final String methodName = "set" + Character.toUpperCase (ch) + root.substring (1);
	return methodName;
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
	mn.exceptions = new ArrayList<> ();
	final InsnList il = mn.instructions;
	if ((setterAccess & ACC_STATIC) != 0)
	{
	    il.add (new VarInsnNode (type.getOpcode (Opcodes.ILOAD), 0));
	    il.add (new FieldInsnNode (Opcodes.PUTSTATIC, classType.getInternalName (), fn.name, fn.desc));
	}
	else
	{
	    il.add (new VarInsnNode (Opcodes.ALOAD, 0));
	    il.add (new VarInsnNode (type.getOpcode (Opcodes.ILOAD), 1));
	    il.add (new FieldInsnNode (Opcodes.PUTFIELD, classType.getInternalName (), fn.name, fn.desc));
	}
	il.add (new InsnNode (Opcodes.RETURN));
	methods.add (mn);
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
