
package lisp.cc;

import java.util.*;

import lisp.lang.*;
import lisp.symbol.*;
import lisp.util.Promotion;

public class Analyzer
{
    private static Promotion PROMOTION = new Promotion ();

    private class Local
    {
	@SuppressWarnings ("unused")
	private final Symbol varName;
	private final Class<?> varType;

	Local (final Symbol varName, final Class<?> varType)
	{
	    this.varName = varName;
	    this.varType = varType;
	}
    }

    private final String className;
    private final Class<?> returnType;
    private final Symbol methodName;
    @SuppressWarnings ("unused")
    private final LispList methodArgs;
    private final List<Symbol> methodArgNames = new ArrayList<Symbol> ();
    private final LispList methodBody;
    private final Map<Symbol, Class<?>> methodArgMap;
    private final Map<Symbol, List<Prototype>> prototypes = new HashMap<Symbol, List<Prototype>> ();

    public Analyzer (final String className, final Class<?> returnType, final Symbol methodName, final LispList methodArgs,
            final LispList methodBody)
    {
	this.className = className;
	this.returnType = returnType;
	this.methodName = methodName;
	this.methodArgs = methodArgs;
	this.methodBody = methodBody;
	methodArgMap = new HashMap<Symbol, Class<?>> ();
	for (final Object nameSpec : methodArgs)
	{
	    final Symbol argName = NameSpec.getVariableName (nameSpec);
	    final Class<?> argType = NameSpec.getVariableClass (nameSpec);
	    methodArgNames.add (argName);
	    methodArgMap.put (argName, argType);
	}
	final Symbol protoSymbol = PackageFactory.getSystemPackage ().internSymbol ("*protos*");
	@SuppressWarnings ("unchecked")
	final List<Prototype> protos = (List<Prototype>)protoSymbol.getValue ();
	for (final Prototype proto : protos)
	{
	    final Symbol protoName = proto.getName ();
	    List<Prototype> bucket = prototypes.get (protoName);
	    if (bucket == null)
	    {
		bucket = new ArrayList<Prototype> ();
		prototypes.put (protoName, bucket);
	    }
	    // final List<Class<?>> protoArgs = proto.getArgTypes ();
	    // final Class<?> protoReturnType = proto.getReturnType ();
	    // final LispList spec = new LispList ();
	    // final LispList protoTypes = new LispList ();
	    // for (final Object arg : protoArgs)
	    // {
	    // protoTypes.add (CompileSupport.getNameType (arg));
	    // }
	    // spec.add (protoName);
	    // spec.add (protoTypes);
	    // spec.add (protoReturnType);
	    bucket.add (proto);
	}
    }

    public void analyze ()
    {
	// (analyze int:foo (int:x) (1+ x))
	// (analyze int:foo (x) (1+ x))
	// (analyze int:foo (a b) a b)
	// (analyze fact (x) (if (<= x 1) 1 (* x (fact (1- x)))))
	// (analyze int:fact (x) (if (<= x 1) 1 (* x (fact (1- x)))))
	// (analyze int:fact (int:x) (if (<= x 1) 1 (* x (fact (1- x)))))
	System.out.printf ("%nAnalyzing %s %n", methodName);
	final Map<Symbol, Local> localVariables = new HashMap<Symbol, Local> ();
	analyze (methodBody, localVariables, returnType);
    }

    private void analyze (final LispList forms, final Map<Symbol, Local> localVariables, final Class<?> valueType)
    {
	for (int i = 0; i < forms.size () - 1; i++)
	{
	    final Object expression = forms.get (i);
	    analyzeVoidExpression (0, expression, localVariables);
	}
	analyzeValueExpression (0, forms.last (), localVariables, valueType);
    }

    private void analyzeVoidExpression (final int level, final Object expression, final Map<Symbol, Local> localVariables)
    {
	if (expression == null)
	{
	    msg (level, "Null is an illegal expression");
	}
	else if (expression instanceof LispList)
	{
	    analyzeVoidFunctionCall (1 + level, (LispList)expression, localVariables);
	}
	else if (expression instanceof Symbol)
	{
	    final Symbol var = (Symbol)expression;
	    final String varCat = getVariableCategory (var, localVariables);
	    final Class<?> varType = getVariableType (var, localVariables);
	    msg (level, "Value of %s %s => %s ignored", varCat, var, varType);
	}
	else
	{
	    // Constants will do nothing
	    msg (level, "Reference to constant %s ignored", expression);
	}
    }

    private void analyzeVoidFunctionCall (final int level, final LispList expression, final Map<Symbol, Local> localVariables)
    {
	final Symbol fn = (Symbol)expression.get (0);
	final FunctionCell fc = fn.getFunction ();
	if (fc == null)
	{
	    if (fn == methodName)
	    {
		msg (level, "Recursive function %s called for void value", fn);
		for (int i = 1; i < expression.size (); i++)
		{
		    final Symbol arg = methodArgNames.get (i - 1);
		    final Class<?> argType = methodArgMap.get (arg);
		    analyzeValueExpression (level + 1, expression.get (i), localVariables, argType);
		}
	    }
	    else
	    {
		msg (level, "Undefined function %s called for void value", fn);
		for (int i = 1; i < expression.size (); i++)
		{
		    analyzeValueExpression (level + 1, expression.get (i), localVariables, Object.class);
		}
	    }
	}
	else if (fc instanceof MacroFunctionCell)
	{
	    msg (level, "Macro function %s called for void value", fn);
	}
	else if (fc instanceof DefFunctionCell)
	{
	    msg (level, "Interpreted def function %s called for void value", fn);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeValueExpression (level + 1, expression.get (i), localVariables, Object.class);
	    }
	}
	else if (fc instanceof StandardFunctionCell)
	{
	    msg (level, "Compiled standard function %s called for void value", fn);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeValueExpression (level + 1, expression.get (i), localVariables, Object.class);
	    }
	}
	else if (fc instanceof SpecialFunctionCell)
	{
	    msg (level, "Special function %s called for void value", fn);
	    analyzeVoidSpecialForm (level, expression, localVariables);
	}
	else
	{
	    msg (level, "Unknown function %s called for void value", fn);
	}
    }

    /**
     * Analyze an expression whose value will be used.
     *
     * @param level Nesting level for log messages.
     * @param expression The expression to analyze.
     * @param localVariables Lexical variable bindings.
     * @param valueType The expected value type.
     */
    private List<Class<?>> analyzeValueExpression (final int level, final Object expression,
            final Map<Symbol, Local> localVariables, final Class<?> valueType)
    {
	// CONSIDER This should return a list of expression value types that can be produced.
	if (expression == null)
	{
	    msg (level, "Null is an illegal expression");
	    return null;
	}
	else if (expression instanceof LispList)
	{
	    return analyzeValueFunctionCall (1 + level, (LispList)expression, localVariables, valueType);
	}
	else if (expression instanceof Symbol)
	{
	    // Argument, local or global.
	    // Undefined will cause an error, but no other result is possible.
	    final Symbol var = (Symbol)expression;
	    final String varCat = getVariableCategory (var, localVariables);
	    final Class<?> varType = getVariableType (var, localVariables);
	    final List<Class<?>> promotions = PROMOTION.getPromotion (varType);
	    if (promotions == null || !promotions.contains (valueType))
	    {
		msg (level, "Invalid value of %s %s => %s can't set to %s", varCat, var, valueType, varType);
		return null;
	    }
	    else
	    {
		// final List<Class<?>> result = new ArrayList<Class<?>> ();
		// result.add (varType);
		msg (level, "%s value of %s %s => %s", valueType, varCat, var, promotions);
		return promotions;
	    }
	}
	// Compile constant expressions
	else
	{
	    final Class<?> expressionType = expression.getClass ();
	    final List<Class<?>> promotions = PROMOTION.getPromotion (expressionType);
	    if (promotions.contains (valueType))
	    {
		msg (level, "%s reference to %s constant %s: %s", valueType, expressionType, expression, promotions);
		return promotions;
	    }
	    else
	    {
		msg (level, "Impossible %s reference to %s constant %s: %s", valueType, expressionType, expression, promotions);
		return null;
	    }

	}
    }

    private String getVariableCategory (final Symbol symbol, final Map<Symbol, Local> localVariables)
    {
	if (localVariables.containsKey (symbol))
	{
	    return "local";
	}
	else if (methodArgMap.containsKey (symbol))
	{
	    return "param";
	}
	// Global variable reference
	else if (!symbol.hasValue ())
	{
	    return "unbound";
	}
	else
	{
	    return "global";
	}
    }

    private Class<?> getVariableType (final Symbol symbol, final Map<Symbol, Local> localVariables)
    {
	if (localVariables.containsKey (symbol))
	{
	    // Local variable reference
	    return localVariables.get (symbol).varType;
	}
	else if (methodArgMap.containsKey (symbol))
	{
	    return methodArgMap.get (symbol);
	}
	else
	{
	    // Global variable reference
	    if (!symbol.hasValue ())
	    {
		// Not currently bound
		return Object.class;
	    }
	    else
	    {
		final ValueCell vc = symbol.getValueCell ();
		return vc.getValueType ();
	    }
	}
    }

    // private Class<?> getCoercedClass (final Class<?> expectedValue, final Class<?> actualValue)
    // {
    // if (expectedValue.isAssignableFrom (actualValue))
    // {
    // return actualValue;
    // }
    // // Try to coerce to a valid type
    // if (actualValue.equals (boolean.class))
    // {
    // if (expectedValue.isAssignableFrom (Boolean.class))
    // {
    // return Boolean.class;
    // }
    // }
    // if (actualValue.equals (short.class))
    // {
    // if (expectedValue.isAssignableFrom (Short.class))
    // {
    // return Short.class;
    // }
    // }
    // if (actualValue.equals (int.class))
    // {
    // if (expectedValue.isAssignableFrom (Integer.class))
    // {
    // return Integer.class;
    // }
    // }
    // if (actualValue.equals (long.class))
    // {
    // if (expectedValue.isAssignableFrom (Long.class))
    // {
    // return Long.class;
    // }
    // }
    // if (actualValue.equals (double.class))
    // {
    // if (expectedValue.isAssignableFrom (Double.class))
    // {
    // return Double.class;
    // }
    // }
    // if (actualValue.equals (float.class))
    // {
    // if (expectedValue.isAssignableFrom (Float.class))
    // {
    // return Float.class;
    // }
    // }
    // if (actualValue.equals (char.class))
    // {
    // if (expectedValue.isAssignableFrom (Character.class))
    // {
    // return Character.class;
    // }
    // }
    // if (actualValue.equals (byte.class))
    // {
    // if (expectedValue.isAssignableFrom (Byte.class))
    // {
    // return Byte.class;
    // }
    // }
    // return null;
    // }

    private List<Class<?>> analyzeValueFunctionCall (final int level, final LispList expression,
            final Map<Symbol, Local> localVariables, final Class<?> valueType)
    {
	final List<Class<?>> result = new ArrayList<Class<?>> ();
	final Symbol fn = (Symbol)expression.get (0);
	final FunctionCell fc = fn.getFunction ();
	// CONSIDER The potential types of the function arguments should be determined and a
	// prototype
	// selected to produce the best method overload.
	if (fc == null)
	{
	    if (fn == methodName)
	    {
		msg (level, "Recursive function %s called for %s value", fn, valueType);
		for (int i = 1; i < expression.size (); i++)
		{
		    final Symbol arg = methodArgNames.get (i - 1);
		    final Class<?> argType = methodArgMap.get (arg);
		    analyzeValueExpression (level + 1, expression.get (i), localVariables, argType);
		}
		result.add (returnType);
	    }
	    else
	    {
		msg (level, "Undefined function %s called for %s value", fn, valueType);
		for (int i = 1; i < expression.size (); i++)
		{
		    // CONSIDER The Object.class valueType is forcing the chooseFunction method to
		    // select a prototype that returns an Object and not allowing primitive data
		    // types that could be converted to Object. This needs to be fixed.
		    analyzeValueExpression (level + 1, expression.get (i), localVariables, Object.class);
		}
		result.add (Object.class);
	    }
	}
	else if (fc instanceof MacroFunctionCell)
	{
	    msg (level, "Macro function %s called for %s value", fn, valueType);
	    result.add (Object.class);
	}
	else if (fc instanceof DefFunctionCell)
	{
	    msg (level, "Interpreted def function %s called for %s value", fn, valueType);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeValueExpression (level + 1, expression.get (i), localVariables, Object.class);
	    }
	    result.add (Object.class);
	}
	else if (fc instanceof StandardFunctionCell)
	{
	    msg (level, "Compiled standard function %s called for %s value", fn, valueType);
	    final List<List<Class<?>>> argValues = new ArrayList<List<Class<?>>> ();
	    for (int i = 1; i < expression.size (); i++)
	    {
		final List<Class<?>> values =
		    analyzeValueExpression (level + 1, expression.get (i), localVariables, Object.class);
		argValues.add (values);
	    }
	    final List<Prototype> protos = chooseFunction (level, fn, argValues, valueType);
	    if (protos.isEmpty ())
	    {
		msg (level, "Function %s arguments:", fn);
		for (final List<Class<?>> arg : argValues)
		{
		    final StringBuilder buffer = new StringBuilder ();
		    buffer.append ("*");
		    if (arg == null)
		    {
			buffer.append (" no possible values");
		    }
		    else
		    {
			for (final Class<?> c : arg)
			{
			    buffer.append (' ');
			    buffer.append (c.getSimpleName ());
			}
		    }
		    msg (level + 1, buffer.toString ());
		}
	    }
	    else
	    {
		final Prototype selectedProto = protos.get (0);
		msg (level, "Selected %s", selectedProto);
	    }
	}
	else if (fc instanceof SpecialFunctionCell)
	{
	    msg (level, "Special function %s called for %s value", fn, valueType);
	    analyzeValueSpecialForm (level, expression, localVariables, valueType);
	}
	else
	{
	    msg (level, "Unknown function %s called for %s value", fn, valueType);
	}
	return result;
    }

    private List<Prototype> chooseFunction (final int level, final Symbol fn, final List<List<Class<?>>> argValues,
            final Class<?> valueType)
    {
	final List<Prototype> result = new ArrayList<Prototype> ();
	final List<Prototype> bucket = prototypes.get (fn);
	if (bucket != null)
	{
	    for (final Prototype proto : bucket)
	    {
		if (isPrototypeApplicable (proto, argValues, valueType))
		{
		    result.add (proto);
		}
	    }
	}
	if (result.size () > 0)
	{
	    for (final Prototype proto : result)
	    {
		msg (level, "%s prototype option %s", fn, proto);
	    }
	}
	else
	{
	    msg (level, "%s has no prototype options", fn);
	}
	return result;
    }

    private boolean isPrototypeApplicable (final Prototype proto, final List<List<Class<?>>> argValues, final Class<?> valueType)
    {
	final List<Class<?>> returnPromotions = PROMOTION.getPromotion (proto.getReturnType ());
	for (final Class<?> rt : returnPromotions)
	{
	    if (valueType.isAssignableFrom (rt))
	    {
		final List<Class<?>> protoArgs = proto.getArgTypes ();
		if (protoArgs.size () == argValues.size ())
		{
		    for (int i = 0; i < protoArgs.size (); i++)
		    {
			final Object arg = protoArgs.get (i);
			final List<Class<?>> values = argValues.get (i);
			if (values == null || !values.contains (arg))
			{
			    return false;
			}
		    }
		    return true;
		}
	    }
	}
	return false;
    }

    private void analyzeVoidSpecialForm (final int level, final LispList expression, final Map<Symbol, Local> localVariables)
    {
	// quote progn when if unless and or setq repeat while until let let* cond
	final Symbol fn = (Symbol)expression.get (0);
	if (fn.is ("quote"))
	{
	    msg (level, "Quoted expression %s ignored", expression.get (1));
	}
	else if (fn.is ("progn"))
	{
	    msg (level, "Progn expression %s ignored", expression);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("when"))
	{
	    msg (level, "When expression %s ignored", expression);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("if"))
	{
	    msg (level, "If expression %s ignored", expression);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("unless"))
	{
	    msg (level, "Unless expression %s ignored", expression);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("and"))
	{
	    msg (level, "And expression %s ignored", expression);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("or"))
	{
	    msg (level, "Or expression %s ignored", expression);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("setq"))
	{
	    msg (level, "Setq expression %s ignored", expression);
	    // CONSIDER Determine type of variable
	    analyzeValueExpression (level + 1, expression.get (2), localVariables, Object.class);
	}
	else if (fn.is ("repeat"))
	{
	    msg (level, "Repeat expression %s ignored", expression);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, int.class);
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("while"))
	{
	    msg (level, "While expression %s ignored", expression);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("until"))
	{
	    msg (level, "Until expression %s ignored", expression);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	}
	else if (fn.is ("let"))
	{
	    msg (level, "Let expression %s ignored", expression);
	    final Map<Symbol, Local> boundVariables = new HashMap<Symbol, Local> (localVariables);
	    final LispList bindings = (LispList)expression.get (1);
	    for (int i = 0; i < bindings.size (); i++)
	    {
		final LispList clause = (LispList)bindings.get (i);
		final Object nameSpec = clause.get (0);
		final Symbol varName = NameSpec.getVariableName (nameSpec);
		final Class<?> varType = NameSpec.getVariableClass (nameSpec);
		analyzeValueExpression (level + 1, clause.get (1), localVariables, varType);
		final Local varSpec = new Local (varName, varType);
		boundVariables.put (varName, varSpec);
	    }
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), boundVariables);
	    }
	}
	else if (fn.is ("let*"))
	{
	    msg (level, "Let* expression %s ignored", expression);
	    final Map<Symbol, Local> boundVariables = new HashMap<Symbol, Local> (localVariables);
	    final LispList bindings = (LispList)expression.get (1);
	    for (int i = 0; i < bindings.size (); i++)
	    {
		final LispList clause = (LispList)bindings.get (i);
		final Object nameSpec = clause.get (0);
		final Symbol varName = NameSpec.getVariableName (nameSpec);
		final Class<?> varType = NameSpec.getVariableClass (nameSpec);
		analyzeValueExpression (level + 1, clause.get (1), boundVariables, varType);
		final Local varSpec = new Local (varName, varType);
		boundVariables.put (varName, varSpec);
	    }
	    for (int i = 2; i < expression.size (); i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), boundVariables);
	    }
	}
	else if (fn.is ("cond"))
	{
	    msg (level, "Cond expression %s ignored", expression);
	    for (int i = 1; i < expression.size (); i++)
	    {
		final LispList clause = (LispList)expression.get (i);
		analyzeValueExpression (level + 1, clause.get (0), localVariables, boolean.class);
		if (clause.size () > 1)
		{
		    for (int j = 2; i < clause.size () - 1; j++)
		    {
			analyzeVoidExpression (level + 1, clause.get (j), localVariables);
		    }
		    analyzeVoidExpression (level + 1, clause.last (), localVariables);
		}
	    }
	}
	else
	{
	    msg (level, "Unknown special form %s ignored", expression);
	}
    }

    private void analyzeValueSpecialForm (final int level, final LispList expression, final Map<Symbol, Local> localVariables,
            final Class<?> valueType)
    {
	// quote progn when if unless and or setq repeat while until let let* cond
	final Symbol fn = (Symbol)expression.get (0);
	if (fn.is ("quote"))
	{
	    msg (level, "Quoted expression %s => %s", expression.get (1), valueType);
	}
	else if (fn.is ("progn"))
	{
	    msg (level, "Progn expression %s => %s", expression, valueType);
	    for (int i = 1; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	}
	else if (fn.is ("when"))
	{
	    msg (level, "When expression %s => %s", expression, valueType);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	}
	else if (fn.is ("if"))
	{
	    msg (level, "If expression %s => %s", expression, valueType);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    analyzeValueExpression (level + 1, expression.get (2), localVariables, valueType);
	    if (expression.size () > 3)
	    {
		for (int i = 3; i < expression.size () - 1; i++)
		{
		    analyzeVoidExpression (level + 1, expression.get (i), localVariables);
		}
		analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	    }
	}
	else if (fn.is ("unless"))
	{
	    msg (level, "Unless expression %s => %s", expression, valueType);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	}
	else if (fn.is ("and"))
	{
	    msg (level, "And expression %s => %s", expression, valueType);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeValueExpression (level + 1, expression.get (i), localVariables, valueType);
	    }
	}
	else if (fn.is ("or"))
	{
	    msg (level, "Or expression %s => %s", expression, valueType);
	    for (int i = 1; i < expression.size (); i++)
	    {
		analyzeValueExpression (level + 1, expression.get (i), localVariables, valueType);
	    }
	}
	else if (fn.is ("setq"))
	{
	    msg (level, "Setq expression %s => %s", expression, valueType);
	    // CONSIDER Determine type of variable
	    analyzeValueExpression (level + 1, expression.get (2), localVariables, Object.class);
	}
	else if (fn.is ("repeat"))
	{
	    msg (level, "Repeat expression %s => %s", expression, valueType);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, int.class);
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	}
	else if (fn.is ("while"))
	{
	    msg (level, "While expression %s => %s", expression, valueType);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	}
	else if (fn.is ("until"))
	{
	    msg (level, "Until expression %s => %s", expression, valueType);
	    analyzeValueExpression (level + 1, expression.get (1), localVariables, boolean.class);
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), localVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), localVariables, valueType);
	}
	else if (fn.is ("let"))
	{
	    msg (level, "Let expression %s => %s", expression, valueType);
	    final Map<Symbol, Local> boundVariables = new HashMap<Symbol, Local> (localVariables);
	    final LispList bindings = (LispList)expression.get (1);
	    for (int i = 0; i < bindings.size (); i++)
	    {
		final LispList clause = (LispList)bindings.get (i);
		final Object nameSpec = clause.get (0);
		final Symbol varName = NameSpec.getVariableName (nameSpec);
		final Class<?> varType = NameSpec.getVariableClass (nameSpec);
		analyzeValueExpression (level + 1, clause.get (1), localVariables, varType);
		final Local varSpec = new Local (varName, varType);
		boundVariables.put (varName, varSpec);
	    }
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), boundVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), boundVariables, valueType);
	}
	else if (fn.is ("let*"))
	{
	    msg (level, "Let* expression %s => %s", expression, valueType);
	    final Map<Symbol, Local> boundVariables = new HashMap<Symbol, Local> (localVariables);
	    final LispList bindings = (LispList)expression.get (1);
	    for (int i = 0; i < bindings.size (); i++)
	    {
		final LispList clause = (LispList)bindings.get (i);
		final Object nameSpec = clause.get (0);
		final Symbol varName = NameSpec.getVariableName (nameSpec);
		final Class<?> varType = NameSpec.getVariableClass (nameSpec);
		analyzeValueExpression (level + 1, clause.get (1), boundVariables, varType);
		final Local varSpec = new Local (varName, varType);
		boundVariables.put (varName, varSpec);
	    }
	    for (int i = 2; i < expression.size () - 1; i++)
	    {
		analyzeVoidExpression (level + 1, expression.get (i), boundVariables);
	    }
	    analyzeValueExpression (level + 1, expression.last (), boundVariables, valueType);
	}
	else if (fn.is ("cond"))
	{
	    msg (level, "Cond expression %s => %s", expression, valueType);
	    for (int i = 1; i < expression.size (); i++)
	    {
		final LispList clause = (LispList)expression.get (i);
		analyzeValueExpression (level + 1, clause.get (0), localVariables, boolean.class);
		if (clause.size () > 1)
		{
		    for (int j = 2; i < clause.size () - 1; j++)
		    {
			analyzeVoidExpression (level + 1, clause.get (j), localVariables);
		    }
		    analyzeValueExpression (level + 1, clause.last (), localVariables, valueType);
		}
	    }
	}
	else
	{
	    msg (level, "Unknown special form %s => %s", expression, valueType);
	}
    }

    private void msg (final int level, final String format, final Object... args)
    {
	for (int i = 0; i < level; i++)
	{
	    System.out.print ("|  ");
	}
	System.out.printf (format, args);
	System.out.println ();
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (className);
	buffer.append (" ");
	buffer.append (methodName);
	buffer.append (">");
	return buffer.toString ();
    }
}
