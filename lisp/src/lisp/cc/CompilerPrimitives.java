
package lisp.cc;

import java.util.*;

import lisp.eval.*;
import lisp.lang.*;

public class CompilerPrimitives extends Definer
{
    @DefineLisp (special = true)
    public Object analyze (@SuppressWarnings ("unused") final LexicalContext context, final Object nameSpec,
            final LispList methodArgs, final Object... bodyForms)
    {
	final Class<?> returnType = NameSpec.getVariableClass (nameSpec);
	final Symbol methodName = NameSpec.getVariableName (nameSpec);
	final LispList body = new LispList (bodyForms);
	final Analyzer analyzer = new Analyzer ("foo", returnType, methodName, methodArgs, body);
	analyzer.analyze ();
	return analyzer;
    }

    @DefineLisp (special = true)
    public Object defproto (@SuppressWarnings ("unused") final LexicalContext context, final Object nameSpec,
            final LispList methodArgs)
    {
	final Symbol protos = PackageFactory.getSystemPackage ().internSymbol ("*protos*");
	final Class<?> returnType = NameSpec.getVariableClass (nameSpec);
	final Symbol methodName = NameSpec.getVariableName (nameSpec);
	final Prototype spec = new Prototype (methodName, methodArgs, returnType);
	if (!protos.hasValue ())
	{
	    protos.setValue (new ArrayList<Prototype> ());
	}
	@SuppressWarnings ("unchecked")
	final List<Prototype> list = (List<Prototype>)protos.getValue ();
	list.add (spec);
	return spec;
    }
}
