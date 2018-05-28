
package lisp.cc;

import java.util.*;

import lisp.*;

public class Prototype
{
    private final Symbol name;
    private final List<Symbol> argNames;
    private final List<Class<?>> argTypes;
    private final Class<?> returnType;

    public Prototype (final Symbol name, final List<Symbol> argNames, final List<Class<?>> argTypes, final Class<?> returnType)
    {
	this.name = name;
	this.argNames = argNames;
	this.argTypes = argTypes;
	this.returnType = returnType;
    }

    public Prototype (final Symbol name, final LispList arguments, final Class<?> returnType)
    {
	this.name = name;
	argNames = new ArrayList<Symbol> ();
	argTypes = new ArrayList<Class<?>> ();
	for (final Object arg : arguments)
	{
	    argNames.add (CompileSupport.getNameVariable (arg));
	    argTypes.add (CompileSupport.getNameType (arg));
	}
	this.returnType = returnType;
    }

    public Symbol getName ()
    {
	return name;
    }

    public List<Symbol> getArgNames ()
    {
	return argNames;
    }

    public List<Class<?>> getArgTypes ()
    {
	return argTypes;
    }

    public Class<?> getReturnType ()
    {
	return returnType;
    }

    @Override
    public String toString ()
    {
	final StringBuilder buffer = new StringBuilder ();
	buffer.append ("#<");
	buffer.append (getClass ().getSimpleName ());
	buffer.append (" ");
	buffer.append (returnType);
	buffer.append (" ");
	buffer.append (name);
	buffer.append (" ");
	buffer.append ("(");
	for (int i = 0; i < argTypes.size (); i++)
	{
	    if (i > 0)
	    {
		buffer.append (", ");
	    }
	    buffer.append (argTypes.get (i).getSimpleName ());
	}
	buffer.append (")");
	buffer.append (">");
	return buffer.toString ();
    }
}
