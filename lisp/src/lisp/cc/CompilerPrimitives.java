
package lisp.cc;

import java.io.*;
import java.util.*;

import lisp.eval.*;
import lisp.lang.*;
import lisp.symbol.*;

public class CompilerPrimitives extends Definer
{
    private static final char DOT = '.';

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

    @DefineLisp (special = true)
    public boolean compileFile (final LexicalContext context, final String inputFile, final String outputFile) throws Exception
    {
	return compileFile (context, new File (inputFile), new File (outputFile));
    }

    @DefineLisp (special = true)
    public boolean compileFile (final LexicalContext context, final File inputFile, final String outputFile) throws Exception
    {
	return compileFile (context, inputFile, new File (outputFile));
    }

    @DefineLisp (special = true)
    public boolean compileFile (final LexicalContext context, final String inputFile, final File outputFile) throws Exception
    {
	return compileFile (context, new File (inputFile), outputFile);
    }

    /**
     * Compile an input file to an output file. The output file is verified to have extension
     * .class.
     *
     * @param context The interpreter context.
     * @param inputFile The source file.
     * @param outputFile This must have the extension .class.
     * @return True if compilation finishes.
     * @throws Exception
     */
    @DefineLisp (special = true)
    public boolean compileFile (final LexicalContext context, final File inputFile, final File outputFile) throws Exception
    {
	final String outputName = outputFile.getName ();
	final int dot = outputName.lastIndexOf (DOT);
	if (dot < 0)
	{
	    return compileFile (context, new LispReader (), inputFile, new File (outputFile, outputName + ".class"));
	}
	else
	{
	    final String extension = outputName.substring (dot + 1);
	    if (!extension.equals ("class"))
	    {
		throw new IllegalArgumentException ("Compiler output files must end with .class");
	    }
	    return compileFile (context, new LispReader (), inputFile, outputFile);

	}
    }

    private boolean compileFile (final LexicalContext context, final LispReader reader, final File inputFile,
            final File outputFile) throws Exception
    {
	try (FileInputStream in = new FileInputStream (inputFile))
	{
	    final BufferedInputStream bufferedInput = new BufferedInputStream (in);
	    final LispStream inputStream = new LispInputStream (bufferedInput);
	    ensureOutputFileExists (outputFile);
	    try (final OutputStream outputStream = new BufferedOutputStream (new FileOutputStream (outputFile));)
	    {
		while (!inputStream.eof ())
		{
		    final Object item = reader.read (inputStream);
		    compileForm (context, item, outputStream);
		}
		outputStream.flush ();
		outputStream.close ();
	    }
	}
	catch (final EOFException e)
	{

	}
	return true;
    }

    private void compileForm (final LexicalContext context, final Object item, final OutputStream outputStream) throws Exception
    {
	if (item instanceof List)
	{
	    final List<?> form = (List<?>)item;
	    if (!form.isEmpty ())
	    {
		compileComplexForm (context, form, outputStream);
	    }
	}
    }

    private void compileComplexForm (final LexicalContext context, final List<?> form, final OutputStream outputStream)
            throws Exception
    {
	// Form is define, defclass or other
	// Special forms like progn, let should be walked
	// macros should be expanded
	final Object key = form.get (0);
	if (key instanceof Symbol)
	{
	    final Symbol fn = (Symbol)key;
	    compileFunctionCall (context, fn, form, outputStream);
	}
	else
	{
	    throw new IllegalArgumentException ("Can't compile form since it does not start with a symbol: " + form);
	}
    }

    private void compileFunctionCall (final LexicalContext context, final Symbol fn, final List<?> form,
            final OutputStream outputStream) throws Exception
    {
	if (fn.is ("%defclass"))
	{
	    compileDefclass (form, outputStream);
	}
	else if (fn.is ("progn"))
	{
	    compileProgn (context, form, outputStream);
	}
	else if (fn.is ("let"))
	{
	    compileLet (context, form, outputStream);
	}
	else if (fn.is ("let*"))
	{
	    compileLetStar (context, form, outputStream);
	}
	else
	{
	    // FIXME Handle user defined macros too
	    final FunctionCell function = fn.getFunction ();
	    if (function != null)
	    {
		if (function instanceof MacroFunctionCell)
		{
		    final MacroFunctionCell macro = (MacroFunctionCell)function;
		    final Object replacement = macro.expand (form);
		    compileForm (context, replacement, outputStream);
		    return;
		}
	    }
	    throw new IllegalArgumentException ("Can't compile toplevel call to " + fn);
	}
    }

    private void compileDefclass (final List<?> form, final OutputStream outputStream) throws IOException, ClassNotFoundException
    {
	final Object nameItem = form.get (1);
	final String name = (nameItem instanceof String) ? (String)nameItem : ((Symbol)nameItem).getName ();
	final LispList[] clauses = new LispList[form.size () - 2];
	for (int i = 2; i < form.size (); i++)
	{
	    clauses[i - 2] = (LispList)(form.get (i));
	}
	final QuotedData quotedDataHandler = new QuotedDataReader ();
	final Defclass defclass = new Defclass (quotedDataHandler, name, clauses);
	final byte[] b = defclass.getBytecode ();
	outputStream.write (b, 0, b.length);
    }

    private void compileProgn (final LexicalContext context, final List<?> form, final OutputStream outputStream) throws Exception
    {
	for (int i = 1; i < form.size (); i++)
	{
	    final Object item = form.get (i);
	    compileForm (context, item, outputStream);
	}
    }

    private void compileLet (final LexicalContext context, final List<?> form, final OutputStream outputStream) throws Exception
    {
	final LexicalContext newContext = new LexicalContext (context);
	final List<?> arglist = (List<?>)form.get (1);
	for (final Object c : arglist)
	{
	    final LispList clause = (LispList)c;
	    final Symbol var = (Symbol)clause.get (0);
	    final Object expr = clause.get (1);
	    // Evaluate expressions in the original context and bind in the newContext
	    newContext.bind (var, context.eval (expr));
	}
	for (int i = 2; i < form.size (); i++)
	{
	    final Object item = form.get (i);
	    compileForm (context, item, outputStream);
	}
    }

    private void compileLetStar (final LexicalContext context, final List<?> form, final OutputStream outputStream)
            throws Exception
    {
	final LexicalContext newContext = new LexicalContext (context);
	final List<?> arglist = (List<?>)form.get (1);
	for (final Object c : arglist)
	{
	    final LispList clause = (LispList)c;
	    final Symbol var = (Symbol)clause.get (0);
	    final Object expr = clause.get (1);
	    // Evaluate expressions in the newContext and bind in the newContext
	    newContext.bind (var, newContext.eval (expr));
	}
	for (int i = 2; i < form.size (); i++)
	{
	    final Object item = form.get (i);
	    compileForm (context, item, outputStream);
	}
    }

    private void ensureOutputFileExists (final File file)
    {
	final File folder = file.getParentFile ();
	if (!folder.exists ())
	{
	    folder.mkdirs ();
	}
    }
}
