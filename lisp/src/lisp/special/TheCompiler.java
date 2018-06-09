//
// package lisp.special;
//
// import org.objectweb.asm.Opcodes;
// import org.objectweb.asm.commons.GeneratorAdapter;
//
// import lisp.*;
// import lisp.cc.*;
// import lisp.eval.*;
//
// public class TheCompiler extends LogicDefiner implements Opcodes
// {
// private static Convert convert = new Convert ();
//
// @DefineLisp (special = true, name = "the", compiler = true)
// public void compileThe (final CompilerGenerator generator, final GeneratorAdapter mv, final
// LispList e,
// final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
// {
// final Object type = e.get (1);
// final Object arg = e.get (2);
//
// compileThe (generator, mv, type, arg, valueClass, allowNarrowing, liberalTruth);
// }
//
// private void compileThe (final CompilerGenerator generator, final GeneratorAdapter mv, final
// Object type, final Object arg,
// final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth)
// {
// // (setq system.showBytecode t)
// // (define foo () byte:3)
// // TODO Need to allow narrowing conversions here
// if (type instanceof Symbol)
// {
// final Symbol t = (Symbol)type;
// if (t.is ("byte"))
// {
// // (define byte:foo () (the byte int:3))
// // (define byte:foo () byte:int:3)
// // (d (foo))
// generator.compileExpression (mv, arg, byte.class, true, false);
// convert.convert (mv, byte.class, valueClass, true, false);
// return;
// }
// if (t.is ("char"))
// {
// generator.compileExpression (mv, arg, int.class, true, false);
// // mv.visitInsn (I2C); // Narrow
// convert.convert (mv, char.class, valueClass, true, false);
// return;
// }
// if (t.is ("short"))
// {
// // (define short:foo () (the short int:3))
// generator.compileExpression (mv, arg, int.class, true, false);
// convert.convert (mv, short.class, valueClass, true, false);
// return;
// }
// if (t.is ("int"))
// {
// generator.compileExpression (mv, arg, int.class, true, false);
// convert.convert (mv, int.class, valueClass, true, false);
// return;
// }
// if (t.is ("long"))
// {
// // (define long:foo () int:3) ; Widening
// // (define long:foo () float:3.3) ; Error needs cast
// // (define long:foo () long:float:3.3) ; Explicit cast
// generator.compileExpression (mv, arg, long.class, true, false);
// convert.convert (mv, long.class, valueClass, true, false);
// return;
// }
// if (t.is ("float"))
// {
// // NOT WORKING
// // mv.visitLdcInsn ((float)6.9);
// generator.compileExpression (mv, arg, float.class, true, false);
// convert.convert (mv, float.class, valueClass, true, false);
// return;
// }
// if (t.is ("double"))
// {
// generator.compileExpression (mv, arg, double.class, true, false);
// convert.convert (mv, double.class, valueClass, true, false);
// return;
// }
// compileThe (generator, mv, t.getName (), arg, valueClass, allowNarrowing, liberalTruth);
// return;
// }
// // Narrowing has not been implemented below this line
// else if (type instanceof Class)
// {
// final Class<?> c = (Class<?>)type;
// convert.convert (mv, Object.class, c, true, false);
// convert.convert (mv, c, valueClass, false, false);
// return;
// }
// if (type instanceof String)
// {
// final String t = (String)type;
// try
// {
// final Class<?> c = Class.forName (t);
// convert.convert (mv, c, valueClass, false, false);
// return;
// }
// catch (final ClassNotFoundException e)
// {
// }
// if (t.indexOf (".") < 0)
// {
// try
// {
// final Class<?> c = Class.forName ("java.lang." + t);
// convert.convert (mv, c, valueClass, false, false);
// return;
// }
// catch (final ClassNotFoundException e)
// {
// }
// }
// }
// }
//
// @Override
// public String toString ()
// {
// final StringBuilder buffer = new StringBuilder ();
// buffer.append ("#<");
// buffer.append (getClass ().getSimpleName ());
// buffer.append (" ");
// buffer.append (System.identityHashCode (this));
// buffer.append (">");
// return buffer.toString ();
// }
// }
