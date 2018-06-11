
package lisp.cc3;

import org.objectweb.asm.commons.GeneratorAdapter;

import lisp.LispList;
import lisp.symbol.LispFunction;

/***Base class for functions to support a Lisp function.This includes compiler and walker methods.**

@author cre
 */
 public interface LispCCFunction extends LispFunction
 {
 /** Compile to bytecode. */
 public void compile (final CompilerGenerator generator, final GeneratorAdapter mv, final LispList
 expression,
 final Class<?> valueClass, final boolean allowNarrowing, final boolean liberalTruth);
 }
