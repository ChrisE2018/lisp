
package lisp.eval;

import java.lang.annotation.*;

@Documented
@Target (ElementType.METHOD)
@Retention (RetentionPolicy.RUNTIME)
public @interface DefineLisp
{
    /** The package for the function name symbol. */
    String packageName () default "lisp.lang";

    /**
     * The name of the function to create. If this is not supplied, the name of the method is used.
     */
    String name () default "";

    /** Text to explain this function. */
    String documentation () default "";

    /**
     * Special form definitions fully control the evaluation process. The interpreter passes itself
     * and the original expression to a special form function. The compiler requires individual
     * support for each special form.
     */
    boolean special () default false;

    /**
     * A macro function is applied to the original expression and returns a new Jisp expression that
     * replaces the original. The new expression is then evaluated or compiled.
     */
    boolean macro () default false;

    /**
     * A fully qualified classname provides a supporting class for this function. The compiler for a
     * special form is implemented as a method in the supporting class.
     */
    String classname () default "";
}
