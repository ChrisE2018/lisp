
package lisp.eval;

import java.lang.annotation.*;

@Documented
@Target (ElementType.METHOD)
@Retention (RetentionPolicy.RUNTIME)
public @interface DefineLisp
{
    String packageName () default "system";

    String name () default "";

    // boolean external () default true;

    boolean special () default false;

    boolean macro () default false;
}
