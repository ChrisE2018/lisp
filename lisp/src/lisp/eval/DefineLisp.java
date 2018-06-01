
package lisp.eval;

import java.lang.annotation.*;

@Documented
@Target (ElementType.METHOD)
@Retention (RetentionPolicy.RUNTIME)
public @interface DefineLisp
{
    String packageName () default "system";

    String name () default "";

    String documentation () default "";

    boolean special () default false;

    boolean macro () default false;

    boolean compiler () default false;

    String classname () default "";
}
