
package lisp.primitives;

import java.util.logging.*;

import lisp.eval.*;

/**
 * Functions to get access to the logging system. These are wired to the limited standard logger.
 * Others should be supported too.
 *
 * @see Log4j vs Log4j2 vs Logback vs java.util.logging.
 * @see https://blog.takipi.com/is-standard-java-logging-dead-log4j-vs-log4j2-vs-logback-vs-java-util-logging/
 * @author cre
 */
public class LoggerPrimitives extends Definer
{
    @DefineLisp
    public Logger getLogger (final String name)
    {
	// (getLogger "lisp.cc.PrintBytecodeClassAdaptor")
	return Logger.getLogger (name);
    }

    @DefineLisp
    public void setLoggerLevel (final String name, final String levelName)
    {
	final Logger logger = Logger.getLogger (name);
	final Level level = Level.parse (levelName);
	logger.setLevel (level);
    }

    @DefineLisp
    public Level getLoggerLevel (final String levelName)
    {
	return Level.parse (levelName);
    }

    @DefineLisp
    public boolean isBytecode ()
    {
	final Logger logger = Logger.getLogger (lisp.cc.PrintBytecodeClassAdaptor.class.getName ());
	return logger.isLoggable (Level.FINE);
    }

    @DefineLisp
    public void showBytecode (final boolean flag)
    {
	final Logger logger = Logger.getLogger (lisp.cc.PrintBytecodeClassAdaptor.class.getName ());
	logger.setLevel (flag ? Level.FINE : Level.WARNING);
    }

    @DefineLisp
    public String showBytecode ()
    {
	final Logger logger = Logger.getLogger (lisp.cc.PrintBytecodeClassAdaptor.class.getName ());
	if (logger.isLoggable (Level.FINE))
	{
	    logger.setLevel (Level.WARNING);
	    return "Bytecode will not be shown";
	}
	else
	{
	    logger.setLevel (Level.FINE);
	    return "Bytecode is being shown";
	}
    }
}
