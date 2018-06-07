//
// package lisp.cc4;
//
// import java.util.*;
//
// import org.objectweb.asm.tree.LabelNode;
//
/// **
// * A label node that can be combined.
// *
// * @Deprecated Just use extra gotos and let them be optimized out.
// * @author cre
// */
// @Deprecated
// public class LabelNodeSet extends LabelNode
// {
// private final List<LabelNode> labels = new ArrayList<LabelNode> ();
//
// public LabelNodeSet (final LabelNode... labels)
// {
// for (final LabelNode l : labels)
// {
// this.labels.add (l);
// }
// }
//
// public void add (final LabelNode... newLabels)
// {
// for (final LabelNode l : newLabels)
// {
// labels.add (l);
// }
// }
//
// public List<LabelNode> getLabels ()
// {
// return labels;
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
