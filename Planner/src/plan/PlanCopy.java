//
// package plan;
//
// import java.util.*;
//
// import lisp.Symbol;
//
/// **
// * Construct a copy of a plan with a map from parent plan nodes to child plan nodes. [TODO] Move
// * this function into the Achiever.
// */
// public class PlanCopy
// {
// private final Achiever achiever;
// private final Plan child;
// private final Map<Node, Node> nodeMap;
//
// /** Construct a copy of a plan with a map from parent plan nodes to child plan nodes. */
// private PlanCopy (final Achiever achiever)
// {
// this.achiever = achiever;
// final Plan parent = achiever.getParent ();
// final Symbol parentName = parent.getName ();
// final Symbol childName = parentName.gensym ();
// child = new Plan (childName, achiever);
// nodeMap = new HashMap<Node, Node> ();
// final List<Node> nodes = parent.getNodes ();
// for (final Node n : nodes)
// {
// final Node c = new Node (n);
// nodeMap.put (n, c);
// child.addNode (c);
// }
// for (final Node n : nodes)
// {
// final Node n2 = nodeMap.get (n);
// for (final Node a : n.getNext ())
// {
// final Node a2 = nodeMap.get (a);
// n2.addSuccessor (a2);
// }
// }
// // Copy causal links
// for (final Node n : nodes)
// {
// final Node n2 = nodeMap.get (n);
// for (final ProtectionInterval pi : n.getCausalLinks ())
// {
// final Condition condition = pi.getCondition ();
// final Node achieverCopy = nodeMap.get (pi.getAchiever ());
// final Node protectedNodeCopy = nodeMap.get (pi.getProtectedNode ());
// final ProtectionInterval pi2 = new ProtectionInterval (condition, achieverCopy,
// protectedNodeCopy);
// n2.addPI (pi2);
// }
// }
// }
//
// /** Get the achiever. */
// public Achiever getAchiever ()
// {
// return achiever;
// }
//
// /** Get the original plan. */
// public Plan getParent ()
// {
// return achiever.getParent ();
// }
//
// /** Get the copy of the original plan. */
// public Plan getChild ()
// {
// return child;
// }
//
// /** Get the map from original plan nodes to copy plan nodes. */
// public Map<Node, Node> getNodeMap ()
// {
// return nodeMap;
// }
//
// /** Get the copy of a node using the nodemap. */
// public Node getChildNode (final Node node)
// {
// return nodeMap.get (node);
// }
//
// @Override
// public String toString ()
// {
// final StringBuilder buffer = new StringBuilder ();
// buffer.append ("#<");
// buffer.append (getClass ().getSimpleName ());
// buffer.append (" ");
// buffer.append (achiever);
// buffer.append (" child:");
// buffer.append (child.getName ());
// buffer.append (">");
// return buffer.toString ();
// }
// }
