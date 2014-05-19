package graph

import util._

class DotGraph (depgraph: DependencyGraph) {
	val dg = depgraph

	def toDotSubgraph(dotfilename: String, member: NodeElem => String): Unit = {
		var groups = Map[String, List[NodeElem]]() 
		for (n <- dg.nodes.values ) {
			val grpstr = member(n)
			if ( !groups.contains(grpstr)) groups += (grpstr -> List[NodeElem]())
			
			groups(grpstr) = n :: groups(grpstr) 
		}
		
	     var (nodemap, count) = dg.nodes.values.toList.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))
	     
	     def groupString(name: String, ns: List[NodeElem]) = {
	    	 "subgraph " + name + "{ \n\t" + ns.map(nodeString(_)).mkString("\n\t") + "}" 
	     }
	     
	     def nodeString(n: NodeElem) = nodemap(n.toString) + " [ label=\"" + n.toDotLabel + "\"]" 
	     def edgeString(e: DepEdge) : String = (nodemap(e.getSource.toString) + "->" + nodemap(e.getTarget.toString))

	     val edgeStrs = dg.edges.foldLeft(Nil: List[String]) ( (r, e) => edgeString(e) :: r )
	     val file = new FileOut 
	    
	     file.saveFile(dotfilename + ".dot", 
	                   "digraph " + dotfilename + " { \n" 
	                   + groups.map(g => groupString(g._1, g._2)).mkString(";\n") + "\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
	}   
}