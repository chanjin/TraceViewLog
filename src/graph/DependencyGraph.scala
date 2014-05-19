package graph

import util._

class NodeElem (v: String) {
	val value = v 
	var outedges: List[DepEdge] = Nil
	var inedges: List[DepEdge] = Nil
 
 	def neighbors: List[NodeElem] = outedges.map(_.getTarget)
	def outdegree: Int = outedges.length
	def indegree: Int = outedges.length
 
 	def nodesbydepth(seen: Set[NodeElem]): List[NodeElem] = {
			def nodesbydepthR(neighbors: List[NodeElem], s: Set[NodeElem]): List[NodeElem] =
				neighbors match {
				case Nil => Nil
				case n :: tail if s(n) => nodesbydepthR(tail, s) //already traversed
				case n :: tail => {
					val subnodes = n.nodesbydepth(s) // new search 
					subnodes ::: nodesbydepthR(tail, s ++ subnodes)
				}
			}
			nodesbydepthR(neighbors, seen + this) ::: List(this)
	}
 	override def toString = value.toString
 	def toDotLabel = value.toString
}


class DepEdge (src: NodeElem, dst: NodeElem) {
	val edgeSep = ">"
	def toTuple = (src.value, dst.value)
	override def toString = src.value + edgeSep + dst.value
	def getTarget = dst
	def getSource = src
	def toLabel: String = ""
}


class DependencyGraph {
	var nodes: Map[String, NodeElem] = Map()
	var edges = List[DepEdge]()
	
	def addNode(value: String): NodeElem = {
		val tn = new NodeElem(value) 
		nodes = Map(value -> tn) ++ nodes
		tn
	}
	
	def addNode(n: NodeElem): NodeElem = {
		nodes = Map(n.value -> n) ++ nodes
		n
	}
	
 	def getNode(value: String) : NodeElem = 
		if ( nodes.contains(value) ) nodes(value) else null
	
	
	def getEdge(from: String, to: String) : DepEdge = {
		val edges = nodes(from).outedges.filter(_.getTarget == nodes(to))
		var edge: MethodCallEdge = null
		if ( edges.length > 0 ) {
			assert(edges.length == 1)
			edge = edges.head.asInstanceOf[MethodCallEdge]
		}
		else if ( edges.length == 0) {
			edge = new MethodCallEdge(nodes(from), nodes(to))
			addEdge(edge)
		}
		edge
 	}
	
	def addEdge(from: String, to: String ) : DepEdge = {
		if ( !nodes.contains(from) || !nodes.contains(to)) {
			println("CHECK (not added): " + from + ":" + nodes.contains(from) + "->" + to + ":" + nodes.contains(to))
			return null
		}
		
		var edge =  getEdge(from, to)
		if ( edge == null ) {
			edge = new DepEdge(nodes(from), nodes(to))
			addEdge(edge)
		}
		edge 
	}
	
 	protected def addEdge(e: DepEdge): DepEdge = {
	    edges = e :: edges
		e.getSource.outedges = e :: e.getSource.outedges
		e.getTarget.inedges = e :: e.getTarget.inedges
		e
	}
  
	protected def removeEdge(e: DepEdge) = {
	   edges -= e
	   e.getSource.outedges -= e
	   e.getTarget.inedges -= e
	}
 
	def removeEdges(es: List[DepEdge]) = {
		es.foreach(removeEdge(_))
	}
  
	def remove(elems: List[NodeElem]) = {
		var es = List[DepEdge]()
		for ( n <- elems ) {
		  es = n.inedges ::: n.outedges ::: es
		}
		
		nodes --= elems.map(_.value)
		for ( e <- es ) removeEdge(e)
	}
  
 	override def equals(o: Any) = o match {
		case g: DependencyGraph => ( nodes.keys.toList -- g.nodes.keys.toList == Nil && 
				edges.map(_.toTuple) -- g.edges.map(_.toTuple) == Nil )
		case _ => false
	}
  
  	override def toString = {
	    val (edgeStrs, unlinkedNodes) = 
	        edges.foldLeft((Nil: List[String], nodes.values.toList)) ((r, e) => (e.toString :: r._1, r._2.filter((n) => n != e.getSource && n != e.getTarget)))
				
	    "[" + (unlinkedNodes.map(_.value.toString) ::: edgeStrs).mkString(", \n") + "]"
	}
   
   def edgeCountStr(e: ModuleEdge): String = "\"" + e.edgeCount + "\""
   def dotname(filename: String) ={
	   if ( filename.contains("/"))
		   filename.substring(filename.lastIndexOf("/") + 1) 
	   else filename
   }
	

  	val nodestyle = "\tnode [shape=box, fillcolor=white, fontname=GillSans, fontsize=13];\n"
	val edgestyle= "\tedge [fontname=GillSans, fontsize=9];\n"
   def toDotType(dotfilename: String) = {
	     var (nodemap, count) = nodes.values.toList.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))

	     def edgeString(e: DepEdge) : String = (nodemap(e.getSource.toString) + "->" + nodemap(e.getTarget.toString))  + 
	     						(if ( e.toLabel.length > 0 ) " [label=\"" + e.toLabel + "\"]" else "") 
	     		
		 def nodeString(n: NodeElem) = nodemap(n.toString) + "[ label=\"" + n.toDotLabel + "\"]"
      
		 
	     val edgeStrs = edges.foldLeft(Nil: List[String]) ( (r, e) => edgeString(e) :: r )
	     val file = new FileOut
	     file.saveFile(dotfilename + ".dot", 
	                   "digraph " + dotname(dotfilename) + " { \n"  
	                   + nodestyle 
	                   + nodes.values.foldLeft(List[String]()) ((s, v) => (nodeString(v) :: s)).mkString(";\n") + ";\n"
	                   + edgestyle
	                   + edgeStrs.mkString(";\n") + "\n}")
   }
   
   def toDotTypeSubgraph(dotfilename: String) = {
	     var (nodemap, count) = nodes.values.toList.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))

	     def edgeString(e: DepEdge) : String = (nodemap(e.getSource.toString) + "->" + nodemap(e.getTarget.toString))     + 
	     		(if ( e.toLabel.length > 0 ) "[label=\"" + e.toLabel + "\"]" else "") 
	     		
		 def nodeString(n: NodeElem) = nodemap(n.toString) + "[ label=\"" + n.toDotLabel + "\""  + ", style = filled]"
      
	     val edgeStrs = edges.foldLeft(Nil: List[String]) ( (r, e) => edgeString(e) :: r )
	     val file = new FileOut
	     file.saveFile(dotfilename + ".dot", 
	                   "digraph " + dotfilename + " { \n" 
	                   + (nodes.values.toList.foldLeft(Nil: List[String]) ((s, v) => (nodeString(v) :: s))).mkString(";\n") + ";\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
   }


    
   def toDot(dotfilename: String, nodelist: List[NodeElem], nodeColor: NodeElem => String): Unit = {
		 val mns = nodes.values.toList.filter(nodelist.contains(_))
	     var (nodemap, count) = 
	    	 mns.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))

	     def edgeString(e: DepEdge) : String = (nodemap(e.getSource.toString) + "->" + nodemap(e.getTarget.toString)) + 
		       	( if (e.isInstanceOf[ModuleEdge]) ( "[label = " + edgeCountStr(e.asInstanceOf[ModuleEdge]) + "]" ) )
                                              
		 def nodeString(n: NodeElem) = nodemap(n.toString) + "[ label=\"" + n + "\""  + nodeColor(n)  + ", style = filled]"
      
		 def edgefilter(e: DepEdge) = ( nodelist.contains(e.getSource) && nodelist.contains(e.getTarget) ) 
	   
		 val edgeStrs = edges.filter(edgefilter(_)). foldLeft(Nil: List[String]) ( (r, e) => edgeString(e) :: r )
	     val file = new FileOut 
	     file.saveFile(dotfilename + ".dot", 
	                   "digraph " + dotfilename + " { \n" 
	                   + mns.foldLeft(Nil: List[String]) ((s, v) => (nodeString(v) :: s)).mkString(";\n") + ";\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
   }
   
}


