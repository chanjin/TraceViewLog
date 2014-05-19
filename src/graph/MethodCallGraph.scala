package graph

import traceview._
import util._

class MethodNode(md: MethodData) extends NodeElem(md.getId.toString) {
	val mdata = md	
	override def toDotLabel = mdata.getMethodName
	def toMethodName = mdata.getMethodName + "(" + mdata.getClassName + ")"
	def getClassName = mdata.getClassName
}

class ClassNode(name: String) extends ModuleNode(name) {
	override def toDotLabel = {
		val index = name.lastIndexOf('/')
		val label = (if ( index > 0 )	name.substring(index+1) else name) //+ " [" + members.map(_.asInstanceOf[MethodNode].toDotLabel).mkString(",") + "]"
		label
	}
}

class PackageNode(name: String) extends ClassNode(name) { 
	override def toDotLabel = name
}

class Order(major: Int, minor: Int) extends Comparable[Order] {
	def getMajor = major
	def getMinor = minor
	override def compareTo(order : Order) : Int = {
		if ( major > order.getMajor )  1
		else if ( major == order.getMajor ) {
			if ( minor > order.getMajor) 1
			else if ( minor == order.getMajor ) 0
			else -1
		}
		else -1
	}
	
	override def toString = major + "-" + minor
}

class MethodCallEdge (src: NodeElem, dst: NodeElem) extends DepEdge  (src, dst) {
	var labels = List[Int]()
	def setLabel(l: Int) = { labels = l :: labels }
	var limit = 50
	def setLimit(l: Int) = limit = l
	
	override def toLabel = {//if ( labels.length < 5)  labels.reverse.mkString(",") else labels.reverse.take(5).mkString(",")
		// labels.filter(_ < limit).sort(_ < _).mkString(",")
		val labelsSorted = labels.sort(_<_)
		if ( labels.length < 4 ) labelsSorted.mkString("-") else labelsSorted.take(3).mkString("-") + "-..." + labelsSorted.last + "(" + labels.length + ")"
	}
	
	
	def reduceLabels = {
		val labelsSorted: List[Int] = labels.sort((e1, e2) => e1 < e2) 
		val (r, count) = labelsSorted.tail.foldLeft((List(labelsSorted.head), labelsSorted.head))((result: (List[Int], Int), elt) => (if (result._2 + 1 < elt ) ( elt :: result._1, elt)  else (result._1, elt)))
		labels = r
	}
}

class MethodCallGraph extends DependencyGraph {
	
	private def checkNodesOfEdge(from: String, to: String) : Boolean = {
		if ( !nodes.contains(from) || !nodes.contains(to)) {
			println("CHECK (not added): " + from + ":" + nodes.contains(from) + "->" + to + ":" + nodes.contains(to))
			return false
		}
		return true
	}

	private def createEdge(from: String, to: String) : MethodCallEdge = {
		var edge =  getEdge(from, to).asInstanceOf[MethodCallEdge]
		if ( edge == null ) {
			edge = new MethodCallEdge(nodes(from), nodes(to))
			addEdge(edge)
		}
		edge
	}
	
	def addEdge(from: String, to: String, label: Int) : MethodCallEdge = {
		if (!checkNodesOfEdge(from, to)) return null
		val e = createEdge(from, to)
		e.setLabel(label)
		e
	}
	
	def addEdgeNoLabel(from: String, to: String) : MethodCallEdge = {
		if (!checkNodesOfEdge(from, to)) return null	
		val e = createEdge(from, to)
		e
	}
	

	def adjustEdgeLabels = {
		adjustEdgeLabel
		relabel
	}
	
	protected def adjustEdgeLabel = 
		edges.foreach(_.asInstanceOf[MethodCallEdge].reduceLabels)
		
	protected def relabel = {
		val l2e = edges.foldLeft(Map[Int, MethodCallEdge]()) ( (m, e) => e.asInstanceOf[MethodCallEdge].labels.foldLeft(m)((m, l) => m + (l -> e.asInstanceOf[MethodCallEdge])))
		val keylist = l2e.keys.toList.sort(_ < _)
		var count = 0
		l2e.values.foreach(_.labels = List[Int]())
		for ( k <- keylist) {
			l2e(k).setLabel(count)
			count += 1
		}
	}
	
	/*def toDotLayer(dotfilename: String, layermap: Map[String, Int]) = {
		def ancestors = nodes.values.toList.filter(n => n.inedges.length == 0)
		
		def layernode(n: Int) = "\"layer-" + n + "\""
		def ranks = {
			var l2n =  Map[Int, List[String]]()
			layermap.map(_.swap).foreach(entry => l2n += (entry._1 -> (entry._2 :: l2n.getOrElse(entry._1, List[String]()))))
			l2n.map(entry => "rank = same;" + layernode(entry._1) + "; " + )
		}
			
		def layerstmt = {
			val layers = layermap.values.toList.sort(_>_)
			layers.map(layernode(_)).mkString(" -> ")
		}
		
		var (nodemap, count) = nodes.values.toList.foldLeft((scala.collection.mutable.Map[String, Int](), 0)) ((m, node) => ((m._1 + (node.toString -> m._2)), m._2 + 1))
		def nodeString(n: NodeElem) = nodemap(n.toString) + "[ label=\"" + n.toDotLabel + "\""  + ", style = filled]"
	
		val layersection = "\t{\n \t\tnode[shape=plaintext]; \n\t\t" + layerstmt + ";\n\t\t" +  ancestors.map(n => nodeString(n)).mkString(";\n\t\t") + "}"
		
	    

	    def edgeString(e: DepEdge) : String = (nodemap(e.getSource.toString) + "->" + nodemap(e.getTarget.toString))  + 
	     						(if ( e.toLabel.length > 0 ) " [label=\"" + e.toLabel + "\"]" else "") 
	     		
		
      
	     val edgeStrs = edges.foldLeft(Nil: List[String]) ( (r, e) => edgeString(e) :: r )
	    
	     val file = new FileOut
	     file.saveFile(dotfilename + ".dot", 
	                   "digraph " + dotname(dotfilename) + " { \n" 
	                   + nodes.values.toList.remove(n => ancestors.contains(n)).foldLeft(Nil: List[String]) ((s, v) => (nodeString(v) :: s)).mkString(";\n") + ";\n"
	                   + edgeStrs.mkString(";\n") + "\n}")
   }*/
}
