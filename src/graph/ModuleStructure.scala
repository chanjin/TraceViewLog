package graph

class ModuleNode(qn: String) extends NodeElem(qn) {
	var members: List[NodeElem] = Nil
	def addMember(t: NodeElem) {
	  	members = t :: members
	  	this
	}
 	override def toString = value + ", " + members.length
}

class ModuleEdge(m1: ModuleNode, m2: ModuleNode) extends MethodCallEdge(m1, m2) {
	  private var memberEdges: List[DepEdge] = Nil
	  def addTypeEdge(e : DepEdge ) = {
	 	  memberEdges = e :: memberEdges
	 	  setLabel(e.asInstanceOf[MethodCallEdge].labels)
	  }
	  def getMemberEdges = memberEdges
	  def edgeCount = memberEdges.length
	  
	  def setLabel(l: List[Int]) = { labels = l ::: labels }
}

class ModuleStructure(g: DependencyGraph) extends MethodCallGraph 
{
  	var memberGraph = g
  	var mapT2M: Map[NodeElem, ModuleNode] = Map()
   
   def addMember(module: ModuleNode, tstr: String) : ModuleNode = {
		  var tn = memberGraph.nodes(tstr).asInstanceOf[NodeElem]
		  mapT2M += tn -> module
		  module.addMember(tn)
		  module
   }
 
	def addModuleNode(m: ModuleNode, ts: List[NodeElem]) = { // assume types are already included
		  for ( t <- ts ) {
			  mapT2M += ( t -> m)
		  }
		  m
	}
	
 
  	def liftEdges() = {
  		for ( module <- nodes.values.toList ) {
  			val m1 = module.asInstanceOf[ModuleNode]
            var medges: Map[ModuleNode, ModuleEdge] = Map()
            val outEdges: List[DepEdge] = ( m1.members.toList flatMap (_.outedges) ) 
            for ( e <- outEdges ) {
            	 var m2: ModuleNode = mapT2M(e.getTarget.asInstanceOf[NodeElem])
            	 if ( m1 != m2 ) {
            		 if ( !medges.contains(m2) ) {
            			 medges += (m2 -> (addEdge(new ModuleEdge(m1, m2))).asInstanceOf[ModuleEdge]) 
            		 }
            		 medges(m2).addTypeEdge(e)
            	 }
             }
  		}
  	}
	
	override def adjustEdgeLabels = {
		relabel
		adjustEdgeLabel
		relabel
	}
	
  
  	override def removeEdges(es: List[DepEdge]) = {
 	  	super.removeEdges(es)
		memberGraph.removeEdges(es.flatMap(_.asInstanceOf[ModuleEdge].getMemberEdges))
	}
 
  	def retainPackages(prefixes: List[String]): Unit = {
		def hasPrefix(pname: String): Boolean = prefixes.exists(pname.startsWith(_))
		val ps = nodes.values.toList.filter(p => !hasPrefix(p.value))
		remove(ps)
		memberGraph.remove(ps.flatMap(_.asInstanceOf[ModuleNode].members))
	}
	
	def getNodesWithPrefix(prefixes: List[String]) : List[NodeElem]= {
		def hasPrefix(pname: String): Boolean = prefixes.exists(pname.startsWith(_))
		nodes.values.toList.filter(p => hasPrefix(p.value))
	}
	
	
}
