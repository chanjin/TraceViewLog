package analysis
 
import project._
import graph._
import util._


import traceview._

class CallAnalysisThreadWise(id: Int, data: ThreadData, project: Scenario) {
	val (tid, tdata) = (id, data)
	var callstack  = new scala.collection.mutable.Stack[Call]
	var callOrder = 0  	     
	var methodCallGraph = new MethodCallGraph
	
	var maxDepth = 0
	var majorOrder = 0
	var majorOrder1 = 0
	var minorOrder = 0
	var topMethods = List[MethodData]()
	var topMethods1 = List[MethodData]()
	var numOfCalls = 0
	
	def packagesInvolved = {
		methodCallGraph.nodes.values.toList.map(mn => 
			TraceUtil.getPackageName(mn.asInstanceOf[MethodNode].mdata.getClassName)).removeDuplicates
	}
	
	// handle each call
	def handleCall(call: Call) : Unit= {
  		//println(tid + ": " + call.getMethodData.getMethodName) 
		val md = call.getMethodData
		if ( md.getId == -1 ) { 
			//if ( callstack.length > 0 ) println("thread switched " + TraceUtil.callString(call) + " - on statck " + TraceUtil.callString(callstack.top))
			return 
		}
		
		val pkg = TraceUtil.getPackageName(md.getClassName)

		// check if method node exists
		if ( methodCallGraph.getNode(md.getId.toString)  == null ) 
  			methodCallGraph.addNode(new MethodNode(md)).asInstanceOf[MethodNode]	                                                               
		//if ( callstack.length == 0 ) println("Top method " + callString(call))
		
  		if (call.getMethodAction() == 0) { // enter		
  			if ( callstack.length > 0 ) {
  				// add call edge
	  			val caller = callstack.top
	  			methodCallGraph.addEdge(caller.getMethodData.getId.toString, md.getId.toString, callOrder)
	  			callOrder += 1
  			}
  			
  			minorOrder = if ( callstack.length == 0)  0 else minorOrder + 1
  			numOfCalls += 1
  			
  			callstack.push(call)
  			
  			if ( callstack.length > maxDepth ) maxDepth = callstack.length
  			//println("\tEnter - mid is  "+ call.getMethodData.getId + " : " + callstack.length )
  		}
  		else { // exit
  			//println("\tExit - mid is "+ call.getMethodData.getId + " : " + callstack.length)
  			if ( callstack.length == 0 ) {
  				println("Return with no stack: " + md.getName)
  				return
  			}
  			val c = callstack.pop
  			if ( callstack.length == 0 ) {
  				majorOrder += 1
  				if ( !topMethods.contains(c.getMethodData) ) topMethods = c.getMethodData :: topMethods
  			}
  			
  			if ( callstack.length == 1 ) {
  				majorOrder1 += 1
  				if ( !topMethods1.contains(c.getMethodData) ) topMethods1 = c.getMethodData :: topMethods1	
  			}
  			
  			if ( md.getName != c.getMethodData.getName ) {
  				println("CHECK stack: " + md.getName + ", " +  c.getMethodData.getName)
		 	}
  		}
  	}
	
	def toDotGraph () : Unit= {
		val mcgraph = methodCallGraph
		if (mcgraph.nodes.size == 0 ) return
		mcgraph.adjustEdgeLabels
		mcgraph.toDotType(ProjectConf.outputdir + "/MethodCallGraph" + tid)
		
		val (classG, packageG) = makeModuleGraphs(mcgraph)
		classG.toDotType(ProjectConf.outputdir + "/ClassCallGraph" + tid)
		packageG.toDotType(ProjectConf.outputdir + "/PackageCallGraph"+ tid)
	}
	
	def makeModuleGraphs (dg: DependencyGraph): (ModuleStructure, ModuleStructure) = {
		var classgraph = new ModuleStructure(dg)
		var packagegraph = new ModuleStructure(dg)
		for ( n <- dg.nodes.values) {
			val mn = n.asInstanceOf[MethodNode]
			var clsnode  = classgraph.getNode(mn.mdata.getClassName)
			if  ( clsnode == null )  clsnode = classgraph.addNode(new ClassNode(mn.mdata.getClassName))
			 classgraph.addMember(clsnode.asInstanceOf[ModuleNode], mn.value)
			 
			val pkgname = TraceUtil.getPackageName(mn.mdata.getClassName)
			var pkgnode = packagegraph.getNode(pkgname)
			if ( pkgnode == null) pkgnode = packagegraph.addNode(new PackageNode(pkgname))
			packagegraph.addMember(pkgnode.asInstanceOf[ModuleNode], mn.value)
		}
		
		classgraph.liftEdges()
		classgraph.adjustEdgeLabels 		

  		packagegraph.liftEdges
  		packagegraph.adjustEdgeLabels
		
		(classgraph, packagegraph)
	}
}
