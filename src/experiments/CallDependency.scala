package experiments
import analysis._
import project._
import graph._
import traceview._


class CallDependencyAnalysis(s: Scenario) extends TraceLogAnalysis(s)  {
	
	def architectureNoOrdering(maparchftn: String=>String, dotname: String) = {
		//println(components.mkString("\n"))

		val archgraph = createArchCallGraphFrom(createMethodCallGraph, maparchftn)
		archgraph.toDotType(ProjectConf.outputdir  + "/" + s.getName+ "/" + dotname )
		
		// TEST => *
		/*val froms = archgraph.getNode("TEST").asInstanceOf[ModuleNode].members
		println("TEST -> .... ")
		println(froms.flatMap(_.outedges.map(_.getTarget.asInstanceOf[MethodNode].mdata.getName)).mkString("\n"))*/
		
		// ANDROID => SNS-Account
		/*val fromsAndroid = archgraph.getNode("ANDROID").asInstanceOf[ModuleNode].members
		val tosAccount = archgraph.getNode("SNS-Account").asInstanceOf[ModuleNode].members
		println("ANDROID => SNS-Account")
		println(fromsAndroid.flatMap(_.outedges.map(_.getTarget).filter(tosAccount.contains(_)).map(_.asInstanceOf[MethodNode].mdata.getName)).mkString("\n"))
		*/
	}
	


	def architectureThreadWise( maparchftn: String=>String, dotname: String) = {
		for (thread <- thrMap ) {
			if (isAppThread(thread._2) ) {
				val archgraph = createArchCallGraphFrom(thread._2.methodCallGraph, maparchftn)
				archgraph.adjustEdgeLabels
				archgraph.toDotType(ProjectConf.outputdir  + "/" + s.getName + "/" + dotname + "_thread_" +  thread._1)
				println("App related thread - " + thread._1 + ", max call stack depth - " + thread._2.maxDepth )
				println(thread._2.topMethods.mkString("\n"))
				println(thread._2.topMethods.length + ", " + thread._2.majorOrder + ", " + thread._2.numOfCalls)
				println(thread._2.topMethods1.mkString("\n"))
				println(thread._2.topMethods1.length + ", " + thread._2.majorOrder1 + ", " + thread._2.numOfCalls)
			}
		}
	}
	
	def run = {
		println("Call Dependency - " + s.getName)
		analyzeCalls
		
		var appCalls = 0
		for ( c <- calls ) 	if ( c.asInstanceOf[Call].getMethodData.getClassName.startsWith("com/lge") ) appCalls += 1
			
		println("# of App calls - " + appCalls)
		println("# of App methods - " + methods.filter(_.getClassName.startsWith("com/lge")).length)
		println("tids of app threads - " + thrMap.values.toList.filter(t => isAppThread(t)).map(_.tid).sort(_ < _).mkString(", "))
		
		
		architectureNoOrdering(Architecture.getModuleOfClass("architecture"), "ArchCallGraph")
		architectureNoOrdering(Architecture.getModuleOfClass("layerstructure"), "LayerCallGraph")
		architectureNoOrdering(Architecture.getModuleOfClass("snsonly"), "AppCallGraph")
		architectureNoOrdering(Architecture.getModuleOfClass("packagearch"), "PackageCallGraph")
		architectureNoOrdering(Architecture.getModuleOfClass("shlee"), "SHLee")
		
		architectureThreadWise(Architecture.getModuleOfClass("architecture"), "ArchCallGraph")
		architectureThreadWise(Architecture.getModuleOfClass("layerstructure"), "LayerCallGraph")
		architectureThreadWise(Architecture.getModuleOfClass("snsonly"), "AppCallGraph")
		architectureThreadWise(Architecture.getModuleOfClass("packagearch"), "PackageCallGraph")
		architectureThreadWise(Architecture.getModuleOfClass("shlee"), "SHLee")
	}
	
}

object CallDependency  {
	
	def main(args: Array[String]) = {
		(new CallDependencyAnalysis(SNSLogin)).run
		(new CallDependencyAnalysis(SNSRefreshTweet)).run
		(new CallDependencyAnalysis(SNSMakeTweet)).run
		(new CallDependencyAnalysis(SNSLogout)).run
	}
}