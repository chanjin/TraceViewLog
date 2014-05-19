package experiments

import analysis._
import project._
import graph._
import util._

class APInUpcallsAnalysis (s: Scenario) extends TraceLogAnalysis(s) {
	def maparchftn(clsname : String ) = {
		if ( clsname.startsWith("com/lge")) "SNS"
		else "LIB"
	}
	
	def edgeStr(e: DepEdge) = {
		val target = e.getTarget.asInstanceOf[MethodNode]
		def toMethodName(e: DepEdge) = {
			 val src = e.getSource.asInstanceOf[MethodNode]
			 val dst = e.getTarget.asInstanceOf[MethodNode]
			 TraceUtil.getPackageName(dst.getClassName) + "\t" + src.mdata.getMethodName + "\t" + src.mdata.getClassName + "\t" + 
			 dst.mdata.getMethodName + "\t" +  dst.mdata.getClassName 
		}
		toMethodName(e)
	}
	
	def run = {
		analyzeCalls
		val archgraph = createArchCallGraphFrom(createMethodCallGraph, maparchftn)
		val edgesStr = archgraph.edges.map( e => e + ": " + e.asInstanceOf[ModuleEdge].getMemberEdges.length +
				"\n" + e.asInstanceOf[ModuleEdge].getMemberEdges.map(me => edgeStr(me)).mkString("\n"))
		val targetClasses = archgraph.edges.map( e => e.asInstanceOf[ModuleEdge].getMemberEdges.map(me => me.getTarget.asInstanceOf[MethodNode].mdata.getClassName).removeDuplicates.mkString("\n"))
		println(targetClasses.mkString("\n\n"))
		
		println(edgesStr.mkString("\n ------------ \n"))
	}
}


object APInUpcalls {
	def main(args: Array[String]) = {
		(new APInUpcallsAnalysis(SNSLogin)).run
		//(new APInUpcallsAnalysis(SNSRefreshTweet)).run
		//(new APInUpcallsAnalysis(SNSMakeTweet)).run
		//(new APInUpcallsAnalysis(SNSLogout)).run
	}
}