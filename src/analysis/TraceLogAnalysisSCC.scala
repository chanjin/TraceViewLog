package analysis
import project._
import traceview._
import graph._

class TraceLogAnalysisSCC(scenario: Scenario) extends TraceLogAnalysis(scenario){
	
	var CallLabel = Map[Call, Int]()
	
	def analyzeCallsPre = {
		//println("Thread Labels: " + reader.getThreadLabels.size)
   		
   		for ( t <- threads ) 
  			thrMap += (t.getId -> new CallAnalysisThreadWise(t.getId, t, scenario))

		for ( c <- calls ) {
			val call : Call =c.asInstanceOf[Call] 
			val tid = call.getThreadId
			if (! thrMap.contains(tid)) {
				println("not defined tid" + call.getMethodData.getName)
			}
			else {
				thrMap(tid).handleCall(call)
			}
		}
	}
}