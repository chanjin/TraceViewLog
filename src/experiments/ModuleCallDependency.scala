package experiments


import project._
import graph._
import traceview._
import seqdiagram._
import sequitur._

class ModuleCallDependency (s: Scenario) extends TraceLogAnalysisSeq(s)  {
	val filterTid = 19
	def architectureThreadWise() = {
		for (thread <- thrMap.values ) {
			//if (isAppThread(thread) ) {
			if (thread.tid == filterTid ) {
				println(thread.calltrace.reverse)
				println(thread.callId)
			
				val id2mpair = thread.mpair2id.map(entry => (entry._2-> entry._1))
				
				println(id2mpair.get(1))
				println(id2mpair.get(2))
				
				val s = new Sequitur
				val res = s.runSequiturResult(thread.calltrace.reverse)
		        val compactSeq = new CompactSeq(Map[Rule, List[Symbol]]() ++ res)
		
		        //println(res.map(e => (e._1.count + ": R" + e._1.index + "->" + e._2.map(s => symval(s)).reverse)).mkString("\n"))
		        println(compactSeq.getRulesString.mkString("\n"))
		        println("-----")
		        println(compactSeq.getFirstRuleString(s.firstRule))
		        println("---")
		        val cl = compactSeq.getRLEListString(s.firstRule)
		        println(cl.mkString("\n"))
		        println(cl.size + ":" + thread.calltrace.size + ":" + compactSeq.newRes(List(s.firstRule)).size)
			}
		}
	}
	
	def run = {
		println("Module Call Dependency - " + getProject.getName)
		analyzeCalls(Architecture.getModuleOfClass("snsonly"))
		
		var appCalls = 0
		for ( c <- calls ) 	
			if ( c.asInstanceOf[Call].getMethodData.getClassName.startsWith("com/lge") ) appCalls += 1
			
		println("# of App calls - " + appCalls + " (" + calls.size + ")")
		println("# of App methods - " + methods.filter(_.getClassName.startsWith("com/lge")).length + " (" + methods.length + ")")
		println("tids of app threads - " + thrMap.values.toList.filter(t => isAppThread(t)).map(_.tid).sort(_ < _).mkString(", "))
		println("tids of all threads - " + thrMap.values.toList.map(_.tid).sort(_ < _).mkString(", "))
		architectureThreadWise
	}
	
}

object ModuleCallDependency  {
	def main(args: Array[String]) = {
		(new ModuleCallDependency(SNSLogin)).run
	}
}