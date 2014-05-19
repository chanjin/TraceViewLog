package experiments

import analysis._
import project._
import traceview._

class ThreadAnalysis(s: Scenario) extends TraceLogAnalysis(s)  {
	def getThreadName(tid: Int) = threads.filter(_.getId == tid).head.getName
	
	def getMapTid2Calls: Map[Int, List[Call]] = {
		var tid2calls = Map[Int, List[Call]]() // tid -> calls
		for ( c <- calls ) {
			val call = c.asInstanceOf[Call]
			val tid = call.getThreadId
			tid2calls += ( tid -> (call :: (tid2calls.getOrElse(tid, List[Call]()))) ) 
		}
		tid2calls
	}
	
	def run = {
		val tids = threads.map(_.getId)
		var tid2calls = getMapTid2Calls
		// start time
		val tid2callsApp = tid2calls.filter(tc => tc._2.exists(c => isAppClass(c.getMethodData.getClassName)))
		var threadInits=  List[Call]()
		tid2callsApp.foreach(tc => {
			val callsSorted = tc._2.sort( (c1, c2) => c1.getStartTime < c2.getStartTime)
			threadInits = callsSorted.head :: threadInits
			println(getThreadName(tc._1) + " : " + callsSorted.head.getStartTime + ", " + callsSorted.last.getEndTime)
		})
		threadInits.sort( (c1, c2) => c1.getStartTime < c2.getStartTime)
		var callsDivided = List[List[Call]]()
		////////
		
		val tid2methods = tid2callsApp.map( tc => {
			tc._1 ->( tc._2.map(_.getMethodData).removeDuplicates )
		})
		println(tid2methods.map(tc => {
			tc._1 + " : " + tc._2.length
		}).mkString("\n"))
		
	}
}

object ThreadWiseCall {
	def main(args: Array[String]) = {
		(new ThreadAnalysis(SNSLogin)).run
	}
}