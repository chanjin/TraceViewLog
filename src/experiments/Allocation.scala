package experiments

import analysis._
import project._
import graph._
import util._
import traceview._

class AllocationAnalysis (s: Scenario) extends TraceLogAnalysis(s) {
	def run = {
		println(methods.length)
		val methodAlloc: List[MethodData] = methods.remove(_ == topLevelMethod).filter(_.getMethodName.startsWith("<"))
		println( methodAlloc.map(m => (m.getTotalCalls + "\t" + m.getMethodName + "\t" + m.getClassName)).mkString("\n"))
	}
}

object Allocation {
	def main(args: Array[String]) = {
		(new AllocationAnalysis(SNSLogin)).run
		//(new APInUpcallsAnalysis(SNSRefreshTweet)).run
		//(new APInUpcallsAnalysis(SNSMakeTweet)).run
		//(new APInUpcallsAnalysis(SNSLogout)).run
	}
}