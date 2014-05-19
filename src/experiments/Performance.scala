package experiments


import analysis._
import util._
import graph._
import traceview._
import project._

class PerformanceAnalysis(s: Scenario) extends TraceLogAnalysis(s) {
	val packages: List[String] = methods.map(md => TraceUtil.getPackageName(md.getClassName)).removeDuplicates


	def outputFile = {
		val headers = List("Package", "Class", "Method", "TimeElapsed", "Freq", "%")
	}

	def analysisScenario(methodList: List[MethodData]) = {
		println("\n*** Method Top 10 in Scenario " + s.getName + " (Exclusive time) " + total)
		println(methodList.sort((m1, m2) => m1.getElapsedExclusive > m2.getElapsedExclusive).take(10).map(m => 
			getTimeStr(m.getElapsedExclusive, total) + "\t:" + getMethodStr(m)).mkString("\n"))
		
		println("\n*** Method Top 25 in Scenario " + s.getName + " (Inclusive time) ")
		println(methodList.sort((m1, m2) => m1.getElapsedInclusive > m2.getElapsedInclusive).take(25).map(m => 
			getTimeStr(m.getElapsedInclusive, total) + "\t:" + getMethodStr(m)).mkString("\n"))

	}
	
	
	def analysisScenarioByModules(archmap: String => String, methodList: List[MethodData]) = {
		def totalm = methodList.foldLeft(0L)((sum, m) => sum + m.getElapsedExclusive)
		var module2ms = Map[String, List[MethodData]]() // module -> List[method]
		for ( m <- methodList) {
			val mod: String = archmap(m.getClassName)
			module2ms += ( mod -> (m :: module2ms.getOrElse(mod, List[MethodData]())))
		}
		
		def getElapsedTime(ms: List[MethodData]): Long = ms.foldLeft(0L)((sum, m) => sum + m.getElapsedExclusive) 
		val results: List[(Long, String)] = module2ms.foldLeft(List[(Long, String)]())((list, mm) => (getElapsedTime(mm._2), mm._1) :: list)
		
		
		println("*** Time elapsed by layers")
		println(results.sort(_._1 > _._1).map( mm => "(" + mm._1 + ", " + percentage(mm._1, totalm) + "%) : " + mm._2 ).mkString("\n")) 
	}
	
	def analysisThreadWise() = {
		var methodTimeByThread = Map[MethodData, Map[Int, Long]]() // method -> (tid -> time)
		for ( c <- calls) {
			val call = c.asInstanceOf[Call]
			val method = call.getMethodData
			val tid = call.getThreadId
			
			var tidTime = methodTimeByThread.getOrElse(method,  Map[Int, Long]())
			tidTime += ( tid -> (call.mTimeExclusive + tidTime.getOrElse(tid, 0L) ) )
			methodTimeByThread += (method -> tidTime)
		}
		
		
		println("\n" + methods.length + ":" + methodTimeByThread.size)
		println("thread methods - methods")
		var diff = methodTimeByThread.keys.toList.remove(m => methods.contains(m))
		println(diff.length + " : " + diff.foldLeft(0L)((sum, m) => sum + m.getElapsedExclusive) + " : "+ diff.map(m => if (m.getMethodName == null) m.getName else m.getMethodName).mkString(","))
		println("methods - thread methods")
		diff = methods.remove(m => methodTimeByThread.keys.toList.contains(m))
		println(diff.length + " : " + diff.foldLeft(0L)((sum, m) => sum + m.getElapsedExclusive) + " : "+ diff.map(m =>if (m.getMethodName == null) m.getName else m.getMethodName).mkString(","))
		
		
		var threadTime = Map[Int, List[(MethodData, Long)]]() // tid -> List(method, time)
		for ( entry <- methodTimeByThread) {
			val m = entry._1
			for ( tt <- entry._2 )
				threadTime += (tt._1 -> ((m, tt._2) :: threadTime.getOrElse(tt._1, List[(MethodData, Long)]())))
		}

		var threadTotal = Map[Int, Long]() //tid -> time
		
		var totals = Map[Int, Long]() // tid -> time
		for ( t <- threadTime ) {
			val total = t._2.foldLeft(0L)((t, mt) => t + mt._2)
			totals += (t._1 -> total)
		}
		
		val sumOfTotals: Long = totals.values.foldLeft(0L)((sum, t) => sum + t) 
		println("\n*** Thread performance data - " + sumOfTotals + "(thread creation/cleanup -" + + topLevelMethod.getElapsedExclusive + "): " + threadTime.keys.toList.mkString(",") )
		
		def threadName(tid: Int) = threads.filter(thr => (thr.getId == tid)).head
		for ( t <-  threadTime) {
			def methodTimeStr(mt: (MethodData, Long)) = getTimeStr(mt._2, totals(t._1)) + "\t:" + getMethodStr(mt._1)
			println("\n*** Method Top 10 in Thread, " + threadName(t._1) + " - " + totals(t._1) + "(" + percentage(totals(t._1), sumOfTotals) + "%)") // t._1 => tid
			println(t._2.sort(_._2 > _._2).take(10).map( mt => methodTimeStr(mt)).mkString("\n"))
			println("-------- App")
			println(t._2.filter(mt => isAppClass(mt._1.getClassName)).sort(_._2 > _._2).take(10).map(mt => methodTimeStr(mt)).mkString("\n"))
			println("***")
		}
	}

	def run = {
		analysisScenario(methods)
		println("--------------- App focused")
		analysisScenario(methods.filter(m => isAppClass(m.getClassName)))
		println("--------------- Layer - SNS only")
		analysisScenarioByModules(Architecture.getModuleOfClass("snsonly"), methods.remove(_ == topLevelMethod))
		println("--------------- Layer - Layer")
		analysisScenarioByModules(Architecture.getModuleOfClass("layers"), methods.remove(_ == topLevelMethod))
		println("--------------- Layer - Component")
		analysisScenarioByModules(Architecture.getModuleOfClass("sns"), methods.remove(_ == topLevelMethod))
		
		analysisThreadWise
	}
}

object Performance  {
	def main(args: Array[String]) = {
		println("Performance data")
		(new PerformanceAnalysis(SNSLogin)).run
	}
}