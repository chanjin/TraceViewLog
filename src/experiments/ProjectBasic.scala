package experiments
import analysis._
import util._
import traceview._
import project._
 
class ProjectBasicAnalysis(s: Scenario) extends TraceLogAnalysis(s) {	
	def basicInfo(maparchftn: String=>String)= {
		println()
		println("***** Project - " + s.getName)
		println("# of threads: " + threads.length)
		println("# of calls: " + calls.length)
		println("# of methods:" + methods.length)
  		println("# of classes: " + methods.map(_.getClassName).removeDuplicates.length)
  		println( "# of packages: " + methods.map(m =>  TraceUtil.getPackageName(m.getClassName)).removeDuplicates.length)
  		
  		println("methods - " + total)
  		println("methods - " + methods.take(10).mkString("\n"))
  		var pkg2mds =	Map[String, List[MethodData]]()
		methods.remove(_ == topLevelMethod).foreach(m => {
			val mod = maparchftn(m.getClassName)
			pkg2mds += ( mod -> (m :: pkg2mds.getOrElse(mod, List[MethodData]())))
		})
		var results = List[String]()
		def methodInfo(ms: List[MethodData]) = {
			val (time, freq) = ms.foldLeft(0L, 0) ((sum, m) => (sum._1 + m.getElapsedExclusive, sum._2  +m.getTotalCalls))
			"time elapsed: " +  getTimeStr(time, totalExceptTop) + ", frequency: " + freq
		}
		pkg2mds.foreach(pms => {
			results = (pms._1 + "\t" + methodInfo(pms._2) + ",#m - " + pms._2.length + ", #class - " + pms._2.flatMap(m => m.getClassName).removeDuplicates.length) :: results
		})
		println(results.sort(_ < _ ).mkString("\n"))
		
		println("package\ttime elapsed(sec)\tpercentage\tfrequency\t# of methods\t# of classes")
		pkg2mds.foreach(pms => {
			val (time, freq) = pms._2.foldLeft(0L, 0) ((sum, m) => (sum._1 + m.getElapsedExclusive, sum._2  +m.getTotalCalls))
			println(pms._1 + "\t" + nfnum.format(time.asInstanceOf[Double]/1000000.0) + "\t" + percentage(time, totalExceptTop) + "\t" + freq + "\t" + pms._2.length + "\t" + pms._2.flatMap(m => m.getClassName).removeDuplicates.length )
		})
		println("-----")
		
		analyzeCalls
		val archgraph = createArchCallGraphFrom(createMethodCallGraph, maparchftn)
		archgraph.toDotType(ProjectConf.outputdir  +  "/" + s.getName+ "Basic")
	}
	
	def run = {
		basicInfo(Architecture.getModuleOfClass("packagearch"))
		println("----------------------")
		basicInfo(Architecture.getModuleOfClass("snsonly"))
	}
	
}

object ProjectBasic  {
	def main(args: Array[String]) = {
		println("Get Project Basic Information")
		// (new ProjectBasicAnalysis(SNSLogin)).run
		SNSCases.getScenarios.foreach( scenario => (new ProjectBasicAnalysis(scenario)).run )
	}
}