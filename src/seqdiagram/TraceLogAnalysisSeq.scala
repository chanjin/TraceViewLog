package seqdiagram
import project._
import traceview._

class TraceLogAnalysisSeq(scenario: Scenario) {
    def getProject = scenario
    val reader = getProject.getTraceReader

    val methods = List.fromArray(reader.getMethods)
    val threads = List.fromArray(reader.getThreads)
    val calls: Array[Object] = reader.getCalls.toArray
    var thrMap = scala.collection.mutable.Map[Int, CallAnalysisThreadWiseSeq]()

    val topLevelMethod = reader.getTopLevelMethod

    protected def isAppClass(clsname: String): Boolean =
    	getProject.getPackagePrefixes.exists(pp => clsname.startsWith(pp))
    	
    protected def isAppThread(thr: CallAnalysisThreadWiseSeq) : Boolean= {
    	thr.mpair2id.keys.foreach( mp => 
    		if ( isAppClass(mp._1.getClassName) || isAppClass(mp._2.getClassName) ) return true )
		false
	}

    def getMethodStr(m: MethodData) = 
    	if (m.getMethodName == null) m.getName else m.getMethodName + " (" + m.getClassName + ")"

    def analyzeCalls(cls2module: String => String) = {

    	def filterCalls(m1: MethodData, m2: MethodData): Boolean = {
    		cls2module(m1.getClassName) != cls2module(m2.getClassName)
    	}
    	
        for (t <- threads) {
            thrMap += (t.getId -> new CallAnalysisThreadWiseSeq(t.getId, filterCalls));
            println(t.getName)
            println(t)
        }

        // later 
        //for (c <- calls.take(100)) {
        for (c <- calls) {
            val call: Call = c.asInstanceOf[Call]
            val tid = call.getThreadId
            if (!thrMap.contains(tid)) {
                println("not defined tid" + call.getMethodData.getName)
            } else {
                thrMap(tid).handleCall(call)
            }
        }
    }

}
