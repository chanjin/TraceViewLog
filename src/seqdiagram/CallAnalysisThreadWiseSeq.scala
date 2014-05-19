package seqdiagram

import traceview._
import util._

class CallAnalysisThreadWiseSeq(thrId: Int, filterCall: (MethodData, MethodData) => Boolean) {
	// intermodule call in architecture
	def tid = thrId
	var callstack  = new scala.collection.mutable.Stack[Call]
	var callId = 1 // alphabets
	val callEnd = 0 //Int.MinValue
	var calltrace = List[Int]()
	var mpair2id = Map[(MethodData, MethodData), Int]() 

    def handleCall(call: Call): Unit = {

        val md = call.getMethodData
        //println(tid + " : " + md.getMethodName)
        if (md.getId == -1) {
            /*if ( callstack.length > 0 ) 
            	println("thread switched " + TraceUtil.callString(call) + " - on statck " + TraceUtil.callString(callstack.top))*/
            return
        }

        val pkg = TraceUtil.getPackageName(md.getClassName)

        if (call.getMethodAction() == 0) { // enter		
            if (callstack.length > 0) {
                // add call edge
                val caller = callstack.top.getMethodData
                if ( filterCall(caller, md)) {
                	val callpair = (caller, md)
                	if ( !mpair2id.contains(callpair)) {
                		mpair2id += (callpair -> callId)
                		calltrace = callId :: calltrace
                	    callId = callId + 1	
                	}
                	else {
                		calltrace = mpair2id(callpair) :: calltrace
                	}
                }
            }

            callstack.push(call)
        } else { // exit
            if (callstack.length == 0) {
                println("Return with no stack: " + md.getName)
                return
            }
            val c = callstack.pop
            if (callstack.length > 0 && filterCall(callstack.top.getMethodData, c.getMethodData)) {
            	val caller = callstack.top.getMethodData
            	val callee = c.getMethodData
            	if ( !mpair2id.contains((caller, callee))) println("ERROR - " + caller + "->" + callee)
            	else {
            		calltrace = (mpair2id((caller, callee)) * -1) :: calltrace // call end mark
            	}
            }
        }
    }
}