package experiments
import analysis._
import project._
import traceview._
import util._

class LabelAanlysisThreadWise(id: Int) {
	val tid = id
	var callstack  = new scala.collection.mutable.Stack[Call]
	var call2label = Map[Call, Int]()
	
	def handleCall(call: Call): Unit = {		
		val md = call.getMethodData
		var callOrder = 0
		if ( md.getId == -1 ) { 
			//if ( callstack.length > 0 ) println("thread switched " + TraceUtil.callString(call) + " - on statck " + TraceUtil.callString(callstack.top))
			return 
		}
		
		val pkg = TraceUtil.getPackageName(md.getClassName)
		
  		if (call.getMethodAction() == 0) { // enter		
  			if ( callstack.length > 0 ) {
	  			val caller = callstack.top
	  			callOrder += 1
	  			val stackmd = callstack.map(_.getMethodData)
	  			if ( stackmd.contains(md) ) { // recursive call
	  				var orderRedefined = -1
	  				callstack.reverse.foreach(c => {
	  					if ( orderRedefined == -1 ) {
	  						 if (c.getMethodData == md) orderRedefined = call2label(c)
	  					}
	  					else 
	  						call2label(c) = orderRedefined
	  				})
	  			}
  			}
  			
  			//minorOrder = if ( callstack.length == 0)  0 else minorOrder + 1
  			//numOfCalls += 1
  			
  			callstack.push(call)
  			
  			//println("\tEnter - mid is  "+ call.getMethodData.getId + " : " + callstack.length )
  		}
  		else { // exit
  			//println("\tExit - mid is "+ call.getMethodData.getId + " : " + callstack.length)
  			if ( callstack.length == 0 ) {
  				println("Return with no stack: " + md.getName)
  				return
  			}
  			val c = callstack.pop
  			if ( callstack.length == 0 ) {
  				//majorOrder += 1
  				//if ( !topMethods.contains(c.getMethodData) ) topMethods = c.getMethodData :: topMethods
  			}
  			
  			if ( callstack.length == 1 ) {
  				//majorOrder1 += 1
  				//if ( !topMethods1.contains(c.getMethodData) ) topMethods1 = c.getMethodData :: topMethods1	
  			}
  			
  			if ( md.getName != c.getMethodData.getName ) {
  				println("CHECK stack: " + md.getName + ", " +  c.getMethodData.getName)
		 	}
  		}
	}
}


class CallLabelingBySCCToSame (s: Scenario) extends TraceLogAnalysis(s)  {
	var thrMapLabel = Map[Int, LabelAanlysisThreadWise]()
	def run = {
		
		for ( t <- threads ) 
  			thrMapLabel += (t.getId -> new LabelAanlysisThreadWise(t.getId))

		for ( c <- calls ) {
			val call : Call =c.asInstanceOf[Call] 
			val tid = call.getThreadId
			if (! thrMapLabel.contains(tid)) {
				println("not defined tid" + call.getMethodData.getName)
			}
			else {
				thrMapLabel(tid).handleCall(call)
			}
		}
	}
}

object CallLabelingBySCCToSame {
	def main(args: Array[String]) = {
		(new CallLabelingBySCCToSame(SNSLogin)).run
	}
}