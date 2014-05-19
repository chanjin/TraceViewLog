package util

import traceview._

object TraceUtil {
	def getPackageName(cls: String) = 
	  			if ( cls.lastIndexOf('/') > 0) cls.substring(0, cls.lastIndexOf('/'))
	  			else cls
	def callString(c: Call) = "(" + c.getThreadId + ", " + c.getMethodAction + ": " + c.getMethodData.getId + ", " + c.getMethodData.getName + ")"
	def methodString(md: MethodData) = "(" + md.getMethodName + ": " + md.getCalls + " times, " + md.getElapsedExclusive + " msec ex, " + md.getElapsedInclusive + " msec in)" 
}