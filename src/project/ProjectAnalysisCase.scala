package project

import traceview._

trait Scenario {
	def getTraceReader: DmTraceReader = 
		new  DmTraceReader(getFileName, false)
	def getName : String
	def getFileName : String
	def 	getPackagePrefixes : List[String]
}



object APICase {
	def getFileName = "com.example.android.apis.trace"
	def getPackages = List("com/example/android/apis")
	def getPackagePrefix = "com/example/android/apis"
	def getPackagePrefixes = List("com/example", "android/view", "android/widget", "android/os", "android/graphics")
}

object SNSCases {
	def getScenarios = List(SNSLogin, SNSMakeTweet, SNSRefreshTweet, SNSLogout)
}

object SNSLogin extends Scenario {
	def getName = "SNSLogin"
	def getFileName = ProjectConf.tracefiledir + "/SNS_Tracing_1_Login.trace"
	def getPackagePrefixes =List("com/lge")  // List("com/lge", "android/view", "android/widget", "android/os", "android/graphics")
}

object SNSMakeTweet extends Scenario {
	def getName = ProjectConf.tracefiledir + "MakeTweet"
	def getFileName = ProjectConf.tracefiledir + "/SNS_Tracing_3_MakeTweet.trace"
	def getPackagePrefixes =List("com/lge")  // List("com/lge", "android/view", "android/widget", "android/os", "android/graphics")
}

object SNSRefreshTweet extends Scenario {
	def getName = ProjectConf.tracefiledir + "RefreshTweet"
	def getFileName = ProjectConf.tracefiledir + "/SNS_Tracing_2_Refresh.trace"
	def getPackagePrefixes =List("com/lge")  // List("com/lge", "android/view", "android/widget", "android/os", "android/graphics")
}

object SNSLogout extends Scenario {
	def getName = ProjectConf.tracefiledir + "Logout"
	def getFileName = ProjectConf.tracefiledir + "/SNS_Tracing_4_Logout.trace"
	def getPackagePrefixes =List("com/lge")  // List("com/lge", "android/view", "android/widget", "android/os", "android/graphics")
}


