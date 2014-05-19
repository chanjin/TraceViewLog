package analysis

import project._
import traceview._
import graph._

class TraceLogAnalysis(scenario: Scenario) {
    val reader = scenario.getTraceReader //DmTraceReader
    val methods: List[MethodData] = reader.getMethods.toList
    val threads: List[ThreadData] = reader.getThreads.toList
    val calls: Array[Object] = reader.getCalls.toArray
    var thrMap = scala.collection.mutable.Map[Int, CallAnalysisThreadWise]()

    val topLevelMethod = reader.getTopLevelMethod

    def printmd(md: MethodData): String = {
        val arr = md.getProfileNodes
        val profData = if (arr.length > 0 && arr(0).getChildren.length > 0) {
            val children = arr(0).getChildren
            children(0)
        } else null
        md.toString + ":" + md.getCalls + ", " + md.getProfileNodes.length + ": " + (if (profData != null) profData.getProfileName else "")
    }

    def getPackages(methods: List[MethodData]) = {
        def mapftn(m: MethodData) =
            if (m.getClassName.lastIndexOf("/") != -1) m.getClassName.substring(0, m.getClassName.lastIndexOf("/"))
            else "<no-package: " + m.getClassName + ">"
        methods.map(mapftn(_)).removeDuplicates
    }

    def getMethodsInPackages(methods: List[MethodData], pkgs: List[String]) = {
        def mapftn(m: MethodData) =
            if (m.getClassName.lastIndexOf("/") != -1) pkgs.contains(m.getClassName.substring(0, m.getClassName.lastIndexOf("/")))
            else false
        methods.filter(mapftn(_))
    }

    protected def isAppClass(clsname: String): Boolean = scenario.getPackagePrefixes.exists(pp => clsname.startsWith(pp))

    protected def isAppThread(thr: CallAnalysisThreadWise): Boolean = {
        val mcg = thr.methodCallGraph
        mcg.nodes.values.exists(mn => isAppClass(mn.asInstanceOf[MethodNode].mdata.getClassName))
    }

    import java.text.NumberFormat
    val nfpct = NumberFormat.getPercentInstance()
    nfpct.setMinimumFractionDigits(2);
    val nfnum = NumberFormat.getNumberInstance()
    nfnum.setMinimumFractionDigits(2)

    def percentage(t: Long, total: Long): String = nfpct.format(t.asInstanceOf[Double] / total.asInstanceOf[Double])
    def getTimeStr(t: Long, total: Long) = {
        nfnum.format(t.asInstanceOf[Double] / 1000000.0) + " sec (" + percentage(t, total) + " %)"
    }
    val total = methods.foldLeft(0L)((sum, m) => sum + m.getElapsedExclusive)
    val totalExceptTop = methods.remove(_ == topLevelMethod).foldLeft(0L)((sum, m) => sum + m.getElapsedExclusive) // top-level method is excluded

    def getMethodStr(m: MethodData) = if (m.getMethodName == null) m.getName else m.getMethodName + " (" + m.getClassName + ")"

    def createMethodCallGraph: MethodCallGraph = { // union thread wise method call graphs
        var mcgarch = new MethodCallGraph
        thrMap.foreach(thr => {
            (thr._2.methodCallGraph).nodes.foreach(n => {
                if (mcgarch.getNode(n._2.value) == null) mcgarch.addNode(new MethodNode(n._2.asInstanceOf[MethodNode].mdata))
            })
        })

        thrMap.foreach(thr => {
            thr._2.methodCallGraph.edges.foreach(e => {
                mcgarch.addEdgeNoLabel(e.getSource.value, e.getTarget.value)
            })
        })
        mcgarch
    }

    def createArchCallGraphFrom(mcgarch: MethodCallGraph, archmapftn: String => String) = {
        var archgraph = new ModuleStructure(mcgarch)
        mcgarch.nodes.foreach(n => {
            val compstr = archmapftn(n._2.asInstanceOf[MethodNode].mdata.getClassName)
            var comp = archgraph.getNode(compstr)
            if (comp == null) comp = archgraph.addNode(new ModuleNode(compstr))
            archgraph.addMember(comp.asInstanceOf[ModuleNode], n._2.asInstanceOf[MethodNode].value)
        })
        archgraph.liftEdges()
        archgraph
    }

    def analyzeCalls = {
        threads.foreach(t =>
            thrMap += (t.getId -> new CallAnalysisThreadWise(t.getId, t, scenario)))

        for (c <- calls) {
            val call: Call = c.asInstanceOf[Call]
            thrMap(call.getThreadId).handleCall(call)
        }
    }
}
