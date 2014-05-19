package experiments


import analysis._
import project._
import graph._
import util._

import colibri.lib._
import colibri.io.lattice._
import scala.collection.JavaConversions._
import java.io._

class FormalConceptAnalysis {
	class ObjectPackage(mod: ModuleNode ) extends Comparable[ObjectPackage] {
		def getPackage = mod
		def value = mod.value
		override def compareTo(obj : ObjectPackage) : Int = value.compareTo(obj.value)
		override def toString = value
	}
	class AttributeScenario(s: Scenario) extends Comparable[AttributeScenario] {
		def getScenario = s
		def value = s.getName
		override def compareTo(attr : AttributeScenario) : Int = value.compareTo(attr.value)
		override def toString = value
	}
	
	class ScenarioAnalysis(s: Scenario)  extends TraceLogAnalysis(s) {
		def getPackageNodes(maparchftn: String => String) = {
			analyzeCalls
			val archgraph = createArchCallGraphFrom(createMethodCallGraph, maparchftn)
			archgraph.nodes.values
		}
	}

	def constructLattice(mappings: Map[ObjectPackage, List[AttributeScenario]]) = {
		var rel = new TreeRelation
		mappings.foreach( m => m._2.foreach( attr => rel.add(m._1, attr) ) )
		var lattice = new RawLattice(rel)
		
		var it : Iterator[Concept] = lattice.conceptIterator(Traversal.TOP_OBJSIZE)
		it.foreach(println(_))
		
		val todot = new LatticeWriterDot
		todot.write(lattice, new File("lattice.dot"), " | ", ", ")
	}
	
	def getObjectAttributeMappings(archmapftn : String => String): Map[ObjectPackage, List[AttributeScenario]] = {
		var mappings = Map[ObjectPackage, List[AttributeScenario]]()
		
		SNSCases.getScenarios.foreach( scenario => {
			val attribute = (new AttributeScenario(scenario)) 
			(new ScenarioAnalysis(scenario)).getPackageNodes(archmapftn).foreach(n => { 
				val objectpkg = new ObjectPackage(n.asInstanceOf[ModuleNode])
				mappings += ( objectpkg -> (attribute :: mappings.getOrElse(objectpkg, List[AttributeScenario]())))
			})
		})
		mappings
	}
	
	def run = {		
		//constructLattice(getObjectAttributeMappings(Architecture.getModuleOfClass(Architecture.packagearch)))
		constructLattice(getObjectAttributeMappings(Architecture.getModuleOfClass("snsonly")))
	}
}



object FormalConcept {
	def main(args: Array[String]) = {
		println("Hello Trace!")
		(new FormalConceptAnalysis).run
	}
}