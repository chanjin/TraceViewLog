package project

class Mapping(mapping: (String, List[(String, List[String])])) {
	def name = mapping._1
	val entries = mapping._2.foldLeft(Map[String, String]()) ( (ms, nvs) =>
		ms ++ nvs._2.map(v => (v, nvs._1))
	)
	
	def getModuleName(clsname: String) = {
		val pkg = entries.keys.toList.filter(p => clsname.startsWith(p))
		if ( pkg.length == 0 ) {
			println("*** ERROR: no component for " + clsname + ": " + entries.keys.mkString(", "))
			""
		}
		else if ( pkg.length == 1) getLabel(pkg.head)
		else 
			getLabel(pkg.tail.foldLeft(pkg.head)((maxLenStr, s) => if (maxLenStr.length < s.length) s else maxLenStr))
	}
	
	def getLabel(pkg: String) = Aliases.get(entries(pkg))
	override def toString = name + ": " + entries
	def toStringLabel = {
		name + ":" + entries.map(e => ( e._1, if ( Aliases.contains(e._2)) Aliases.get(e._2) else e._2))
	}
}

object Aliases {
	var aliases = Map[String, String]()
	def add(as: List[(String, String)]) = {
		as.foreach(a => aliases += (a._1 -> a._2))
	}
	def get(name: String) = aliases(name)
	def contains(name: String) = aliases.contains(name)
	
}

object Mappings {
	var mappings = Map[String, Mapping]()
	def add(mapping: (String, List[(String, List[String])])) = {
		mappings += (mapping._1 -> new Mapping(mapping))
	}
	def getMapping(n: String) = {
		if ( !mappings.contains(n)) {
			println(n + " --- " + mappings.keys.mkString(","))
		}
		mappings(n)
	}
}