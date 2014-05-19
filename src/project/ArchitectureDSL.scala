package project

import scala.util.parsing.combinator._
import scala.io._
import java.io._

class ArchitectureDSL extends JavaTokenParsers {
    var mappings = List[Mapping]()
    def parse(desc: String): List[Mapping] = {
        println(parseAll(architectureMappingsDef, desc))
        Mappings.mappings.values.toList
    }

    def architectureMappingsDef: Parser[Any] = rep(mappingDef | aliasDef ) ^^ { // | commentDef
        case defs => {
        	defs.foreach(d => 
        		d match {
        			case aliases : List[(String, String)] => Aliases.add(aliases)
        			case mappings : (String, List[(String, List[String])]) =>Mappings.add(mappings)
        		}
        	)
        	defs
        }
    }

    def aliasDef: Parser[List[(String, String)]] = "alias" ~ "{" ~ rep(aliasEntryDef) ~ "}" ^^ {
        case "alias" ~ "{" ~ entries ~ "}" => { entries }
    }
    def aliasEntryDef: Parser[(String, String)] = ident ~ ":" ~ ident ^^ {
        case name ~ ":" ~ value => { (name, value) }
    }

    def mappingDef: Parser[(String, List[(String, List[String])])] = "mapping" ~ ident ~ "{" ~ rep(aliasMapping | stringMapping) ~ "}" ^^ {
        case "mapping" ~ mappingName ~ "{" ~ entries ~ "}" => {  (mappingName, entries) }
    }

    def aliasMapping: Parser[(String, List[String])] = ident ~ "(" ~ rep(stringLiteral) ~ ")" ^^ {
        case name ~ "(" ~ values ~ ")" => {  (name, values.map(v => v.substring(1, v.length-1))) }
    }

    def stringMapping: Parser[(String, List[String])] = stringLiteral ~ "->" ~ stringLiteral ^^ {
        case value ~ "->" ~ name => {  (name.substring(1, name.length-1), List(value.substring(1, name.length-1))) }
    }
    
    //def commentDef : Parser[Any] = "//" ~ ident ^^ { case "//"~ comment => "comment" }
}

object  ArchitectureDSL {
	def main(args: Array[String]): Unit = {
		val parser = new ArchitectureDSL
		parser.parse(Source.fromFile(new File("../TraceViewLog/input/architecture.mapping")).mkString)
		
		println(Aliases.aliases)
		Mappings.mappings.foreach(m => {println(m._1); println(m._2); println(m._2.toStringLabel)} )
	}
}