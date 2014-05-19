package project

import scala.io._
import java.io._
import org.apache.commons.configuration._

object Architecture {
    val config = ProjectConf.config 

    val parser = new ArchitectureDSL
    parser.parse(Source.fromFile(new File(config.getString("architecture_loc"))).mkString)

    //def getModuleOfClass(mapClass2Module: Map[String, String])(clsname: String): String = {
    def getModuleOfClass(arch: String)(clsname: String): String = {
    	val mapClass2Module = Mappings.getMapping(arch)
    	mapClass2Module.getModuleName(clsname)
    }
}