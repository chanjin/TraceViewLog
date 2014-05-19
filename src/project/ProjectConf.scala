package project
import org.apache.commons.configuration._;


object ProjectConf {
	
	val config = new PropertiesConfiguration("traceviewlog.properties")
	val outputdir = config.getString("output_dir")
	val tracefiledir = config.getString("tracefile_dir")
	
    def main(args: Array[String]): Unit = {
        try {
            println("start")
            val config = new PropertiesConfiguration("traceviewlog.properties");
            println(config.getString("message"))
        } catch {
            case e: ConfigurationException => println(e)
            case e: Exception => e.printStackTrace()
        }
    }
}