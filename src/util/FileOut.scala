package util

class FileOut {
	import java.io.{File, FileOutputStream, PrintWriter}
  
	def parent(dir: String) = dir.substring(0, dir.lastIndexOf("/"))
	def makeDirectory(dir: String) : Unit = {
		if ( !checkDirectory(parent(dir)) ) makeDirectory(parent(dir))
		
	   	val success = (new File(dir)).mkdir();
	     if (success) {
	    	 System.out.println("Directory: " + dir + " created");
	     }
	}
	
  	def checkDirectory(dir: String) : Boolean= {
		(new File(dir)).exists
  	}


  
  def saveFile(file: String, str: String) = {
		if ( !checkDirectory( parent(file))) {
			if ( parent(file) == file ) throw new Exception("no directory found")
			println(parent(file))
			makeDirectory(parent(file))
		}
			
		var fos: FileOutputStream = null
		var writer: PrintWriter = null
	    try{   
		  	 fos = new FileOutputStream(file)
		  	 writer = new PrintWriter(fos, true)
			 writer.print(str)
		 }
	    finally {
	    	writer.close()
		  	fos.close()
	    }
   }
}


object TestFile {
	def main(args: Array[String]) = {
		val fout = new FileOut
		val filename = project.ProjectConf.outputdir  +  "/SNSLogin/Test/ArchCallGraph.dot"
		val dir = filename.substring(0, filename.lastIndexOf("/"))
		if ( !fout.checkDirectory(dir) ) {
			fout.makeDirectory(dir) 
		}
	}
}