package graph


object PkgNodeColor {
	private val colormap =Array( "aliceblue", "aquamarine",  "cyan",  "gold",  "yellow",  "steelblue",  "steelblue1", 
			"darkturquoise",  "deepskyblue",  "chartreuse", "mintcream", "linen", "papayawhip", "beige", "bisque", 
			"cornsilk", "darkseagreen", "floralwhite", "gainsboro", "ivory", "lightcyan", "aliceblue", "aquamarine",  "cyan",  "gold",  "yellow",  "steelblue",  "steelblue1", 
			"darkturquoise",  "deepskyblue",  "chartreuse", "mintcream", "linen", "papayawhip", "beige", "bisque", 
			"cornsilk", "darkseagreen", "floralwhite", "gainsboro", "ivory", "lightcyan")
	private var pkgcolor = scala.collection.mutable.Map[String, String] ()
	
	def initPkgcolor(pdg: ModuleStructure) = {
		var i = 0
		pkgcolor.empty
		for ( p <- pdg.nodes.keys) {
			pkgcolor += (p -> colormap(i % colormap.length))
			i = i+1 
		}
	}
	
	def packageNodeColor(n: NodeElem): String =  ", color= " + pkgcolor(n.value)
}