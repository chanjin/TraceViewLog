package sequitur

class Sequitur {
    var firstRule: Rule = null
    def runSequitur(seq: List[Int]) = {
        firstRule = new Rule
        var i = 0
        //Symbol.theDigrams.clear();
        //Symbol.theDigrams = Map[Symbol, Symbol]()
        seq.foreach(c => {
            //println("---- " + c + ": " + seq);
            firstRule.last.insertAfter(new Terminal(c));
            firstRule.last.p.check
            //println(Symbol.theDigrams)
        })
        firstRule.getRules
    }

    def runSequiturResult(seq: List[Int]) = {
        firstRule = new Rule
        var i = 0
        Rule.numOfRules = 0
        Symbol.theDigrams.clear
        seq.foreach(c => {
            println("---- " + c + ": " + seq);
            firstRule.last.insertAfter(new Terminal(c));
            firstRule.last.p.check
            println(Symbol.theDigrams)
        })
        firstRule.getRulesResult
    }

}

class CompactSeq(res: Map[Rule, List[Symbol]]) {
    val equivRules = checkDuplicateRules(res)
    val eqmap = equivRules.foldLeft(Map[Rule, List[Rule]]())((m, rs) =>
        m ++ rs.foldLeft(Map[Rule, List[Rule]]())((m1, r) => m1 + (r -> rs)));
    val newIndex : Map[Rule, Int] = 
    	( eqmap.foldLeft((Map[Rule, Int](), 0)) ((mc, entry) => (mc._1 + (entry._1 -> mc._2 ), mc._2 + 1)) )._1 ;
    val newRes = res.foldLeft(Map[List[Rule], List[Symbol]]())((newmap, entry) =>
            if (!newmap.contains(eqmap(entry._1))) newmap + (eqmap(entry._1) -> entry._2)
            else newmap);
    
    val rle =newRes.foldLeft(Map[List[Rule], List[(Int, String)]]())( (rlemap, entry) => 
    	rlemap + (entry._1 -> encodeRLE(entry._2)) )
    	
    val compactSeq = {
    	
    }
            
    def checkDuplicateRules(rules: Map[Rule, List[Symbol]]): List[List[Rule]] = {
        def checkDuplicateRulesR(head: (Rule, List[Symbol]), tail: Map[Rule, List[Symbol]], eq: List[List[Rule]]): List[List[Rule]] = {
            //println("check - " + head._1 + " : " + tail.keys.mkString(", "))
            if (tail.size == 0) return List(head._1) :: eq
            val headEq = (tail.foldLeft((head, List[Rule]()))((dup, r) => if (dup._1._2 == r._2) (dup._1, r._1 :: dup._2) else (dup)))._2
            val equivList = (head._1 :: headEq) :: eq
            //println("check - " + equivList)
            val newmap = (tail -- headEq)
            if (newmap.size == 0) return equivList
            checkDuplicateRulesR(newmap.head, newmap.tail, equivList)
        }

        checkDuplicateRulesR(rules.head, rules.tail, List[List[Rule]]())
    }

    def groupedIndex(rules: List[Rule]) = "R(" + rules.map(_.index).mkString(",") + ")"

    def symval(s: Symbol): String =
        s match {
            case nt: NonTerminal => groupedIndex(eqmap(nt.rule))
            case t: Terminal => t.value.toString
        }
    
    def encodeRLE(syms: List[Symbol]): List[(Int, String)] =
        (1 until syms.size).foldLeft((1, syms.head, List[(Int, String)]())) {
            case ((len, sym, ls), index) if symval(sym) != symval(syms(index)) => (1, syms(index), (len, symval(sym)) :: ls)
            case ((len, sym, ls), _) => (len + 1, sym, ls)
        } match {
            case (len, c, ls) => (len, symval(c)) :: ls //(len, symval(c)) :: ls
        }
     
    def encodeRLE1(syms: List[Symbol]): List[(Int, String)] =
        (1 until syms.size).foldLeft((1, syms.head, List[(Int, String)]())) {
            case ((len, sym, ls), index) if symval(sym) != symval(syms(index)) => (1, syms(index), (len, symval(sym)) :: ls)
            case ((len, sym, ls), _) => (len + 1, sym, ls)
        } match {
            case (len, c, ls) => (len, symval(c)) :: ls //(len, symval(c)) :: ls
        }
        
     def getRulesString = newRes.map(e => (groupedIndex(e._1) + "->" + e._2.map(s => symval(s)).reverse))
     def getFirstRuleString(rule: Rule) = newRes(List(rule)).reverse.map(sym => symval(sym))
     def getRLEListString(rule: Rule): List[String] = rle(List(rule)).map(ls => ls._1 + ":" + ls._2)
     
     // Map of Rules ( Rule -> List of (freq, Int), (freq, Rule) )
}

class CallSym
case class CallElt(v: Int) extends CallSym {
	def value = v
}
case class CallSeq(ruleIndex: Int, elts: List[CallSym]) extends CallSym {
	val index = ruleIndex
	val elements = elts
}


object Sequitur {

    def main(args: Array[String]): Unit = {
        //val defaultText = "abbbbbbbbbcdddbcdc" "012111112341114432343121212"
        //"pease porridge hot,\npease porridge cold,\npease porridge in the pot,\nnine days old.\n\nsome like it hot,\nsome like it cold,\nsome like it in the pot,\nnine days old.\n"
        val defaultText = //List(1, -1, 1, -1, 2, -2, 3, 4, -4, 4, -4, 5, -5, 4, -4, 6, -6, 7, -7, 8, -8, 9, -9, 9, -9, 9, -9, 9, -9, 9, -9, 9, -9, 9, -9, 9, -9, 9, -9, 9, -9, 10, -10, 11, -11, 12, -12, 13, -13, 13, -13, 14, -14, 15, -15, 16, -16, 17, -17, 16, -16, 18, -18, 18, -18, 16, -16, 19, -19, 16, -16, 20, -20, 21, -21, 16, -16, 22, -22, 16, -16, 16, -16, 23, -23, 24, -24, 25, -25, 16, -16, 26, -26, 24, -24, 25, -25, 16, -16, 27, -27, 16, -16, 28, -28, 29, -29, 29, -29, -3)
        List(0, 1, 2, 1, 1, 1, 1, 1, 2, 3, 4, 1, 1, 1, 4, 4, 3,  2, 3, 4, 3, 1, 2, 1, 2, 1,2)
        val s = new Sequitur
        //println(s.runSequitur(defaultText))
        println(defaultText)
        val res = s.runSequiturResult(defaultText)
        val compactSeq = new CompactSeq(Map[Rule, List[Symbol]]() ++ res)

        //println(res.map(e => (e._1.count + ": R" + e._1.index + "->" + e._2.map(s => symval(s)).reverse)).mkString("\n"))
        println(compactSeq.getRulesString.mkString("\n"))
        println("-----")
        println(compactSeq.getFirstRuleString(s.firstRule))
        println("---")
        println(compactSeq.getRLEListString(s.firstRule).mkString("\n"))
    }
}