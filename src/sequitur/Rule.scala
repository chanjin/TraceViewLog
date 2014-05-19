package sequitur

class Rule {
    // Guard symbol to mark beginning and end of rule.
    var theGuard: Guard = new Guard(this)
    var count = 0
    var number = Rule.numOfRules
    Rule.numOfRules = Rule.numOfRules + 1
    var index = 0

    def first = theGuard.n
    def last = theGuard.p

    override def toString = {
        count + "R" + index
    }

    import java.util.Vector
    def getRulesResult:  scala.collection.mutable.Map[Rule, List[Symbol]] = {
    	var result = scala.collection.mutable.Map[Rule, List[Symbol]]()
    	
        val rules = new Vector[Rule](Rule.numOfRules)
        var (currentRule, referedTo): (Rule, Rule) = (null, null)
        var sym: Symbol = null
        var index = 0

        var charCounter = 0;

        rules.addElement(this);
        result += (this -> List[Symbol]())

        var processedRules = 0;
        while (processedRules < rules.size) {
            currentRule = rules.elementAt(processedRules);
            val result_rule = currentRule

            sym = currentRule.first
            while (!sym.isInstanceOf[Guard]) {
                sym match {
                    case nt: NonTerminal => {
                    	println("***" + sym)
                        referedTo = nt.rule
                        if ((rules.size > referedTo.index) && (rules.elementAt(referedTo.index).eq(referedTo))) {
                            index = referedTo.index;
                        } else {
                            index = rules.size();
                            referedTo.index = index;
                            rules.addElement(referedTo);
                            if ( !result.contains(referedTo)) 
                            	result += (referedTo -> List[Symbol]())
                        }
                        result(result_rule) = sym :: result(result_rule)
                    }
                    case t: Terminal => {
                    	println("***" + sym)
                        result(result_rule) = sym :: result(result_rule)
                    }
                    case _ =>
                }
                sym = sym.n
            }
            processedRules = processedRules + 1
        }
        result    	
    }
    
    def getRules: String = {
        val rules = new Vector[Rule](Rule.numOfRules)
        var (currentRule, referedTo): (Rule, Rule) = (null, null)
        var sym: Symbol = null
        var index = 0

        var text = new StringBuffer();
        var charCounter = 0;

        text.append("Usage\tRule\n");
        rules.addElement(this);

        var processedRules = 0;
        while (processedRules < rules.size) {
            currentRule = rules.elementAt(processedRules);
            text.append(" " + currentRule.count + "\tR" + processedRules + " -> ");

            sym = currentRule.first
            while (!sym.isInstanceOf[Guard]) {
                sym match {
                    case nt: NonTerminal => {
                        referedTo = nt.rule
                        if ((rules.size > referedTo.index) && (rules.elementAt(referedTo.index).eq(referedTo))) {
                            index = referedTo.index;
                        } else {
                            index = rules.size();
                            referedTo.index = index;
                            rules.addElement(referedTo);
                        }
                        text.append("R" + index)
                    }
                    case t: Terminal => {
                        text.append(sym.value.toString)
                    }
                    case _ =>
                }
                text.append(' ');
                sym = sym.n
            }
            text.append('\n');
            processedRules = processedRules + 1
        }
        new String(text);
    }
}

object Rule {
    var numOfRules = 0
}
