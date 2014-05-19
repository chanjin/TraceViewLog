package sequitur

import java.util.Hashtable

abstract class Symbol {
    var value = 0
    var (p, n): (Symbol, Symbol) = (null, null)

    def deleteDigram: Unit = {
        if (n.isInstanceOf[Guard]) return
        
        val dummy = Symbol.theDigrams.get(this);

		// Only delete digram if its exactly
		// the stored one.

		if (dummy.eq(this))
			Symbol.theDigrams.remove(this);
    }

    def cleanUp

    /**
     * Checks a new digram. If it appears elsewhere, deals with it by calling
     * match(), otherwise inserts it into the hash table. 
     * Overwritten in subclass guard.
     */
    def check: Boolean = {
        if (n.isInstanceOf[Guard]) return false;
        if (!Symbol.theDigrams.contains(this)) {
            //Symbol.theDigrams += (this -> this)
        	Symbol.theDigrams.put(this,this)
            return false
        } 
           
        val found = Symbol.theDigrams.get(this).asInstanceOf[Symbol]
        if ( ! found.n.eq(this) ) {
            matchSymbol(this, found)
        } 
        //else println("found but not match - " + found + ":" + this)
        true
    }

    def insertAfter(toInsert: Symbol) = {
        Symbol.join(toInsert, n);
        Symbol.join(this, toInsert);
    }

    def substitute(r: Rule) {
        cleanUp
        n.cleanUp
        p.insertAfter(new NonTerminal(r));
        if (!p.check) p.n.check
    }

    // Deal with a matching digram.
    def matchSymbol(newD: Symbol, matching: Symbol) {
        var r: Rule = null
        var (first, second): (Symbol, Symbol) = (null, null)

        if (matching.p.isInstanceOf[Guard] && matching.n.n.isInstanceOf[Guard]) { // reuse an existing rule
            r = matching.p.asInstanceOf[Guard].rule;
            newD.substitute(r);
            println("Rule reuse - " + r)
        } else {  // create a new rule
            r = new Rule
            try {
                first = newD.clone.asInstanceOf[Symbol]
                second = newD.n.clone.asInstanceOf[Symbol]
                r.theGuard.n = first;
                first.p = r.theGuard;
                first.n = second;
                second.p = first;
                second.n = r.theGuard;
                r.theGuard.p = second;

                Symbol.theDigrams.put(first, first);
                //Symbol.theDigrams += (first -> first)
                matching.substitute(r);
                newD.substitute(r);
                println("Rule created - " + r)
            } catch {
                case c: CloneNotSupportedException => c.printStackTrace
                case _ =>
            }
        }

        // Check for an underused rule.
        if (r.first.isInstanceOf[NonTerminal] && ((r.first.asInstanceOf[NonTerminal]).rule.count == 1))
            (r.first.asInstanceOf[NonTerminal]).expand
    }

    // Values in linear combination with two prime numbers.
    override def hashCode: Int = {
        val code: Long = (21599.asInstanceOf[Long] * (value.asInstanceOf[Long]) + 20507.asInstanceOf[Long] * (n.value.asInstanceOf[Long]))
        (code % Symbol.prime.asInstanceOf[Long]).asInstanceOf[Int]
    }

    // Test if two digrams are equal. two symbols are equal if both of my value and next's value are equal (digram) 
    override def equals(obj: Any): Boolean = {
        val sym = obj.asInstanceOf[Symbol]
        (value == sym.value) && (n.value == sym.n.value)
    }

    override def toString() = {
        value + "(" + n.value + ")"
    }

}



object Symbol {
    val numTerminals = 100000;
    val prime = 2265539
    //var theDigrams = Map[Symbol, Symbol]()
    var theDigrams = new Hashtable[Symbol, Symbol](prime)

    def join(left: Symbol, right: Symbol) = {
        if (left.n != null) left.deleteDigram
        left.n = right;
        right.p = left;
    }
}

case class Terminal(v: Int) extends Symbol with Cloneable {
    value = v
    override def cleanUp = {
        Symbol.join(p, n)
        deleteDigram
    }
}

case class NonTerminal(r: Rule) extends Symbol with Cloneable {
    def rule = r

    value = Symbol.numTerminals + r.number;
    r.count = r.count + 1
    p = null
    n = null

    override protected def clone: Object = {
        val sym = new NonTerminal(r)
        sym.p = p;
        sym.n = n;
        sym
    }

    override def cleanUp = {
        Symbol.join(p, n)
        deleteDigram
        r.count = r.count - 1
    }

    def expand = {
        Symbol.join(p, r.first)
        Symbol.join(r.last, n)

        // Necessary so that garbage collector
        // can delete rule and guard.

        r.theGuard.rule = null;
        r.theGuard = null;
    }
    
}

case class Guard(r: Rule) extends Symbol {
    var rule = r
    value = 0
    p = this
    n = this

    override def cleanUp() = Symbol.join(p, n);
    override def deleteDigram = {}
    override def check: Boolean = false
}