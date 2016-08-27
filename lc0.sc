import scala.annotation.tailrec
import scala.util.Try

def checkE(a: Any, b: Any, m: String): Unit = assert(a == b, s"failed: $m: got $a vs $b")
def checkA(a: Term, b: Term, m: String): Unit = assert(a === b, s"failed: $m: got $a vs $b")

sealed trait Term {
	def apply(a:Term) : Term = Ap(this, a)
	def -:(x: Symbol): Term = Lam(x,this)  // right-associative by virtue of the trailing colon
	override def toString: String = Term.printTerm(this, Neutral)
	def ! : Term = oneStepEval(this)
	def !? : Option[Term] = manyStepsEval(this)
	def !! : Term = manyStepsEval(this).get
	def === (other: Term): Boolean = compareTerms(this, other) 
}
case class Lam(x: Symbol, body: Term) extends Term
case class Ap(f: Term, a: Term) extends Term
case class Var(x: Symbol) extends Term

implicit def fromVar(x: Symbol): Term = Var(x)

checkE('a -: 'b , Lam('a, Var('b)), "lambda is converted from syntax")
checkE( ('a -: 'a)('b) , Ap(Lam('a, Var('a)), Var('b)) , "application is converted from syntax" )
checkE('a -: 'b -: 'a , Lam('a, Lam('b, Var('a))), "lambda is right-associative")
checkE('a -: 'b('a) , Lam('a, Ap(Var('b), Var('a))), "lambda binds looser than application")
checkE('a('b)('c) , Ap(Ap(Var('a), Var('b)), Var('c)), "application is left-associative")

def compareTerms(a: Term, b: Term): Boolean = if (a==b) true else a match {
	case Lam(x,e) => b match {
		case Lam(y,f) => if (x==y) compareTerms(e,f) else {
			val z = newSymbol(fv(e) ++ fv(f))
			compareTerms(boundRename(a, x, z), boundRename(b, y, z))
		}
		case _ => false
	}
	case Ap(f,e) => b match {
		case Ap(g,h) => compareTerms(f,g) && compareTerms(e,h)
		case _ => false
	}
	case Var(x) => false // if it were the same variable, we would have picked that up with literal comparison
}

checkA( 'a , 'a, "var is equal to itself")
checkA( 'a -: 'a , 'a -: 'a, "lambda is equal to itself")
checkA( 'a -: 'a , 'b -: 'b, "lambda is equal to itself up to alpha-conversion")
assert( ! ('a -: 'a  == 'a -: 'b), "lambda is equal to another lambda")
assert( ! ('a -: 'a == 'b -: 'a), "lambda is equal to another lambda with free variable")
assert( ! ('b -: 'a == 'a -: 'a), "lambda is equal to another lambda with free variable b")
assert( ! ('b -: 'a == 'b -: 'b), "lambda is equal to another lambda with free variable a")
checkA( 'b -: 'a , 'c -: 'a, "lambda is equal to another lambda with free variable and alpha conversion")
checkA( ('a-:'a)('b-:'b), ('c-:'c)('d-:'d), "two applications are equal")

def fv(t: Term): Set[Symbol] = t match {
	case Lam(x,e) => fv(e) - x
	case Ap(f,e) => fv(f) ++ fv(e)
	case Var(x) => Set(x)
}

checkE(fv('a) , Set('a), "correct freevars of variable")
checkE(fv('a -: 'b) , Set('b), "correct freevars of lambda with unbound var")
checkE(fv('a -: 'a) , Set(), "correct freevars of lambda with bound var")
checkE(fv('a -: 'a('b)('c)) , Set('b,'c), "correct freevars of lambda with bound and unbound var")
checkE(fv(('a-:'b)('c)) , Set('b,'c), "correct freevars of application")

def newSymbol(old: Set[Symbol]): Symbol = {
	Stream.from(0).map(i => Symbol(s"x$i")).find( s => !(old contains s) ).get
}

checkE(newSymbol(Set()) , 'x0, "first new symbol should be 'x0")
checkE(newSymbol(Set('a, 'b, 'x0,'x1, 'x3)) , 'x2, "new symbol should be 'x2")

sealed trait Location
case object Left extends Location
case object Right extends Location
case object Neutral extends Location

object Term {

	private def bracket(s: String, bracketed: Boolean): String = if (bracketed) s"($s)" else s

	def printTerm(t: Term, where: Location): String = t match {
		// variables are never bracketed
 		case Var(x) => s"${x.name}"
		
		// the body of lambda is never bracketed
		// but the lambda itself is always bracketed in a non-neutral location
		case Lam(x,e) => bracket(s"${x.name} \u2192 ${printTerm(e, Neutral)}", where != Neutral)

		// application is bracketed only in the right position		
		case Ap(f,e) => bracket(s"${printTerm(f, Left)} ${printTerm(e, Right)}", where == Right)
	}
}

checkE( ('a -: 'b).toString , "a → b", "printTerm lambda")
checkE( ('a -: 'b -: 'c).toString , "a → b → c", " printTerm nested lambda")
checkE( ('b ('c) ).toString , "b c", " printTerm application")
checkE( ('a -: 'b ('c) ).toString , "a → b c", " printTerm lambda body")
checkE( ('a('b)('c) ).toString , "a b c", " printTerm left-nested application")
checkE( ('a('b('c)) ).toString , "a (b c)", " printTerm right-nested application")
checkE( (('a -: 'b)('c) ).toString , "(a → b) c", " printTerm application of lambda")
checkE( ('c('a -: 'b) ).toString , "c (a → b)", " printTerm application to lambda")
checkE( (('a -: 'b)('c -:'d) ).toString , "(a → b) (c → d)", " printTerm application of lambda to lambda")
checkE( (('a('b)('a('c))) ).toString , "a b (a c)", " printTerm left-right-nested application")
checkE( ('a('b)(('a-:'a)('c)) ).toString , "a b ((a → a) c)", " printTerm left-right-nested application of lambda")

// in term t, blindly rename all occurrences of symbol x to symbol y 
def rename(t: Term, x: Symbol, y: Symbol): Term = t match {
	case Lam(z, body) => if (x == z) Lam(y, rename(body, x,y))
                          else Lam(z, rename(body, x,y))
	case Ap(a,b) => Ap(rename(a,x,y),rename(b,x,y))
	case Var(z) => if (x == z) Var(y) else t
}

// in term t, rename all bound occurrences of symbol x to symbol y, limited to top-level lambda
def boundRename(t: Term, x: Symbol, y: Symbol): Term = if (x == y) t else t match {
	case Lam(z, body) if (x == z) => Lam(y, freeRename(body, x,y))
	case _ => t
}

// in term t, rename all non-bound occurrences of symbol x to symbol y 
def freeRename(t: Term, x: Symbol, y: Symbol): Term = if (x==y) t else t match {
	case Lam(z, body) => if (x == z) t // x is bound in body, not renaming
                          else Lam(z, freeRename(body, x,y)) // x is free in body, need to rename
	case Ap(a,b) => Ap(freeRename(a,x,y), freeRename(b,x,y))
	case Var(z) => if (x == z) Var(y) else t
}

// beta-reduction: in term t, substitute a free variable x by term y, perform the necessary alpha-conversion
def subst(t: Term, x: Symbol, y: Term): Term = {
	if (! (fv(t) contains x))
		t  // nothing to substitute: there is no free x in t
	else {
	/*
	If x is present in t as a free variable (note that x could also be bound in some subterms of t),
	we need to check whether any free variables in y will clash with free variables in t after substitution.
	 */
		t match {
			case Lam(z, body) => {
				val freeVarsInY = fv(y)
				if (freeVarsInY contains z) {
				/*
				If z == x, "body" can't contain any free x. But we already know it does. So z is not equal to x here.
				However, z is among the free variables in y, so we need an alpha-conversion before we can insert y.
				The new name for z must be such that it does not clash with y or with existing body.
				We need to rename all free occurrences of z in body to newZ.
				 */
					val newZ = newSymbol(freeVarsInY ++ fv(body))
					Lam(newZ, subst(freeRename(body,z,newZ), x, y))
				} else
				// here, z is not free in y, so we can substitute without alpha-conversion. If body contains some of the same free variables as y, it's not a problem.
					Lam(z, subst(body, x, y))
			}
			case Ap(a,b) => Ap(subst(a,x,y), subst(b,x,y))
			case Var(_) => y // we know that t == Var(x) because x is free in t
		}
	}
}

def oneStepEval(t: Term): Term = t match {
	case Ap(a, b) => {
		val newA = oneStepEval(a)
		if (newA != a) Ap(newA, b) else {
			val newB = oneStepEval(b)
			if (newB != b) Ap(a, newB) else a match {
				case Lam(x,e) => subst(e, x, b)
				case _ => t
			}
		}
	}
	case _ => t // lambdas are not evaluated at all unless applied; free variables are never evaluated
}

checkE(oneStepEval('a) , Var('a), "eval of variable")
checkE(('a!) , Var('a), "unary syntax eval of variable")
val test1 = 'a -: ('x -: 'x)('a)
checkE(oneStepEval(test1) , test1, "eval of lambda")
checkE(oneStepEval( ('a -: 'a)('b) ) , Var('b), "eval of identity on variable")
val test2 = ('x -: 'x)('c)
checkE(oneStepEval( test1('c) ) , test2, "one step eval of test1")
checkE(oneStepEval(test2) , Var('c), "second step eval of test1")

val test3 = 'a -: 'b -: 'b('a)
val test4 = 'z -: 'b
checkE(oneStepEval(test3(test4)) , 'x0 -: 'x0(test4), "alpha conversion")

// examples with many alpha-conversions required
checkE( ( 'a -: 'b -: 'c -: 'a('b)('c) )('x -: 'y -: 'b('c)) !, 'x0 -: 'x1 -: ('x -: 'y -: 'b('c))('x0)('x1), "perform two alpha-conversions")

// loop detection

def termAIsSubtermOfB(a: Term, b: Term): Boolean = if (compareTerms(a,b)) true else b match {
	case Lam(x,e) => if (fv(a) contains x) {
			val z = newSymbol(fv(a) ++ fv(e))
			termAIsSubtermOfB(a, boundRename(e, x, z))
		}
		 else termAIsSubtermOfB(a, e)
	case Ap(f,e) => termAIsSubtermOfB(a, f) || termAIsSubtermOfB(a, e)
	case Var(x) => false
}

@tailrec
def manyStepsEval(a: Term): Option[Term] = {
	val newA = oneStepEval(a)
	if (compareTerms(a, newA)) Some(a)
	else if (termAIsSubtermOfB(a, newA)) None
	else manyStepsEval(newA)
}

val a0 = 'a -: 'a
val b1 = 'a -: 'a('a)
val b2 = b1(b1)
val c1 = 'a -: 'a('a)('a)
val c2 = c1(c1)

checkE(manyStepsEval(test1('c)) , Some(Var('c)), "two-steps eval")
checkE(('a!!) , Var('a), "one-steps eval")
checkE((test4!!) , test4, "one-steps eval of test4")
checkE((b2!!) , b2, "detection of finite loop in many-steps eval")
checkE((a0(a0)(a0)(a0)!!) , a0, "four-steps eval")
checkE(Try((c2!!)).toOption , None, "detection of infinite loop in many-steps eval")

def checkM(a: Term, b: Term, m: String): Unit = assert((a!!) === (b!!), s"failed: $m: got $a evaluated to ${a!!} vs $b evaluated to ${b!!}")
