import scala.annotation.tailrec

sealed trait Term {
	def apply(a:Term) : Term = Ap(this, a)
	def -:(x: Symbol): Term = Lam(x,this)  // right-associative by virtue of the trailing colon
	override def toString: String = Term.printTerm(this, Term.Neutral)
	def ! : Term = Term.oneStepEval(this)
	def !? : Option[Term] = Term.manyStepsEval(this)
	def !! : Term = Term.manyStepsEval(this).get
	def === (other: Term): Boolean = Term.compareTerms(this, other)
	def size: Int = Term.termSize(this)
}
case class Lam(x: Symbol, body: Term) extends Term
case class Ap(f: Term, a: Term) extends Term
case class Var(x: Symbol) extends Term

implicit def fromVar(x: Symbol): Term = Var(x)

object Term {

	def termSize(a: Term): Int = a match {
		case Lam(x,e) => termSize(e) + 1
		case Ap(f,e) => termSize(f) + termSize(e) + 1
		case Var(x) => 1
	}

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

	// compare terms and evaluate everything under lambda
	def compareTermsWithDeepEval(a: Term, b: Term): Boolean = if (a==b) true else a match {
		case Lam(x,e) => b match {
			case Lam(y,f) => if (x==y) compareTermsWithDeepEval(e!!,f!!) else {
				val z = newSymbol(fv(e) ++ fv(f))
				compareTermsWithDeepEval(boundRename(a, x, z), boundRename(b, y, z))
			}
			case _ => false
		}
		case Ap(f,e) => b match {
			case Ap(g,h) => compareTermsWithDeepEval(f!!, g!!) && compareTermsWithDeepEval(e!!, h!!)
			case _ => false
		}
		case Var(x) => false // if it were the same variable, we would have picked that up with literal comparison
	}

	def fv(t: Term): Set[Symbol] = t match {
		case Lam(x,e) => fv(e) - x
		case Ap(f,e) => fv(f) ++ fv(e)
		case Var(x) => Set(x)
	}

	def newSymbol(old: Set[Symbol]): Symbol = {
		Stream.from(0).map(i => Symbol(s"x$i")).find( s => !(old contains s) ).get
	}

	sealed trait Location
	case object Left extends Location
	case object Right extends Location
	case object Neutral extends Location

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

	def oneStepEval(t: Term): Term = evalLazyHeadFirstThenArgs(t)

	def evalLazyHeadFirstThenArgs(t: Term): Term = t match {
		case Ap(a, b) => {
			val newA = evalLazyHeadFirstThenArgs(a)
			if (newA != a) Ap(newA, b)
			else
			// if we are here, we can't evaluate a to anything else
			a match {
				case Lam(x,e) => subst(e, x, b)
				case _ => Ap(a, evalLazyHeadFirstThenArgs(b))	// can't evaluate t, so let's at least eval b
			}
		}
		case _ => t // lambdas are not evaluated at all unless applied; free variables are never evaluated
	}

	def evalLazyHeadFirst(t: Term): Term = t match {
		case Ap(a, b) => {
			val newA = evalLazyHeadFirst(a)
			if (newA != a) Ap(newA, b)
			else
			// if we are here, we can't evaluate a to anything else
			a match {
				case Lam(x,e) => subst(e, x, b)
				case _ => t
			}
		}
		case _ => t // lambdas are not evaluated at all unless applied; free variables are never evaluated
	}

	def evalEagerHeadFirst(t: Term): Term = t match {
		case Ap(a, b) => {
			val newA = evalEagerHeadFirst(a)
			if (newA != a) Ap(newA, b) else {
				val newB = evalEagerHeadFirst(b)
				if (newB != b) Ap(a, newB) else a match {
					case Lam(x,e) => subst(e, x, b)
					case _ => t
				}
			}
		}
		case _ => t // lambdas are not evaluated at all unless applied; free variables are never evaluated
	}

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

}

true // avoid error when running this script by itself
