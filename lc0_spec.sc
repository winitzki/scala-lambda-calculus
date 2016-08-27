import $file.lc0, lc0._
import scala.util.Try

// shallow (syntactic) equality

def checkE(a: Any, b: Any, m: String): Unit = assert(a == b, s"failed: $m: got $a vs $b")

// Deep equality up to alpha-conversions.
def checkA(a: Term, b: Term, m: String): Unit = assert(a === b, s"failed: $m: got $a vs $b")

// Deep equality up to multiple evaluations and alpha-conversions.
def checkM(a: Term, b: Term, m: String): Unit = {
	assert((a!!) === (b!!), s"failed: $m: got $a evaluated to ${a!!} vs $b evaluated to ${b!!}")
}

// Deep equality up to multiple evaluations on Church numbers.
// To test equality Church numbers, we need to supply two arguments to each number, or else nothing is evaluated under lambdas.
def checkC(a: Term, b: Term, m: String): Unit = {
	assert((a('x)('y)!!) === (b('x)('y)!!), s"failed: $m: got $a evaluated to ${a('x)('y)!!} vs $b evaluated to ${b('x)('y)!!}")
}

// basic syntax
checkE('a -: 'b , Lam('a, Var('b)), "lambda is converted from syntax")
checkE( ('a -: 'a)('b) , Ap(Lam('a, Var('a)), Var('b)) , "application is converted from syntax" )
checkE('a -: 'b -: 'a , Lam('a, Lam('b, Var('a))), "lambda is right-associative")
checkE('a -: 'b('a) , Lam('a, Ap(Var('b), Var('a))), "lambda binds looser than application")
checkE('a('b)('c) , Ap(Ap(Var('a), Var('b)), Var('c)), "application is left-associative")

// shallow equality
checkA( 'a , 'a, "var is equal to itself")
checkA( 'a -: 'a , 'a -: 'a, "lambda is equal to itself")
checkA( 'a -: 'a , 'b -: 'b, "lambda is equal to itself up to alpha-conversion")
assert( ! ('a -: 'a  == 'a -: 'b), "lambda is equal to another lambda")
assert( ! ('a -: 'a == 'b -: 'a), "lambda is equal to another lambda with free variable")
assert( ! ('b -: 'a == 'a -: 'a), "lambda is equal to another lambda with free variable b")
assert( ! ('b -: 'a == 'b -: 'b), "lambda is equal to another lambda with free variable a")
checkA( 'b -: 'a , 'c -: 'a, "lambda is equal to another lambda with free variable and alpha conversion")
checkA( ('a-:'a)('b-:'b), ('c-:'c)('d-:'d), "two applications are equal")

// compute free variables of a term
checkE(Term.fv('a) , Set('a), "correct freevars of variable")
checkE(Term.fv('a -: 'b) , Set('b), "correct freevars of lambda with unbound var")
checkE(Term.fv('a -: 'a) , Set(), "correct freevars of lambda with bound var")
checkE(Term.fv('a -: 'a('b)('c)) , Set('b,'c), "correct freevars of lambda with bound and unbound var")
checkE(Term.fv(('a-:'b)('c)) , Set('b,'c), "correct freevars of application")

// create a new symbol that does not clash
checkE(Term.newSymbol(Set()) , 'x0, "first new symbol should be 'x0")
checkE(Term.newSymbol(Set('a, 'b, 'x0,'x1, 'x3)) , 'x2, "new symbol should be 'x2")

// pretty-printer
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


// one-step evaluation
checkE(('a!) , Var('a), "unary syntax eval of variable")
val test1 = 'a -: ('x -: 'x)('a)
checkE((test1!) , test1, "eval of lambda")
checkE(( ('a -: 'a)('b) !) , Var('b), "eval of identity on variable")
val test2 = ('x -: 'x)('c)
checkE(( test1('c) !) , test2, "one step eval of test1")
checkE((test2!) , Var('c), "second step eval of test1")

val test3 = 'a -: 'b -: 'b('a)
val test4 = 'z -: 'b
checkE((test3(test4)!) , 'x0 -: 'x0(test4), "alpha conversion")

// several alpha-conversions required
checkE( ( 'a -: 'b -: 'c -: 'a('b)('c) )('x -: 'y -: 'b('c)) !, 'x0 -: 'x1 -: ('x -: 'y -: 'b('c))('x0)('x1), "perform two alpha-conversions")

// loop detection
val a0 = 'a -: 'a
val b1 = 'a -: 'a('a)
val b2 = b1(b1)
val c1 = 'a -: 'a('a)('a)
val c2 = c1(c1)

checkE((test1('c) !?) , Some(Var('c)), "two-steps eval")
checkE(('a!!) , Var('a), "one-steps eval")
checkE((test4!!) , test4, "one-steps eval of test4")
checkE((b2!!) , b2, "detection of finite loop in many-steps eval")
checkE((a0(a0)(a0)(a0)!!) , a0, "four-steps eval")
checkE(Try((c2!!)).toOption , None, "detection of infinite loop in many-steps eval")
