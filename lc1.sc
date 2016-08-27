import $file.lc0, lc0._

//load.module("lc0.sc")

// Booleans

val bTrue = 'a -: 'b -: 'a
val bFalse = 'a -: 'b -: 'b
val bIf = 'c -: 't -: 'e -: 'c('t)('e)
val bAnd = 'x -: 'y -: bIf('x)('y)(bFalse)
val bNot = 'x -: bIf('x)(bFalse)(bTrue)
val bOr = 'x -: 'y -: bIf('x)(bTrue)('y)

val bXor = 'x -: 'y -: bIf('x)(bNot('y))('y)

checkM(bOr(bTrue)(bTrue) ,  bTrue, "'or' operation is correct")
checkM(bXor(bTrue)(bTrue) ,  bFalse, "'xor' operation is correct")

// Curry's combinators

val cI = 'a -: 'a
val cK = 'a -: 'b -: 'a
val cS = 'a -: 'b -: 'x -: 'a ('x)('b('x))
val cIota = 'a -: 'a(cS)(cK)

checkM(cS(cK)(cK(cK))('a), 'a, "S K (K K) == I")
checkM(cS(cK)(cS)(cK), cK, "S K S K == K")
checkM(cS(cK)(cK)('a), cI('a), "S K K == I")  // need eta-expansion here

// the iota combinator can be used to recover all of I, K, S
checkM(cIota(cIota)('a), cI('a), "i i = I")
checkM(cIota(cIota(cI)), cK, "i (i I) = K")
checkM(cIota(cK), cS, "i K = S")

// Church numerals

val cZero =  's -: 'z -: 'z
val cOne =   's -: 'z -: 's('z)
val cSucc =  'c -: 's -: 'z -: 's('c('s)('z))
val cTwo =   's -: 'z -: 's('s('z))
val cThree = 's -: 'z -: 's('s('s('z)))
val cFour =  's -: 'z -: 's('s('s('s('z))))
val cFive =  's -: 'z -: 's('s('s('s('s('z)))))
val cSix =   's -: 'z -: 's('s('s('s('s('s('z))))))
val cEight = 's -: 'z -: 's('s('s('s('s('s('s('s('z))))))))

// to test Church numbers, we need to supply two arguments to each number, or else nothing is evaluated under lambdas
checkM(cSucc(cZero) ('a)('b) ,  cOne('a)('b), "Church numeral One works")
checkM(cSucc(cSucc(cZero)) ('a)('b),  cSucc(cOne)('a)('b), "Church numeral Succ(One) works")
checkM(cSucc(cSucc(cZero)) ('a)('b),  cTwo('a)('b), "Church numeral Two works")

// repeat n times applying a function f to x
val cRepeat = 'n -: 'n // this is just eta-conversion of identity, let's inline it

val cAdd = 'a -: 'b -: 'a(cSucc)('b)
val cMul = 'a -: 'b -: 'x -: 'a ('b('x))
val cExp = 'a -: 'b -: 'b('a)

checkM(cAdd(cTwo)(cThree)('a)('b), cFive('a)('b), "2+3=5")
checkM(cMul(cTwo)(cThree)('a)('b), cSix('a)('b), "2*3=6")
checkM(cExp(cTwo)(cTwo)('a)('b), cFour('a)('b), "2^2=4")
checkM(cExp(cTwo)(cThree)('a)('b), cEight('a)('b), "2^3=8")

// product type 
val tPair = 'p -: 'q -: 'f -: 'f('p)('q)
val tFst = 'p -: 'p(bTrue)
val tSnd = 'p -: 'p(bFalse)

checkM( tFst(tPair('p)('q)) ,  'p, "first of pair")
checkM( tSnd(tPair('p)('q)) ,  'q, "second of pair p q")
checkM( tSnd(tPair('q)('p)) ,  'p, "second of pair q p")

// predecessor of Church numerals
// compute Pair (x+1) x from Pair x _
val cNextPair = 'p -: tPair(cSucc(tFst('p)))(tFst('p))
val cPred = 'c -: tSnd('c(cNextPair)(tPair(cZero)(cZero)))

checkM(cPred(cZero)('a)('b), cZero('a)('b), "pred(0) must be 0")
checkM(cPred(cOne)('a)('b), cZero('a)('b), "pred(1) must be 0")
checkM(cPred(cTwo)('a)('b), cOne('a)('b), "pred(2) must be 1")
checkM(cPred(cSix)('a)('b), cFive('a)('b), "pred(6) must be 5")

// arithmetic inequalities

val cIsZero = 'c -: 'c(bFalse)(bNot)(bFalse)
val cIsGeq = 'n -: 'm -: cIsZero('n(cPred)('m))

// sum type
val tInl =  'x -: 'f -: 'g -: 'f('x)
val tInr =  'x -: 'f -: 'g -: 'g('x)
val tCase = 's -: 'f -: 'g -: 's('f)('g)

// given a disjoint sum of a Church numeral and a Boolean, return True if either the Church numeral is zero, or the Boolean is false

val prog1 = 's -: tCase('s)(cIsZero)(bNot)

// tests

checkM(prog1(tInl(cOne)), bFalse, "prog1(c1 + 0) = false")
checkM(prog1(tInl(cZero)), bTrue, "prog1(c0 + 0) = true")
checkM(prog1(tInr(bTrue)), bFalse, "prog1(0 + True) = false")
checkM(prog1(tInl(bFalse)), bTrue, "prog1(0 + False) = true")
