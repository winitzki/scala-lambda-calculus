import $file.lc0, lc0._

// Booleans

val bTrue = 'a -: 'b -: 'a
val bFalse = 'a -: 'b -: 'b
val bIf = 'c -: 't -: 'e -: 'c('t)('e)
val bAnd = 'x -: 'y -: bIf('x)('y)(bFalse)
val bNot = 'x -: bIf('x)(bFalse)(bTrue)
val bOr = 'x -: 'y -: bIf('x)(bTrue)('y)

val bXor = 'x -: 'y -: bIf('x)(bNot('y))('y)

// Curry's combinators

val cI = 'a -: 'a
val cK = 'a -: 'b -: 'a
val cS = 'a -: 'b -: 'x -: 'a ('x)('b('x))
val cIota = 'a -: 'a(cS)(cK)
// the iota combinator can be used to recover all of I, K, S

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

// repeat n times applying a function f to x
val cRepeat = 'n -: 'n // this is just eta-conversion of identity, let's inline it

val cAdd = 'a -: 'b -: 'a(cSucc)('b)
val cMul = 'a -: 'b -: 'x -: 'a ('b('x))
val cExp = 'a -: 'b -: 'b('a)

// Cartesian product type 
val tPair = 'p -: 'q -: 'f -: 'f('p)('q)
val tFst = 'p -: 'p(bTrue)
val tSnd = 'p -: 'p(bFalse)

// predecessor of a Church numeral
// compute Pair (x+1) x from Pair x _
val cNextPair = 'p -: tPair(cSucc(tFst('p)))(tFst('p))
val cPred = 'c -: tSnd('c(cNextPair)(tPair(cZero)(cZero)))

// arithmetic inequalities

val cIsZero = 'c -: 'c(bFalse)(bNot)(bFalse)
val cIsGeq = 'n -: 'm -: cIsZero('n(cPred)('m))

// disjoint union type
val tInl =  'x -: 'f -: 'g -: 'f('x)
val tInr =  'x -: 'f -: 'g -: 'g('x)
val tCase = 's -: 'f -: 'g -: 's('f)('g)

// Y-combinator

val cY = 'f -: ('x -: 'f('x('x)))('x -: 'f('x('x)))
