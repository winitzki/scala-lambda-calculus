import $file.lc1, lc1._
import $file.lc0_spec, lc0_spec.{checkM,checkC}
import lc0._

// Booleans

checkM(bOr(bTrue)(bTrue) ,  bTrue, "'or' operation is correct")
checkM(bXor(bTrue)(bTrue) ,  bFalse, "'xor' operation is correct")

// Curry's combinators

checkM(cS(cK)(cK(cK))('a), 'a, "S K (K K) == I")
checkM(cS(cK)(cS)(cK), cK, "S K S K == K")
checkM(cS(cK)(cK)('a), cI('a), "S K K == I")  // need eta-expansion here

// the iota combinator can be used to recover all of I, K, S
checkM(cIota(cIota)('a), cI('a), "i i = I")
checkM(cIota(cIota(cI)), cK, "i (i I) = K")
checkM(cIota(cK), cS, "i K = S")

// Church numerals

checkC(cSucc(cZero)  ,  cOne, "Church numeral One works")
checkC(cSucc(cSucc(cZero)) ,  cSucc(cOne), "Church numeral Succ(One) works")
checkC(cSucc(cSucc(cZero)) ,  cTwo, "Church numeral Two works")

checkC(cAdd(cTwo)(cThree), cFive, "2+3=5")
checkC(cMul(cTwo)(cThree), cSix, "2*3=6")
checkC(cExp(cTwo)(cTwo), cFour, "2^2=4")
checkC(cExp(cTwo)(cThree), cEight, "2^3=8")

// product type 

checkM( tFst(tPair('p)('q)) ,  'p, "first of pair")
checkM( tSnd(tPair('p)('q)) ,  'q, "second of pair p q")
checkM( tSnd(tPair('q)('p)) ,  'p, "second of pair q p")

// predecessor of Church numerals

checkC(cPred(cZero), cZero, "pred(0) must be 0")
checkC(cPred(cOne), cZero, "pred(1) must be 0")
checkC(cPred(cTwo), cOne, "pred(2) must be 1")
checkC(cPred(cSix), cFive, "pred(6) must be 5")

// arithmetic inequalities

val cIsZero = 'c -: 'c(bFalse)(bNot)(bFalse)
val cIsGeq = 'n -: 'm -: cIsZero('n(cPred)('m))

checkM(cIsZero(cZero), bTrue, "c0 is zero")
checkM(cIsZero(cOne), bFalse, "c1 is not zero")
checkM(cIsGeq(cZero)(cZero), bTrue, "c0 >= c0")
checkM(cIsGeq(cZero)(cOne), bFalse, "c0 < c1")
checkM(cIsGeq(cOne)(cZero), bTrue, "c1 >= c0")
checkM(cIsGeq(cSix)(cZero), bTrue, "c6 >= c0")
checkM(cIsGeq(cZero)(cSix), bFalse, "c0 < c6")
checkM(cIsGeq(cSix)(cSix), bTrue, "c6 >= c6")

// sum type

// sample program: 
// given a disjoint sum of a Church numeral and a Boolean, return True if either the Church numeral is zero, or the Boolean is false

val prog1 = 's -: tCase('s)(cIsZero)(bNot)

checkM(prog1(tInl(cOne)), bFalse, "prog1(c1 + 0) = false")
checkM(prog1(tInl(cZero)), bTrue, "prog1(c0 + 0) = true")
checkM(prog1(tInr(bTrue)), bFalse, "prog1(0 + True) = false")
checkM(prog1(tInl(bFalse)), bTrue, "prog1(0 + False) = true")

// Recursive function: sum all Church numbers from 1 to n

val cSum = {
	val cRec = 'r -: 'n -: bIf(cIsZero('n))(cZero)(cAdd('n)('r(cPred('n))))
	cY(cRec)
}

checkC(cSum(cZero), cZero, "sum from 0 to 0 = 0")
checkC(cSum(cOne), cOne, "sum from 0 to 1 = 1")
checkC(cSum(cTwo), cThree, "sum from 0 to 2 = 3")
checkC(cSum(cThree), cSix, "sum from 0 to 3 = 6")
