import $file.lc1, lc1._
import $file.lc0_spec, lc0_spec.{checkC,checkE,checkM,checkMD}
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

// subtraction
checkC(cSub(cSix)(cTwo), cFour, "6-2 must be 4")

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

// convert Church numerals to Int
checkE(churchToInt(Var('x)), None, "x is not a Church numeral")
checkE(churchToInt('x -: 'x), None, "x -> x is not a Church numeral")
checkE(churchToInt('x -: 'y -: 'x), None, "x -> y -> x is not a Church numeral")
checkE(churchToInt('x -: 'y -: 'y('x)), None, "x -> y -> y x is not a Church numeral")

checkE(churchToInt(cZero), Some(0), "c0 => 0")
checkE(churchToInt(cOne), Some(1), "c1 => 1")
checkE(churchToInt(cTwo), Some(2), "c2 => 2")
checkE(churchToInt(cThree), Some(3), "c3 => 3")
checkE(churchToInt(cSix), Some(6), "c6 => 6")

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


// Recursive function: factorial of a Church number

checkC(cFact(cZero), cOne, "0! = 1")
checkC(cFact(cOne), cOne, "1! = 1")
// this is very slow!
checkC(cFact(cTwo), cTwo, "2! = 2")

// this is extremely slow!
// checkC(cFact(cThree), cSix, "3! = 6")


// Recursive type: single-linked list of lambda-terms

val lNil = tInl(cZero)
val lCons = 'x -: 'l -: tInr(tPair('x)('l))
val lIsEmpty = 'l -: tCase('l)(cK(bTrue))(cK(bFalse))
val lHead = 'l -: tCase('l)(cK(cZero))(tFst)
val lTail = 'l -: tCase('l)(cK(cZero))(tSnd)
// "case" for lists: if l is empty then e else evaluate f on head and tail of l
val lCase = 'l -: 'e -: 'f -: tCase('l)(cK('e))('p -: 'f(tFst('p))(tSnd('p)))

val list0 = lNil
val list1 = lCons('a)(lNil)
val list2 = lCons('b)(lCons('a)(lNil))
val list3 = lCons(cOne)(lCons(cOne)(lCons(cOne)(lNil)))

checkM(lIsEmpty(list0), bTrue, "empty list is empty")
checkM(lIsEmpty(list1), bFalse, "list [a] is not empty")
checkM(lHead(list1), Var('a), "head of [a] is a")
checkM(lTail(list2), list1, "tail of [b, a] is [a]")
checkM(lCase(list0)(cTwo)('a), cTwo, "lCase of [] is correct")
checkM(lCase(list1)(cTwo)('h -: 't -: 'h), 'a, "lCase of [a] head is correct")
checkM(lCase(list1)(cTwo)('h -: 't -: 't), lNil, "lCase of [a] tail is correct")

// Recursive function: compute length of list as Church numeral

val lLen = cY( 'r -: 'l -: lCase('l)(cZero)('h -: 't -: cSucc('r('t))) )

checkC(lLen(list0), cZero, "length of empty list is 0")
checkC(lLen(list3), cThree, "length of [1,1,1] is 3")

// Recursive function: concatenate lists

val lConcat = cY( 'r -: 'l1 -: 'l2 -: lCase('l1)('l2)('h-: 't -: lCons('h)('r('t)('l2))) )

// need a deeper comparison for lists

checkMD(lConcat(list0)(list3), list3, "[] ++ [1,1,1] = [1,1,1]")
checkMD(lConcat(list1)(list0), list1, "[a] ++ [] = [a]")
checkMD(lConcat(lCons('b)(lNil))(list1), list2, "[b] ++ [a] = [b,a]")
checkMD(lConcat(list3)(list0), list3, "[1,1,1] ++ [] = [1,1,1]")
