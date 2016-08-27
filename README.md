# scala-lambda-calculus
This is a simple untyped lambda calculus interpreter in Scala using Ammonite Shell.

Use for pedagogical purposes only, because the code is not performance-optimized!
Especially slow is the test of summation of Church numbers using recursion and the Y-combinator.

The interpreter uses the "lazy" evaluation strategy.

# Prerequisites

You will need to install the [Ammonite shell](http://www.lihaoyi.com/Ammonite/)

This makes it easier to do scripting in Scala.

To install systemwide:

```
mkdir ~/.ammonite && curl -L -o ~/.ammonite/predef.sc https://git.io/vo4wx
sudo curl -L -o /usr/local/bin/amm https://git.io/vKSOR && sudo chmod +x /usr/local/bin/amm
```

# Run tests

```
amm lc0_spec.sc
amm lc1_spec.sc
```

There should be no errors (and no test output).

# Usage

Start the Ammonite shell:

```
amm
```

The Ammonite prompt usually ends with `@` (although it can be customized).

Import the basic lambda calculus script:

```
import $file.lc0, lc0._
```

Now you can use lambda calculus terms.

If you want to use the "standard library" (Booleans, Church numbers, product and sum types, and the Y-combinator), import `lc1.sc` in the same way.

## Syntax of lambda-terms

Free and bound variables must be prefixed with apostrophe. They must be valid Scala "symbols".
For example, `'a` or `'z123`.

Instead of the theoretical notation λx.e, write `'x -: e`. The lambda arrow is right-associative in Scala (because it ends with a colon). For example:
```
@ 'x -: 'y -: 'x
res1: Term = x → y → x
```

Terms are pretty-printed using a "short" notation.

Function application follows Scala syntax, for example: 
```
@ ('x -: 'y -: 'x)('a)('b)
res2: Term = (x → y → x) a b
```

You can declare Scala values to be terms, and you can use these values later.

```
@ val q = 'x -: 'x('x)
q: Term = x → x x
@ val q2 = q(q)
q2: Term = (x → x x) (x → x x)
```

## Evaluation

To evaluate terms, use the postfix operators `!` (for just one evaluation step) and `!!` ("full evaluation", making all possible evaluation steps).

The "full evaluation" will try to detect an infinite loop in evaluation. If it detects that the next evaluation step contains the previous step as a sub-term, an exception is thrown.
This divergence detection mechanism is primitive and only good for illustrative purposes.

The operator `!?` is the same as `!!` but returns an `Option[Term]`. It will return `None` if divergence was detected. The operator `!!` is defined as `!?` with the following `.get`.

For example, the term `q2` defined above is evaluated to itself after one step:

```
@ q2 !
res3: Term = (x → x x) (x → x x)
```

This is detected:

```
@ q2 !!
res4: Term = (x → x x) (x → x x)
```

The term `r2` shown below will grow unboundedly when evaluated:

```
@ val r = 'x -: 'x('x)('x)
r: Term = x → x x x
@ val r2 = r(r)
r2: Term = (x → x x x) (x → x x x)
@ r2!
res5: Term = (x → x x x) (x → x x x) (x → x x x)
@ res5!
res6: Term = (x → x x x) (x → x x x) (x → x x x) (x → x x x)
```

This is detected as a divergence. Evaluating with `!?` will return `None`, while evaluating with `!!` will throw an exception.

```
@ r2 !?
res7: Option[Term] = None
```

## Library

The file `lc0.sc` defines only the basic machinery of lambda calculus.

The file `lc1.sc` defines the standard combinators (`cS`, `cK`, `cI`) including the Y-combinator (`cY`), 
Booleans (`bFalse`, `bTrue`), Church numbers (`cZero`, `cSucc`, `cOne`, ...),
Cartesian product and co-product types (`tPair`, `tCase`), and their associated operations.

