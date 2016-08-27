# scala-lambda-calculus
This is a simple untyped lambda calculus interpreter in Scala using Ammonite Shell.
Use for pedagogical purposes only. (Not performance-optimized!)

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
amm lc0.sc
amm lc1.sc
```

There should be no errors (and no test output).

# Usage

Start the Ammonite shell:

```
amm
```

The Ammonite prompt usually ends with `@` (although it can be customized).

Import the lambda calculus script:

```
import $file.lc0, lc0._
```

Now you can use lambda calculus terms.

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

To evaluate terms, use the postfix operators `!` (for just one evaluation step) and `!!` ("full evaluation", making all possible evaluation steps).
The "full evaluation" will try to detect an infinite loop in evaluation. If it detects that the next evaluation step contains the previous step as a sub-term, an exception is thrown.
The operator `!?` is the same as `!!` but returns an `Option[Term]`. It will return `None` if divergence was detected.

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

