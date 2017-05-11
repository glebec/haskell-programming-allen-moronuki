# 1: All You Need is Lambda

Side note: [Raymond Smullyan](https://en.wikipedia.org/wiki/Raymond_Smullyan) wrote a quirky book on combinatory logic, [To Mock a Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird), in which functions are presented as various birds which sing the "name" or description of other birds. David C. Keenan shows some examples in a more graphical format in [To Dissect a Mockingbird](http://dkeenan.com/Lambda/). As Haskell Curry was an avid birdwatcher, it is fitting that various [combinators](http://www.angelfire.com/tx4/cus/combinator/birds.html) have therefore come to be known by names suck as Mockingbird, Blackbird, etc.

## Some lambda calculus jargon

* λ-abstraction: an anonymous function
	* `λh.b`: `λh` is the head, `b` is the body, `h` is a parameter
	* arguments are concrete values a function is applied to
	* Called λ-*abstraction* because abstracts over the concrete function domain
* α-conversion & α-equivalence: renaming variables does not change a function
	* example: `λab.a+b` is α-equivalent to `λxy.x+y`
* β-reduction: application of a function to its arguments
	* example: `(λx.x - 2) 5` -> `5 - 2`
	* left-associative
	* normal order: apply leftmost outermost, then reduce inner terms
		* β normal form: fully β-reduced
	* convergent: β-reduction eventually terminates
	* divergent: β-reduction never terminates
		* example: `(λa.aa)(λa.aa)` (the omega term)
* η-conversion (eta-conversion)
	* η-abstraction: adding a (no-op) wrapper function (useless)
		* example: `M` -> `λx.Mx`
	* η-reduction: removal of redundant wrapper functions (towards point-free)
		* example: `λx.Mx` -> `M`
		* leads to point-free notation ("points" are parameters)
			* example: `\xs -> map negate xs` -> `map negate`
* free variables: variables in the body but not bound in the head
* combinator: a lambda term with no free variables (every term in the body occurs in the head)

## Other Notes

> _"The λ calculus can be called the smallest universal programming language of the
world"_ — http://www.inf.fu-berlin.de/lehre/WS03/alpi/lambda.pdf
