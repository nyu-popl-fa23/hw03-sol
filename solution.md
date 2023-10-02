## Problem 1

(a)

_ (*) _ : N x N -> N
 <> (*) y = <>
<x> (*) y = (x (*) y) + y


(b)

Proof by structural induction on x.

Base case x = <>:

<x> = <<>>              by assumption on x
    = <> (+) <<>>       by def. of (+)
    = x + <<>>          by assumption on x

Induction case x = <x'> for some x' in N:

Induction hypothesis: <x'> = x' (+) <<>>

<x> = <<x'>>          by assumption on x
    = <x' + <<>>      by induction hypothesis
    = <x'> + <<>>     by def. of (+)
    = x + <<>>        by assumption on x

(c)

filter: ZZ x List -> List
filter(n, <>) = <>
filter(n, hd :: tl) = hd :: filter(n, tl)  if n <= hd
filter(n, hd :: tl) = filter(n, tl)        if n > hd

(f)

Proof by structural induction on l.

Base case l = <>:

  reverse(reverse(l))
= reverse(reverse(<>))      by assumption on l
= reverse(<>)               by def. of reverse
= <>                        by def. of reverse

Step case l = hd :: tl for some hd in ZZ and tl in List:

Induction hypothesis: reverse(reverse(tl)) = tl

  reverse(reverse(l))
= reverse(reverse(hd :: tl))                  by assumption on l
= reverse(append(reverse(tl), hd :: nil))     by def. of reverse
= append(reverse(hd :: nil), reverse(reverse(tl))) by assumed equality
= append(reverse(hd :: nil), tl)              by induction hypothesis
= append(append(reverse(nil), hd :: nil), tl) by def. of reverse
= append(append(nil, hd :: nil), tl)          by def. of reverse
= append(hd :: nil, tl)                       by def. of append
= hd :: append(nil, tl)                       by def. of append
= hd :: tl                                    by def. of append
= l                                           by assumption on l


## Problem 2

(a)

String to number conversion:
    1 * "2" -> 2
    1 * "1.23e-1" -> 0.123
    1 * "banana" -> NaN
    1 * "" -> 0
Boolean to number conversion:
    1 * true -> 1
    1 * false -> 0
String to Boolean conversion:
    ! "true" -> false
    ! "false" -> false
    ! "banana" -> false
    ! "" -> true
Number to Boolean conversion:
    ! 0 -> true
    ! NaN -> true
    ! 3 -> false
    ! -3 -> false
  
The general conversion rules are as follows

* Strings to numbers: if the given string is the numeral
  representation of a number (either in decimal or scientific floating
  point notation), then the string is converted to the represented
  number. If the given string is empty, then it is converted to the
  number 0. In all other cases, the string is converted to the
  floating point number `NaN` (not a number).

* Booleans to numbers: the value `true` is converted to the number 1
    and the value `false` to the number 0.

* Strings to Booleans: the empty string is converted to the value
    `false`. All other strings are converted to the value
    `true`.

* Numbers to Booleans: the numbers 0 and `NaN` are converted to the value `false`. All
    other numbers are converted to the value `true`.


(b)

"1" + 2 -> "12"
1 + "2" -> "12"
"hello" + "world" -> "helloworld"
1 + 2 -> 3
1 + true -> 2
true + 1 -> 2
true + "1" -> "true1"

The general rule for evaluating an expression `e1 + e2` is as follows:
first `e1` and `e2` are evaluated. If either of the resulting values
is a string, then both intermediate values are converted to strings
and the two strings are concatenated. In all other cases (i.e., if
both subexpressions evaluate to non-string values), the obtained
intermediate values are converted to numbers and the two numbers are
added.

(c)

true && true -> true
false && true -> false
true && 3 -> 3
false && 3 -> false
0 && 3 -> 0
1 && 3 -> 3
"hello" && "hello" -> "hello"
"" && 3 -> ""

true || false -> true
false || false -> false
1 || 3 -> 1
false || 3 -> 3
false || "hello" -> "hello"
true || "hello" -> true

The general rule for `e1 && e2` is as follows. First `e1` is evaluated
and the resulting value `v1` is converted to a Boolean. If the
obtained Boolean value is `true`, the result of the whole expression
is the result of evaluating `e2` (without conversion). If the
converted result of `e1` is `false` then the whole expression
evaluates to `v1` (the result of `e1` before conversion to a Boolean).

The general rule for `e1 || e2` is the logical dual of the rule for
&&. First `e1` is evaluated and the resulting value `v1` is converted
to a Boolean. If the obtained Boolean value is `false`, the result of
the whole expression is the result of evaluating `e2` (without
conversion). If the converted result of `e1` is `true` then the whole
expression evaluates to `v1` (the result of `e1` before conversion to
a Boolean).




