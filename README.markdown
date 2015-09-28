# Caramel

          🍮 A modern syntax for the oldest programming language in the world. 🍮

Caramel is a set of bidirectional, Haskell-inspired **syntax-sugars** that are expanded to, and contracted from, [λ-Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) terms. Caramel is not a new programming language - it is a new syntax for an old language, enabling it to be written in a much saner way. The implementation aims to be simple and terse and currently stands at around 350 lines of commented Haskell code.

## Example

The following Caramel (`.mel`) file implements QuickSort on the Lambda Calculus:

```haskell
-- example_qsort.mel

qsort          = (match cons nil)
    nil        = []
    cons x xs  = (flatten [smalls,[x],bigs])
        smalls = (qsort (filter (< x) xs))
        bigs   = (qsort (filter (> x) xs))

qsort_example = (fix qsort [5,6,1,4,3,7,2])
```

You can run it by typing `mel qsort_example` on the Prelude directory. It looks for the `qsort_example` definition through all `.mel` files on the dir, expands it to a pure Lambda Calculus term, evaluates, and then prints the result on the Caramel syntax:

```haskell
$ cd Prelude
$ mel qsort_example
[1,2,3,4,5,6,7]
```

If you want to see the actual Lambda Calculus code, just append `.lam` to the name of the term you want to evaluate - it will output the unsugared view of the term, using [De Bruijn Index](https://en.wikipedia.org/wiki/De_Bruijn_index):

```bash
$ mel qsort_example.lam
λλ((1 λλ(1 0)) 
  ((1 λλ(1 (1 0))) 
  ((1 λλ(1 (1 (1 0))))
  ((1 λλ(1 (1 (1 (1 0))))) 
  ((1 λλ(1 (1 (1 (1 (1 0)))))) 
  ((1 λλ(1 (1 (1 (1 (1 (1 0))))))) 
  ((1 λλ(1 (1 (1 (1 (1 (1 (1 0)))))))) 
    0 )))))))
# (This is how [1,2,3,4,5,6,7] is usually represented on the Lambda Calculus.)
# (Identation added manually.)
```

In a future, you'll be also able to compile your programs to other languages such as JavaScript, Haskell and Scheme using `mel my_program.js`, for example.

More examples can be found on the Prelude directory, with the "example" prefix. I suggest looking at [this one](https://github.com/MaiaVictor/caramel/blob/master/Prelude/example_many_things.mel) first.
See [`example_qsort.mel`](https://github.com/MaiaVictor/caramel/blob/master/Prelude/example_qsort.mel) for more on the QuickSort example. Mind most of the remaining of files there were written before Caramel and need some refactoring to better show Caramel's syntax.

# Featured syntax-sugars

  * [Lambdas](#lambdas)
  * [Application](#application)
  * [Let and variable assignment](#let-and-variable-assignment)
  * [Where and layout syntax](#where-and-layout-syntax)
  * [Top-level definitions](#top-level-definitions)
  * [Natural Numbers](#natural-numbers)
  * [Lists](#lists)
  * [Tuples](#tuples)
  * [Chars](#chars)
  * [Strings](#strings)
  * [Words](#words)
  * [Comments](#comments)
  * [ADTs](#adts)

## Lambdas

Lambda Calculus's lambdas are anonymous, single argument functions. Caramel's lambda syntax allows creating anonymous functions with multiple named variables separated from the body by an arrow. 

Example:

```haskell
(a b c -> (a (b c)))
```

Expands to/from the λ-calculus as:

```haskell
λλλ(2 (1 0))
```

(Remember numbers on the λ-calculus expressions are bruijn-indexed variables, not natural numbers.)

## Application

Lambda Calculus applications substitutes bound variables of a lambda abstraction by the applied term. Caramel's application syntax uses parenthesis and allows you to omit redundant ones. Using f, x and y as placeholders:

```haskell
(f x y z)
```

Expands to/from:

```haskell
λλλ(((f x) y) z)
```

## Let and variable assignment

Let expressions are syntax for the assignment of variables. As opposed to Haskell, the `let` keyword isn't used, but `;`-separated assignments enclosed by curly brackets, followed by the expression on which those definitions are visible. They also allow named function definitions exactly like Haskell. They are recursive, so you don't have to care about the order you write them. Self-referential terms aren't made recursive - they instead gain an extra bound variable in order to be used with fixed-point combinators. Let expressions are expanded to lambda applications.

```haskell
{double   = (mul 2); 
 square x = (mul x x); 
 (double (square 3))}
```

Is the same as:

```haskell
((double -> ((square -> (double (square 3))) (x -> (mul x x)))) (mul 2))
```

## Where and Layout syntax

Properly idented newlines after a term are interpreted as local definitions and nicely expanded to "let" expressions. This enables a layout syntax very similar to Haskell's `where` clauses, although without the `where` keyword.

```haskell
(double (square 3)) 
    double   = (mul 2)
    square x = (mul x x)
```

This is the same as the `let` expression above. See [this example](...) for more info.

## Top-level definitions

Top-level definitions are just expanded to the implicit `where` (and, thus, `let`) syntax. Example:

```haskell
-- some top level variables
a = 3
b = 2

-- an example program
example_tld = (mul a b)
```

By calling `mel example_tld`, the above program is expanded as follows:

```haskell
example_tld
    a = 3
    b = 2
    example_tld = (mul a b)
...
{a = 3; b = 2; example_tld = (mul a b); example_tld}
...
((a -> ((b -> ((example_tld -> example_tld) (mul a b))) 2)) 3)
...
(λ(λ(λ(λ0 ((2 λλ(1 (1 (1 0)))) λλ(1 (1 0)))) λλ(1 (1 0))) λλ(1 (1 (1 0)))) λλλ(2 (1 0)))
...
λλ(1 (1 (1 (1 (1 (1 0))))))
```

Which is printed as `6`, the result of `3 * 2`.

## Natural Numbers

The simplest implementation of natural numbers on the Lambda Calculus is the church encoding. Caramell's Nat syntax is just the number literal in ASCII, which is expanded to church-encoded numbers, allowing you to input them on the lambda calculus without dealing with the encoding yourself. It is just an input method - church numbers can be converted to any format at compile time, in case you don't want them.

```haskell
3
```

Expands to/from the Lambda Calculus as:

```haskell
λλ(1 (1 (1 0)))
```

## Lists

The simplest implementation of lists on the Lambda Calculus is, too, the church encoding. Similarly to natural numbers, Camamell's Lst syntax is the same as Haskell and allows you to input lists on the Lambda Calculus without having to deal with the church encoding. After inputing, those can always be converted to any format you like, such as the Scott Encoding. Using, a, b and c as placeholders:

```haskell
[a, b, c]
```

Is the same as:

```haskell
(cons nil -> (cons a (cons b (cons c nil))))
```

And expands to/from the Lambda Calculus as:

```haskell
λλ(1 a (1 b (1 c 0)))
```

## Tuples

Very similarly to natural numbers and lists, tuples have a natural implementation based on the Church encoding. Caramel's tuple syntax for tuples is very similar to the application syntax, in that it uses parenthesis. The key difference is the presence of colons - with colons, it is an application, without them, it is an application. If it has only one element (e.g, `(7)`), is is an 1-tuple, not a redundant paren!

```haskell
(a, b, c)
```

Is the same as:

```haskell
(t -> (a b c))
```

And expands to/from the Lambda Calculus as:

```haskell
λ(((0 a) b) c)
```

## Chars

Although unconventional, chars can be encoded as functions of two variables that return octuples. For example, the char 'a', which is 97 in ASCII and 01100001 in binary, can be represented as the Haskell function (\ i o t -> (t o i i o o o o i)). I'm not sure this is a good idea, though, and am considering replacing by a 8-tuple of bools, since that can be encoded on the ADT system.  The caramel syntax uses is identical to Haskell's char syntax and does that expansion. The variable "i" was chosen to come before "o" in the argument list because it makes the bruijn indices on the lambda calculus version look exactly like the bitstring for the ASCII.

```haskell
'a'
```

Is the same as:

```haskell
(i o t -> (t o i i o o o o i)) 
```

And expands to/from the Lambda Calculus as:

```haskell
λλλ((((((((2 0) 1) 1) 0) 0) 0) 0) 1)
```

## Strings

Strings are just lists of chars.

```haskell
"abcd"
```

The program above is expanded as:

```haskell
['a', 'b', 'c', 'd']
...
(cons nil -> (cons 'a' (cons 'b' (cons 'c' (cons 'd' nil)))))
...
λλ  ((1 λλλ((((((((2 0) 1) 1) 0) 0) 0) 0) 1))
    ((1 λλλ((((((((2 0) 1) 1) 0) 0) 0) 1) 0))
    ((1 λλλ((((((((2 0) 1) 1) 0) 0) 0) 1) 1)) 
    ((1 λλλ((((((((2 0) 1) 1) 0) 0) 1) 0) 0)) 
        0 ))))
```

## Words

Words are 32-bit unsigned inters. The encoding is the same as chars, except the function of 2 arguments returns a 32-tuple instead of an 8-tuple. The syntax is a hash symbol (`#`) followed by a number literal. It is a minor shortcut since one could already write a lambda calculus function called `#` which would convert a church number to a word, and just write (# 123) instead of #123. 123 in binary is 00000000 00000000 00000000 01111011, so...

```haskell
#123
```

Expands to/from the Lambda Calculus as:

```haskell
λλλ((((((((((((((((((((((((((((((((2 
    0) 0) 0) 0) 0) 0) 0) 0) 
    0) 0) 0) 0) 0) 0) 0) 0) 
    0) 0) 0) 0) 0) 0) 0) 0) 
    0) 1) 1) 1) 1) 0) 1) 1)
```

## Comments

Anything between double hyphen (`--`) and a newline is ignored by the compiler.

```haskell
-- Hello I'm a comment
a = 7 -- I'm too
```

## ADTs

First-class Algebraic DataTypes (ADTs) are experimental and the most complex feature here. The point is that defining functions for a datatype on the λ-calculus is hard when you have to deal with church/scott encodings yourself. Combining ADTs with **high-order derivers**, many functions such as (on the List case) `cons`, `nil` (constructors), `head`, `tail` (getters), `map`, `zipWith` (some algorithms), as well as lenses, monads and so on, can be derived automatically. While this already works (for Scott encodings only), it is very likely that my design is imperfect and there are better solutions. Example:

In Haskell, we have:

```haskell
data Bool   = True | False deriving Show
data List a = Cons { head :: a, tail :: List a } | Nil deriving Show
data Tree a = Node a (List (Tree a)) deriving Show
type TLB    = Tree (List Bool)
```

In Caramel, we have:

```haskell
Bool   = #{True {} | False {}}
List a = #{Cons {head : a, tail : *} | Nil {}}
Tree a = #{Tree {tag : a, children : (List *)}}
TLB    = (Tree (List Bool))
```

Notice recursion uses the special `*` character, repeated n times, which works like bruijn indexed variables, refering to the nth enclosing complete ADT. Polymorphic types are just functions returning ADTs. Also, the syntax does not (as a design principle) create any top level definition. We can get many free functions for those datatypes using **high-order derivers** such as `Ctor`, `Show`, `Match`, `Getter`, `Fold`.

```haskell
Bool  = #{True | False}
True  = (Ctor 0 Bool)
False = (Ctor 1 Bool) 
show  = (Show Bool)
if    = (Match Bool)
... and so on
```
Note this has nothing to do with types or typechecking. 

Consult [`Prelude/derivers.mel`](https://github.com/maiavictor/caramel/blob/master/Prelude/derivers.mel) for the available derivers.  

# Open Problems

This project is experimental with lots of open problems, most regarding the encoding of ADTs. There are two sane answers, the Scott or the Church encodings, both with pros/cons:

- The **Church encoding** represents structures as their fold. An example would be `(c n -> (c 1 (c 2 (c 3 n))))` for the list `[1,2,3]`. It fuses algorithms naturally and miraculously well, removing all intermediate structures even for things like Zip, which Haskell itself struggled with. Moreover, it allows for a total implementation of algorithms - i.e., you don't need recursion, fixed point combinators, your programs always terminate and strongly normalize. It is very efficient and less complex.
    
- The **Scott encoding** represents structures as their pattern match. An example would be `(c n -> (c 1 (c n -> (c 2 (c n -> (c 3 n))))))` for the list `[1,2,3]`. It is much more efficient for pattern matching and, thus, walking through a data structure step by step (think zippers, lens). The issue is that, to operate over the whole structure, you need recursion so algorithms won't fuse, among other undesirable properties.  

It is hard to decide if the Scott encoding is necessary: for example, having a slow "tail" operation looks like a huge issue, until you realise you almost never need "tail" on the church encoding anyway. But what about zippers and lenses? Determining what encoding to pick has been hard.

Moreover, the encoding for first-class ADTs I'm using looks arbitrary and programming the derivers (for constructors, matching, etc) is almost too tricky for me to keep track. This could certainly be improved. Someone with better type algebra knowledge could certainly aid me on that one.

# TODOs

There are more things than I can think of. Here are some of them:

- Implement pattern matching.

- Implement operators.

- Implement fast evaluators (the current one is as slow as it gets).

- Compile to [`Tromp's Binary Lambda Calculus`](https://tromp.github.io/cl/cl.html), as suggested by `murbard2` on [Hacker News](https://news.ycombinator.com/item?id=10288249).

- Web interface (compiled via GHCJS?) as suggested by `tikhonj` also on [Hacker News](https://news.ycombinator.com/item?id=10288249).

- Allow omiting application parens in some places such as inside lists (i.e., `[f 1, f 2]` istead of `[(f 1), (f 2)]`, let syntax, etc).

- Shorter syntax for ADTs when you don't want to specify field names (i.e., `List a = #{Cons a * | Nil}`).

- Translation to other languages, example: `mel main.js`, `mel main.rb`, etc.

- Create Church-Encoded libraries on the other languages so they can interact with lambda-calculus compiled terms. For example, creating a `church_list_to_array` function on JavaScript, and similar things - this would be even better paired with derivers, write a generic `ADT_to_JSON` and `JSON_to_ADT`.

- Proper parse error messages.

- Types?

- Syntax for the Scott Encoding, perhaps?

- Rethink the encoding of Char (wouldn't tuples of booleans be better?).

- Rethink the encoding of ADTs.

- (On Prelude) - implement derivers for Church Encoded types.

- (On Prelude) - reimplement derivers for the Scott Encoded types, and implement missing derivers (equality, monad, etc.).

- (On Prelude) - implement a ton of data structures. Maybe, Either, Tree, Map and so on. Lucky, most of that can be almost line-by-line borrowed from Haskell existings resources.

- Many more things I probably forgot about right now.
