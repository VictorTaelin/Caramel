# Caramel's transmogrifer IO utils.

Lambda Calculus is a language of pure computation and doesn't have any notion of IO. That way, if you need to interface with the real world, you need to inject a Lambda Calculus program into a "real world" programming language, such as JavaScript. A problem with that is popular languages use native datatypes (such as Int), which won't be compatible with Lambda Calculus datatypes (such as Church-encoded numbers). Caramel's IO libraries convert native datatypes to lambda-based datatypes, enabling you to use Caramel programs in any environment.

Example: using Fib on JavaScript.

    var lambda = require("../lambda.js");

    // The function was obtained from the Lambda Calculus using `mel fib.js`.
    // `lambda.natFn` makes it operate on JS's Number (such as `3`), not lambda
    // numbers (such as `(function(f){return function(x){return f(f(f(x)))}})`).
    var fib = lambda.natFn(function(a){return a((function(b){return b((function(c){return (function(d){return (function(e){return e(d)((function(f){return (function(g){return c(f)(d(f)(g))})}))})})}))}))((function(b){return b((function(c){return (function(d){return d})}))((function(c){return (function(d){return c(d)})}))}))((function(b){return (function(c){return b})}))})

    // fib(10) == 55
    console.log(fib(10))

Another problem is that most programs obtained this way will be terribly inefficient due to:

1. Unary numbers being used - you should prefer binary formats (TODO: implement Int/Float algebra on Prelude).

2. Most programming languages not implementing functions optimally. See the [http://github.com/MaiaVictor/optlam](Optlam) repository for more info.

From some tests, I believe using binary numbers and good functional evaluators (such as GHC, some Scheme compilers) can be performant enough to run a few simple lambda calculus applications and games even in slow computers. (TODO: POC)

# TODO

The IO utils are only implemented for JavaScript at this point. Implementing IO utils for other languages should be an easy task.

