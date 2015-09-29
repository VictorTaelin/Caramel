// JS<->Lambda datatype conversors.
var lambda = require("../lambda.js");

// The function was obtained from the Lambda Calculus using `mel fib.js`.
// `lambda.natFn` makes it operate on JS's Number (such as `3`), not lambda
// numbers (such as `(function(f){return function(x){return f(f(f(x)))}})`).
var fib = lambda.natFn(function(a){return a((function(b){return b((function(c){return (function(d){return (function(e){return e(d)((function(f){return (function(g){return c(f)(d(f)(g))})}))})})}))}))((function(b){return b((function(c){return (function(d){return d})}))((function(c){return (function(d){return c(d)})}))}))((function(b){return (function(c){return b})}))})

// fib(10) == 55
console.log(fib(10))
