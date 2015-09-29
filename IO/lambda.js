// lambda.js
//
// This lib converts JavaScript native datatypes to/from Lambda-encoded datatypes,
// so you can use Lambda-Calculus functions inside a JavaScript environment.
//
// Convention:
//
// Normally named functions receive JS datatypes and return Lambda datatypes.
// Functions ending in "_" receive Lambda datatypes and return JS datatypes.
//
// Example: 
//
//     // Converts JS's number 3 to Church number.
//     console.log(nat(3));                 // output: [Function]
//
//     // Converts the Church number three to JS's number 3.
//     // Notice that `nat(nat_(x)) == x`.
//     console.log(nat_(nat(3)));           // output: 3
//
//     // Calculates 3^3 using Church numbers (exponentiation = application).
//     console.log(nat_((nat(3))(nat(3)))); // output: 27
//     

var lambda = (function(){
    // Converts a JavaScript native Number to a Church-encoded number.
    // nat :: JSNumber -> ChurchNat
    var nat = function(n){
        return(function(f){
            return(function(a){
                for(var i=0;i<n;++i)
                    a = f(a);
                return a;
            })
        })
    };

    // Converts a Church-encoded number to a JavaScript native Number.
    // nat_ :: ChurchNat -> JSNumber
    var nat_ = function(n){
        return((n(function(a){
            return(a+1)
        }))(0))
    };

    // Converts a function on Church-encoded numbers to a function that
    // works on native JavaScript numbers.
    //  natFn :: (Nat -> Nat) -> (JSNumber -> JSNumber)
    var natFn = function(f){
        return function(x){
            return nat_(f(nat(x)));
        };
    };

    return {
        nat   : nat,
        nat_  : nat_,
        natFn : natFn};
})();
if (typeof module.exports !== "undefined") module.exports = lambda;
