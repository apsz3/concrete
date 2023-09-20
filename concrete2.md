Builtin SQL / query engine with ?= assignment operator.
Interpolating values into strings sucks, allow for
inline computation and referencing values that way

Gradual typing

Constant folding


//Raku (formerly Perl6) has gradual typing implemented from the start. Type checks occur at all locations where values are assigned or bound. An "untyped" variable or parameter is typed as Any, which will match (almost) all values. The compiler flags type-checking conflicts at compile time if it can determine at compile time that they will never succeed.



# Conditionals are expressions

var y = if true
    var y = 1
    y
end

# Gradual typing

// to indicate no return value, use void
// to indicate an Any return value, no annotation needed
fun fn (x: int) -> void do

end
var x : int = 0
var y = fn(x)
