#include "list_comprehension.hpp"
#include <iostream>
#include <map>
#include <array>
#include <vector>
#include <ranges>

namespace std {
    template<class A> vector(A&&)->vector<typename A::value_type>;
    template<class A> vector(A&)->vector<typename A::value_type>;
}

int main()
{
    using namespace kaixo;
    using namespace kaixo::lc_operators;
    using namespace kaixo::lc_functions;

    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;
    constexpr auto x = var<"x">;
    constexpr auto key = var<"key">;
    constexpr auto value = var<"value">;

    // Normal list comprehension stuff with multiple ranges, some constraints, and the output. 
    // In this case the output is a tuple of 3 ints. This will create a std::vector<std::tuple<int, int, int>> 
    // with all the tuples from the cartesian product that satisfy the constraint. And, as you can see, 
    // you can use the variables in the ranges!
    std::vector r1 = lc[(a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c];

    // Parallel iteration! This is where the magic really comes in. It will iterate in parallel, 
    // so the output here will be (0, 0), (1, 1), ...,(4, 4).
    std::vector r2 = lc[a + b | (a, b) <- (range(0, 5), range(0, 5))];
    
    // Decompose the keys and values from an std::map! You can also combine this with parallel 
    // iteration to extract all separate variables.
    std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
    std::vector r3 = lc[value + a | ((key, value), a) <- (data, range(0, 5))];

    // Calling standard functions in an expression. Most of the functions in the standard have 
    // been given an overload for expr<Type>, so you can use them in the constraints or in the 
    // result expression. This example takes the pairs from the vector, and results in a vector 
    // with the biggest of the 2 values in the tuple.
    std::vector r4 = lc[max(a, b) | (a, b) <- std::array<std::pair<int, int>, 3>{ { { 1, 5 }, { 5, 4 }, { 3, 4 } } }];

    // Lazy evaluation! You can create an infinite list
    constexpr auto r5 = lc[a | a <- range(0, inf)];

    // Breaking conditions. You can add a breaking condition to a list comprehension, 
    // and it will stop generating results as soon as the condition evaluates to true. So 
    // this means the example down below will only generate values until `x` reaches 100.
    std::vector r6 = lc[x | x <- range(0, inf), brk <<= x == 100];

    // Variable assignments.You can have intermediate expressions to use in the rest of the list comprehension!
    std::vector r7 = lc[a | b <- range(0, 10), a <<= b * 2];
}
