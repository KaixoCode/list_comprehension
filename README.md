# list comprehension
 Single header list comprehension in C++! 
```cpp
constexpr auto a = var<"a">;
constexpr auto b = var<"b">;
constexpr auto res = lc[(a, b) | a <- range(0, 4), b <- range(0, 4), a > b];
```
output:
```
(1, 0), (2, 0), (2, 1), (3, 0), (3, 1), (3, 2)
```

## Features
So, what can this do?

Normal list comprehension stuff with multiple ranges, some constraints, and the output. In this case the output is a tuple of 3 ints. This will create a `std::vector<std::tuple<int, int, int>>` with all the tuples from the cartesian product that satisfy the constraint. And, as you can see, you can use the variables in the ranges!
```cpp
constexpr auto r1 = ((a, b, c) | c <- range(1, 11), 
    b <- range(1, c), a <- range(1, b), a*a + b*b == c*c);
``` 

Parallel iteration! This is where the magic really comes in. It will iterate in parallel, so the output here will be `(0, 0), (1, 1), ...,(4, 4)`.
```cpp
constexpr auto r2 = (a + b | (a, b) <- (range(0, 5), range(0, 5)));
```

Decompose the keys and values from an `std::map`! You can also combine this with parallel iteration to extract all separate variables.
```cpp
std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
auto r3 = (value + a | ((key, value), a) <- (data, range(0, 5)));
```

Calling standard functions in an expression. Most of the functions in the standard have been given an overload for `expr<Type>`, so you can use them in the constraints or in the result expression. This example takes the pairs from the vector, and results in a vector with the biggest of the 2 values in the tuple. 
```cpp
constexpr auto r4 = (max(a, b) | (a, b) <- 
    std::array<std::pair<int, int>, 3>{ { { 1, 5 }, { 5, 4 }, { 3, 4 } } });
```

Lazy evaluation! You can create an infinite list
```cpp
constexpr auto r5 = (a | a <- range(0, inf));
```

Breaking conditions. You can add a breaking condition to a list comprehension, and it will stop generating results as soon as the condition evaluates to true. So this means the example down below will only generate values until `x` reaches 100.
```cpp
constexpr auto r6 = (x | x <- range(0, inf), brk <<= x == 100);
```

Variable assignments. You can have intermediate expressions to use in the rest of the list comprehension!
```cpp
constexpr auto r7 = (a | b <- range(0, 10), a <<= b * 2);
```
