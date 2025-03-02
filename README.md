# list comprehension
List comprehension in C++! 

```cpp
constexpr var<struct A> a{};
constexpr var<struct B> b{};
constexpr auto res = ((a, b) | a <- range(0, 4), b <- range(0, 4), a > b);
```
output:
```
(1, 0), (2, 0), (2, 1), (3, 0), (3, 1), (3, 2)
```

## Features
So, what can this do?

Normal list comprehension stuff with multiple ranges, some constraints, and the output. In this case the output is a tuple of 3 ints. 
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

Lazy evaluation! You can create an infinite list
```cpp
constexpr auto r5 = (a | a <- range(0, inf));
```

Breaking conditions. You can add a breaking condition to a list comprehension, and it will stop generating results as soon as the condition evaluates to true. So this means the example down below will only generate values until `x` reaches 100.
```cpp
constexpr auto r6 = (x | x <- range(0, inf), brk = x == 100);
```
