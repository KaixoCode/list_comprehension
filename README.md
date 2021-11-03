# list comprehension
 Single header list comprehension in C++! 
```cpp
var<int> a, b;
auto res = lc[(a, b) | a <- range(0, 4), b <- range(0, 4), a > b];
```
output:
```
(1, 0), (2, 0), (2, 1), (3, 0), (3, 1), (3, 2)
```

## Features
So, what can this do?

Normal list comprehension stuff with multiple ranges, some constraints, and the output. In this case the output is a tuple of 3 ints. This will create a `std::vector<std::tuple<int, int, int>>` with all the tuples from the cartesian product that satisfy the constraint. And, as you can see, you can use the variables in the ranges!
```cpp
var<int> a, b, c;
auto r1 = lc[(a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c];
``` 

It automatically uses `std::string` when the output is characters! The example below will result in `"abc"`.
```cpp
std::vector<char> chars{ 'a', 'b', 'c' };
var<char> d;
std::string r2 = lc[d | d <- chars];
```

Parallel iteration! This is where the magic really comes in. It will iterate in parallel, so the output here will be `(0, 0), (1, 1), ...,(4, 4)`.
```cpp
var<int> a, b;
auto r3 = lc[a + b | (a, b) <- (range(0, 5), range(0, 5))];
```

Decompose the keys and values from an `std::map`! You can also combine this with parallel iteration to extract all separate variables.
```cpp
std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
var<int> key, value, a;
auto r4 = lc[value + a | ((key, value), a) <- (data, range(0, 5))];
```

Nested list comprehensions! Yes, you heard that right! Nested list comprehension also works, you just have to use `lcv` instead of `lc`. This example will filter through all the vectors in the vector, and only keep the even numbers.
```cpp
var<int> x;
var<std::vector<int>> xs;
std::vector<std::vector<int>> xxs{ { 1,3,5,2,3,1,2,4,5 }, { 1,2,3,4,5,6,7,8,9 }, { 1,2,4,2,1,6,3,1,3,2,3,6 } };
auto r8 = lc[lcv[x | x <- xs, x % 2 == 0] | xs <- xxs];
```

Using variables as a container! Yes, you can also use a variable as a container. The example down here will concatenate all the vectors in the vector `xxs` into a single vector.
```cpp
var<int> x;
var<std::vector<int>> xs;
std::vector<std::vector<int>> xxs{ { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
auto r9 = lc[x | xs <- xxs, x <- xs];
```

Calling standard functions in an expression. Most of the functions in the standard have been given an overload for `expr<Type>`, so you can use them in the constraints or in the result expression. This example takes the pairs from the vector, and results in a vector with the biggest of the 2 values in the tuple. 
```cpp
std::vector<std::pair<int, int>> data{ { 1, 5 }, { 5, 4 }, { 3, 4 } };
var<int> a, b;
auto r10 = lc[max(a, b) | (a, b) <- data];
```

Some of the classes in the standard also have their own specialization of `expr<Type>` which means their member functions also work in expressions! The example down below keeps all strings that have more than 4 characters, and then filters the individual strings to only keep alphabetical character
```cpp
std::vector<std::string> data{ "1jf1d", "afj3", "a09af", "a31" };
var<std::string> str;
var<char> c;
auto r11 = lc[lcv[c | c <- str, isalpha(c)] | str <- data, str.size() > 4];
```

But what if you want to store the result in a `std::list`? Easy, just tell the thing! This works for all the standard containers!
```cpp
var<int> a;
auto r12 = lc[list(a) | a <- range(0, 10)];
```

Lazy evaluation! You can create an infinite list and only generate the first `n` values.
```cpp
var<int> a;
auto lazylist = lcl[a | a <- range(0, inf)];
auto r13 = lazylist.take(30);
```

## How it works
Operator overloads! Obviously, but how exactly? The first part is the `container_syntax`, which is the part before the `|`, and it determines what the resulting container is. In the `container_syntax` a tuple of expressions is stored, which can be evaluated when generating values for the result. 

The next part are the containers, which is the `<-` operator. The first unary `-` operator constructs a general `container` object, which stores the specified container by value or by reference. Then the `<` operator links that `container` to a variable in a `linked_container`. When doing parallel iteration, the comma operator first constructs a `tuple_of_vars` or a `tuple_of_containers`, which both behave exactly like a single variable or container.

The first `linked_container` and the `container_syntax` get constructed into a `list_comprehension` object by the `|` operator. When more `linked_container`s are used, the comma operator will combine the `list_comprehension` and the `linked_container` into a new `list_comprehension` object.

Then come the constraints, which are simply `expr<bool>`, so can be stored in a vector inside the `list_comprehension`. These are added by another version of the overloaded comma operator.

After all this we have the final `list_comprehension` object, which then gets passed to the `lc` global's index operator, which simply calls the `get` method on the `list_comprehension` to generate the result. The `lcv` creates an expression that when evaluated will call the `get` method and return the result. And the `lcl` for lazy evalutation simply returns the `list_comprehension` object, which contains the `take` method.
