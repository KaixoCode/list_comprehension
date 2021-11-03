# list comprehension
 List comprehension in C++. Works by creating expr<Result> objects, which are basically lambdas that return the result of some expression, so the result can be evaluated later for some values of var<Type> used in the expression. a var holds either a value or a reference.
```cpp
using namespace kaixo;
int main() {
    
    // Simple list comprehension with ranges and a constraint.
    var<int> a, b, c;
    auto r1 = lc[(a, b, c) | c <- range(1, 11), b <- range(1, c+1), a <- range(1, b+1), a*a + b*b == c*c];

    // Parallel iteration
    var<std::tuple<int, int>> d;
    auto r2 = lc[d | d <- (range(0, 10), range(0, 10))];
    
    // Iteration on some container
    std::vector<int> data{ 1, 2, 3, 4, 5 };
    auto r3 = lc[a + b | a <- data, b <- range(0, 10)];

    // Determine resulting container
    std::vector<std::string> strings{ "hello", "carrot", "pizza" };
    var<std::string> e;
    auto r4 = lc[map(e, a) | (e, a) <- (strings, range(0, 100))]; 
    
    // Call std functions
    std::vector<int> ints1{ 5, 2, 7, 3, 1, 9 };
    std::vector<int> ints2{ 4, 1, 8, 9, 3, 2 };
    auto r5 = lc[max(a, b) | (a, b) <- (ints1, ints2)];
    
    // Automatically uses std::string when working with characters (to<char>() because tolower() returns int)
    std::string mystr = "HelloWorld";
    var<char> g;
    std::string r6 = lc[tolower(g).to<char>() | g <- mystr];
    
    // Nested list comprehension for lists of lists
    var<int> x;
    var<std::vector<int>> xs;
    std::vector<std::vector<int>> xxs{ { 1,3,5,2,3,1,2,4,5 }, { 1,2,3,4,5,6,7,8,9 }, { 1,2,4,2,1,6,3,1,3,2,3,6 } };
    auto r8 = lc[lcv[x | x <- xs, x % 2 == 0] | xs <- xxs];
    
    // Using a used variable as a container
    auto r9 = lc[x | xs <- xxs, x <- xs];
    
    // Make a utility function
    auto indices = [x = var<int>{}, i = var<int>{}](auto& data, var<int> a) mutable {
        // Parallel iteration of value and index, constraint on value == argument, store index.
        return lc[i | (x, i) <- (data, range(0, data.size())), a == x];
    };

    std::vector<int> datas{ 0, 1, 1, 0, 0, 1, 0 };
    auto r10 = indices(datas, 1);
    
    // Parallel iteration with names for each instead of tuple
    auto r11 = lc[(a + b + c) | (a, b, c) <- (range(0, 10), range(0, 10), range(0, 10))];
    
    // Split a tuple from a container
    std::vector<std::tuple<int, int, int>> q{ { 0, 1, 1 }, { 2, 3, 3 } };
    auto r12 = lc[a + b + c | (a, b, c) <- q];
    
    // Get the key and value from a map
    std::map<std::string, int> q{ { "apple", 1 }, { "carrot", 3 } };
    var<std::string> key;
    var<int> value;
    auto r13 = lc[value | (key, value) <- q];
}
```
