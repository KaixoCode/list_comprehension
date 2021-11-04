#include "list_comprehension.hpp"
#include <iostream>
using namespace kaixo;
using namespace kaixo::lc_operators;


void test() {
    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();


    auto triangle = [](auto v) { 
        var r = range(0ll, v + 1);
        return kaixo::accumulate(r.begin(), r.end(), 0ll); 
    };

    auto factors = [x = var<int64_t>{}](auto v) {
        return lcv[x | x <- range(2ll, sqrt(v).to<int64_t>() + 1), v % x == 0];
    };

    var<int64_t> x, a;
    var<size_t> b;
    auto r = lcl[(x, a, b) | x <- range(1ll, inf), a <<= a + x, b <<= factors(a).size() + 2, b > 500];
    auto res = r.take(1);



    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << "[�s]" << std::endl;
}


int main()
{
    test();
    // Normal list comprehension stuff with multiple ranges, some constraints, and the output. 
    // In this case the output is a tuple of 3 ints. This will create a std::vector<std::tuple<int, int, int>> 
    // with all the tuples from the cartesian product that satisfy the constraint. And, as you can see, 
    // you can use the variables in the ranges!
    var<int> a, b, c;
    auto r1 = lc[(a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c];

    // It automatically uses std::string when the output is characters! The example below will result in "abc".
    std::vector<char> chars{ 'a', 'b', 'c' };
    var<char> d;
    std::string r2 = lc[d | d <- chars];

    // Parallel iteration! This is where the magic really comes in. It will iterate in parallel, 
    // so the output here will be (0, 0), (1, 1), ...,(4, 4).
    auto r3 = lc[a + b | (a, b) <- (range(0, 5), range(0, 5))];
    
    // Decompose the keys and values from an std::map! You can also combine this with parallel 
    // iteration to extract all separate variables.
    std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
    var<int> key, value;
    auto r4 = lc[value + a | ((key, value), a) <- (data, range(0, 5))];

    // Nested list comprehensions! Yes, you heard that right! Nested list comprehension also works, 
    // you just have to use lcv instead of lc. This example will filter through all the vectors in
    // the vector, and only keep the even numbers.
    var<int> x;
    var<std::vector<int>> xs;
    std::vector<std::vector<int>> xxs{ { 1,3,5,2,3,1,2,4,5 }, { 1,2,3,4,5,6,7,8,9 }, { 1,2,4,2,1,6,3,1,3,2,3,6 } };
    auto r8 = lc[lcv[x | x <- xs, x % 2 == 0] | xs <- xxs];

    // Using variables as a container! Yes, you can also use a variable as a container. 
    // The example down here will concatenate all the vectors in the vector xxs into a single vector.
    auto r9 = lc[x | xs <- xxs, x <- xs];

    // Calling standard functions in an expression. Most of the functions in the standard have 
    // been given an overload for expr<Type>, so you can use them in the constraints or in the 
    // result expression. This example takes the pairs from the vector, and results in a vector 
    // with the biggest of the 2 values in the tuple.
    std::vector<std::pair<int, int>> data2{ { 1, 5 }, { 5, 4 }, { 3, 4 } };
    auto r10 = lc[max(a, b) | (a, b) <- data2];

    // Some of the classes in the standard also have their own specialization of expr<Type> which 
    // means their member functions also work in expressions! The example down below keeps all strings 
    // that have more than 4 characters, and then filters the individual strings to only keep 
    // alphabetical character
    std::vector<std::string> data3{ "1jf1d", "afj3", "a09af", "a31" };
    var<std::string> str;
    var<char> ch;
    auto r11 = lc[lcv[ch | ch <- str, isalpha(ch)] | str <- data3, str.size() > 4];

    // But what if you want to store the result in a std::list? Easy, just tell the thing! 
    // This works for all the standard containers!
    auto r12 = lc[list(a) | a <- range(0, 10)];

    // Lazy evaluation! You can create an infinite list and only generate the first n values.
    auto lazylist = lcl[a | a <- range(0, inf)];
    auto r13 = lazylist.take(30);

    // Breaking conditions. You can add a breaking condition to a list comprehension, 
    // and it will stop generating results as soon as the condition evaluates to true. So 
    // this means the example down below will only generate values until `x` reaches 100.
    auto r14 = lc[x | x <- range(0, inf), brk <<= x == 100];

    // Name alias for the current result! This means you can use the current result of a list 
    // comprehension in a nested list comprehension! This allows for a really fast prime generator.
    // Combining this with the breaking conditions we have a prime generator that only checks prime 
    // factors up to the sqrt of the number, and using the `max_size` of the lazy evaluation list,
    // it will stop generating factors once it exceeds the given limit of 0.
    xs.run_expression().reserve(100000); // Reserve to prevent reallocation.
    auto primegenerator = lcl[xs = x | x <- range(2, inf), lcl[a | a <- xs, x % a == 0, brk <<= a > sqrt(x)].max_size(0)];
    auto r15 = primegenerator.take(100000).back(); // Get the millionth prime!
}
