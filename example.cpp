#include "list_comprehension.hpp"
#include <iostream>
using namespace kaixo;
using namespace kaixo::lc_operators;


void test() {
    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

    int64_t n = 20;
    int64_t l = 4;
    var data = std::vector<int64_t>{
         8,02,22,97,38,15,00,40,00,75,04,05,07,78,52,12,50,77,91, 8,
        49,49,99,40,17,81,18,57,60,87,17,40,98,43,69,48,04,56,62,00,
        81,49,31,73,55,79,14,29,93,71,40,67,53,88,30,03,49,13,36,65,
        52,70,95,23,04,60,11,42,69,24,68,56,01,32,56,71,37,02,36,91,
        22,31,16,71,51,67,63,89,41,92,36,54,22,40,40,28,66,33,13,80,
        24,47,32,60,99,03,45,02,44,75,33,53,78,36,84,20,35,17,12,50,
        32,98,81,28,64,23,67,10,26,38,40,67,59,54,70,66,18,38,64,70,
        67,26,20,68,02,62,12,20,95,63,94,39,63, 8,40,91,66,49,94,21,
        24,55,58,05,66,73,99,26,97,17,78,78,96,83,14,88,34,89,63,72,
        21,36,23, 9,75,00,76,44,20,45,35,14,00,61,33,97,34,31,33,95,
        78,17,53,28,22,75,31,67,15,94,03,80,04,62,16,14, 9,53,56,92,
        16,39,05,42,96,35,31,47,55,58,88,24,00,17,54,24,36,29,85,57,
        86,56,00,48,35,71,89,07,05,44,44,37,44,60,21,58,51,54,17,58,
        19,80,81,68,05,94,47,69,28,73,92,13,86,52,17,77,04,89,55,40,
        04,52, 8,83,97,35,99,16,07,97,57,32,16,26,26,79,33,27,98,66,
        88,36,68,87,57,62,20,72,03,46,33,67,46,55,12,32,63,93,53,69,
        04,42,16,73,38,25,39,11,24,94,72,18, 8,46,29,32,40,62,76,36,
        20,69,36,41,72,30,23,88,34,62,99,69,82,67,59,85,74,04,36,16,
        20,73,35,29,78,31,90,01,74,31,49,71,48,86,81,16,23,57,05,54,
        01,70,54,71,83,51,54,69,16,92,33,48,61,43,52,01,89,19,67,48,
    };
    var<size_t> i;
    var<int64_t> x, a;
    var<std::vector<int64_t>> xs;

    // Define product function
    auto product = [](auto& xs) { return kaixo::accumulate(xs.begin(), xs.end(), 1ll, std::multiplies<int64_t>()); };

    // Get all products of each direction
    auto hori = lc[product(xs) | xs <- lc[lcv[data[i + a] | a <- range(0ll, l)] | i <- range(0ull, data.size()), i % n < n - l]];
    auto vert = lc[product(xs) | xs <- lc[lcv[data[i + a * n] | a <- range(0ll, l)] | i <- range(0ull, data.size()), i / n < n - l]];
    auto dia1 = lc[product(xs) | xs <- lc[lcv[data[i + a * n + a] | a <- range(0ll, l)] | i <- range(0ull, data.size()), i / n < n - l, i % n < n - l]];
    auto dia2 = lc[product(xs) | xs <- lc[lcv[data[i + a * n - a] | a <- range(0ll, l)] | i <- range(0ull, data.size()), i / n < n - l, i % n >= l]];

    // Combine products into single vector
    auto vovt = std::vector<std::vector<int64_t>>{ hori, vert, dia1, dia2 };
    auto alld = lc[x | xs <- vovt, x <- xs];

    // Get biggest element
    auto grst = std::max_element(alld.begin(), alld.end());

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << "[µs]" << std::endl;
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
