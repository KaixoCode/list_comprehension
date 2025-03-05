#include <algorithm>
#include <map>
#include <vector>
#include <print>
#include <string>

#include "kaixo/list_comprehension.hpp"

int main() {
    using namespace kaixo;
    using namespace kaixo::variables;

    // Normal list comprehension stuff with multiple ranges, some constraints, and the output. 
    // In this case the output is a tuple of 3 ints. This will create a std::vector<std::tuple<int, int, int>> 
    // with all the tuples from the cartesian product that satisfy the constraint. And, as you can see, 
    // you can use the variables in the ranges!
    auto r1 = ((a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c);
    for (auto [a, b, c] : r1) std::print("({}, {}, {}), ", a, b, c); std::println("");
    // result: (3, 4, 5), (6, 8, 10),

    // Parallel iteration! This is where the magic really comes in. It will iterate in parallel, 
    // so the output here will be (0, 0), (1, 1), ...,(4, 4).
    auto r2 = ((a, b) | (a, b) <- (range(0, 5), range(0, 5)));
    for (auto [a, b] : r2) std::print("({}, {}), ", a, b); std::println("");
    // result: (0, 0), (1, 1), (2, 2), (3, 3), (4, 4),

    // Decompose the keys and values from an std::map! You can also combine this with parallel 
    // iteration to extract all separate variables.
    std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
    auto r3 = (value + a | ((key, value), a) <- (data, range(0, 5)));
    for (auto v : r3) std::print("{}, ", v); std::println("");
    // result: 2, 5, 8,

    // Calling standard functions in an expression. Most of the functions in the standard have 
    // been given an overload for unevaluated arguments, so you can use them in the constraints 
    // or in the result expression. This example takes the pairs from the array, and takes the 
    // biggest of the 2 values in the pair.
    auto r4 = (std::max(a, b) | (a, b) <- std::array<std::pair<int, int>, 3>{ { { 1, 5 }, { 5, 4 }, { 3, 4 } } });
    for (auto v : r4) std::print("{}, ", v); std::println("");
    // result: 5, 5, 4,

    // Lazy evaluation! You can create an infinite list
    constexpr auto r5 = (a | a <- range(0, inf));

    // Breaking conditions. You can add a breaking condition to a list comprehension, 
    // and it will stop generating results as soon as the condition evaluates to true. So 
    // this means the example down below will only generate values until `x` reaches 10.
    auto r6 = (x | x <- range(0, inf), brk = x == 10);
    for (auto v : r6) std::print("{}, ", v); std::println("");
    // result: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,

    // You can create a range of ranges.
    auto r7 = (range(0, a) | a <- range(1, 5));
    for (auto r : r7) { for (auto v : r) std::print("{}, ", v); std::println(""); }
    // result: 0,
    //         0, 1,
    //         0, 1, 2,
    //         0, 1, 2, 3,
    
    // You can iterate over nested ranges as well.
    std::vector<std::string> names{ "John", "Jimmy", "James" };
    auto r8 = (b | a <- names, b <- a, std::islower(b));
    for (auto r : r8) std::print("{}", r); std::println("");
    // result: "ohnimmyames"

    // There is a special operation that lets you insert into ranges while iterating
    // as well. One great use for that is to generate prime numbers. It uses the
    // vector as a cache for all the primes, which it'll then use to check for divisors.
    // This also uses the 'is_empty' function, which just checks whether there's any 
    // values in the resulting range.
    std::vector<int> primes{};
    auto r9 = (a | a <- range(2, 20), is_empty((1 | b <- primes, b <= std::sqrt(a), a % b == 0)), primes << a);
    for (auto r : r9) std::print("{}, ", r); std::println("");
    // result: 2, 3, 5, 7, 11, 13, 17, 19,

    // You can also create more complex list comprehensions with objects.
    struct Person {
        int id;
        std::string name;
        int age;
        std::vector<int> friends;
    };

    constexpr kaixo::var<struct variable_person> person;
    constexpr kaixo::var<struct variable_friend_ids> friend_ids;
    constexpr kaixo::var<struct variable_friends> friends;

    std::array people{
        Person{.id = 0, .name = "John",  .age = 36, .friends = { 1, 2 } },
        Person{.id = 1, .name = "Harry", .age = 22, .friends = { 0, 2 } },
        Person{.id = 2, .name = "Larry", .age = 55, .friends = { 1, 3 } },
        Person{.id = 3, .name = "Sam",   .age = 15, .friends = { 1 } },
    };
    
    // Here we query all people, and then also do a sub-query for all their
    // Note how we need to use the unary '+' operator before the sub-query
    // this is sometimes necessary when having a range in an expression.
    // Without the unary '+' operator this would result in a zipped range
    // instead of a tuple operation.
    auto query = (
        ( person
        , +(friends[&Person::name] | friends    <- people,
                                     friend_ids <- person[&Person::friends],
                                     friends[&Person::id] == friend_ids) // Join on friends.id
            ) | person <- people);

    for (auto [person, friends] : query) {
        std::print("{} is {} years old and is friends with ", person.name, person.age);
        for (auto& frend : std::views::join_with(friends, " and ")) {
            std::print("{}", frend);
        }
        std::println("");
    }
    // result: John is 36 years old and is friends with Harry and Larry
    //         Harry is 22 years old and is friends with John and Larry
    //         Larry is 55 years old and is friends with Harry and Sam
    //         Sam is 15 years old and is friends with Harry

    // Call member functions on objects
    struct Counter {
        int count = 0;

        int increment() { return ++count; }
        int incrementWith(int amount) { return count += amount; }
    };

    std::vector<Counter> counters{ { 1 }, { 5 } };

    auto r10 = (a(&Counter::incrementWith, b) | a <- counters, b <- range(1, 4));
    for (auto v : r10) std::print("{}, ", v); std::println("");

    return 0;
}
