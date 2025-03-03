
#include <algorithm>
#include <map>
#include <vector>
#include <print>
#include <string>
#include <source_location>
#include <stacktrace>

#include "kaixo/list_comprehension.hpp"

namespace kaixo {
    template<evaluated_range Range, class ...Vars>
    constexpr named_range<var<Vars...>, std::views::all_t<Range&&>> operator>>(Range&& range, var<Vars...>) {
        return { {
            .range = std::views::all(std::forward<Range>(range)),
        } };
    }    
    
    template<unevaluated Range, class ...Vars>
    constexpr named_range<var<Vars...>, std::decay_t<Range>> operator>>(Range&& range, var<Vars...>) {
        return { {
            .range = std::forward<Range>(range),
        } };
    }
}

#define SELECT (
#define FROM ) |
#define WHERE ,
#define AND and
#define AS >>

struct CopyChecker {
    mutable std::stacktrace trace = std::stacktrace::current();
    mutable std::size_t line{};
    mutable std::size_t column{};
    mutable int copies = 0;
    mutable int moves = 0;

    CopyChecker() = default;

    CopyChecker(const CopyChecker& c)
        : copies(c.copies + 1)
    {
        c.copies++;
        c.trace = trace;
    }

    CopyChecker(CopyChecker&& c)
        : moves(c.moves + 1) 
    {
        c.moves++;
    }

    CopyChecker& operator=(const CopyChecker& c) {
        trace = std::stacktrace::current();
        c.trace = trace;
        c.copies++;
        copies++;
        return *this; 
    }

    CopyChecker& operator=(CopyChecker&& c) {
        c.copies++;
        moves++; 
        return *this; 
    }
};

constexpr kaixo::var<struct name> name;
constexpr kaixo::var<struct age> age;
constexpr kaixo::var<struct person> person;
constexpr kaixo::var<struct friend_ids> friend_ids;
constexpr kaixo::var<struct friends> friends;

struct Person {
    int id;
    std::string name;
    int age;
    std::vector<int> friends;
};

int main() {
    using namespace kaixo;
    using namespace kaixo::variables;

    std::array people{
        Person{ .id = 0, .name = "John",  .age = 36, .friends = { 1, 2 } },
        Person{ .id = 1, .name = "Harry", .age = 22, .friends = { 0, 2 } },
        Person{ .id = 2, .name = "Larry", .age = 55, .friends = { 2, 3 } },
        Person{ .id = 3, .name = "Sam",   .age = 15, .friends = { 1 } },
    };
        
    auto query = (
        SELECT 
            person[&Person::name], 
            person[&Person::age], 
            (
                SELECT friends[&Person::name]
                FROM people                   AS friends,
                     person[&Person::friends] AS friend_ids
                WHERE friends[&Person::id] == friend_ids
                  AND friends[&Person::id] != person[&Person::id]
            )
        FROM people AS person
    );

    for (auto [name, age, friends] : query) {
        std::print("{} is {} years old and is friends with ", name, age);
        for (auto& frend : std::views::join_with(friends, " and ")) {
            std::print("{}", frend);
        }
        std::println("");
    }
    
    auto query2 = (
        SELECT person[&Person::name]
        FROM people                   AS person,
             person[&Person::friends] AS friend_ids
        WHERE person[&Person::id] == friend_ids
    );

    for (auto& name : query2) {
        std::println("{} is friends with himself", name);
    }
    
    auto query3 = (
        SELECT person[&Person::friends][0]
        FROM people AS person
    );

    for (auto& firstFriend : query3) {
        std::println("{}", firstFriend);
    }
    
    return 0;



    {
        std::vector<std::string> names{ "John", "Harry", "James" };

        auto cs = (b | a <- names, b <- a, std::islower(b));

        for (auto c : cs) {
            std::print("{}", c);
        }

        return 0;

        std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
        auto r3 = (value + a | ((key, value), a) <- (data, range(0, 5)));
        for (auto v : r3) std::println("{}", v);

        auto query = (
            SELECT a, b, c 
            FROM range(1, 11) AS c, 
                 range(1,  c) AS b, 
                 range(1,  b) AS a
            WHERE a * a + b * b == c * c
        );
    
        for (auto [a, b, c] : query) {
            std::println("({}, {}, {})", a, b, c);
        }
    
        auto query2 = (
            SELECT 
                a + 0,
                (   SELECT d 
                    FROM range(0, a) AS d
                    WHERE d % 2 == 0
                      AND d != 0
                ), 
                b + 1,
                c
            FROM range(1, 11) AS c, 
                 range(1,  c) AS b, 
                 range(1,  b) AS a
            WHERE a * a + b * b == c * c
        );

        for (auto [a, range, b, c] : query2) {
            std::println("({}, {}, {})", a, b, c);
            for (auto d : range) {
                std::println("{}", d);
            }
        }

        //constexpr bool aeio = std::random_access_iterator<range<int, int>::iterator>;
        //constexpr bool aeion = std::ranges::random_access_range<range<int, int>>;
        //
        //constexpr auto aoein = (std::max(std::max(a, b), c) | a <- range(0, 10), b <- range(0, 10), c <- range(0, 10));
        //constexpr auto sroig = aoein[639];
        //
        //std::vector<int> primes{};
        //
        //auto generatePrimes = (a <- range(2, inf), is_empty((b <- primes, b <= std::sqrt(a), a % b == 0)), primes << a);
        //
        //for (auto prime : generatePrimes) {
        //    std::println("{}", prime);
        //}
    }
}