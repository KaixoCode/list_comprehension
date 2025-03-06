
#include <algorithm>
#include <map>
#include <vector>
#include <print>
#include <string>
#include <source_location>
#include <stacktrace>
#include <any>
#include <memory>

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
    
    auto aoine = (a | (a, _) <- (range(0, 0), range(0,0)));

    constexpr auto aoine = []() {
        std::vector<bool> aefae{ true, true };
        
        auto aeiounf = (a & _ & 1);
        
        named_tuple<decltype(a), std::vector<bool>> aea{ aefae };
        return aeiounf.evaluate(aea);

    }();




    named_tuple<decltype(a), std::tuple<std::any>> vssss{ { 1 } };
    named_tuple<decltype(a), std::tuple<int>> fefaefa{ { 1 } };

    auto resfea = std::any_cast<int>(a).evaluate(vssss);

    //auto rsrg = std::make_unique<int>(a + 1).evaluate(fefaefa);

    std::vector<int> av{};
    std::vector<int> bv{};

    //auto oiane = (a, b) <- std::views::cartesian_product(av | std::views::filter([](auto& a) -> decltype(auto) { return a; }), bv);

    //oiane.begin();
    
    //auto aoien = (b, c) <- (a, range(0, 10));
    //
    //decltype(aoien)::depends::size;
    //
    //auto srgsrgs = (b, c) <- (a, range(0, 10));
    //
    //decltype(srgsrgs)::defines::size;

    std::vector<std::string> names{};
    constexpr auto aefa1 = ((a, +range(0, a), +range(0, 1)) | a <- range(0, 10))[0];
    constexpr auto aefa2 = ((+(b | b <- range(0, a)), a) | a <- range(0, 10))[0];
    auto aefa3 = ((a, b, c) | a <- names, (b, c) <- (a, range(0, 10)));

    auto aonei = (b, d) <- ((c, +range(0, a)) | c <- range(0, 10));

    auto gsrgsr = (1 | a <- range(0, 10), (b, d) <- ((c, +range(0, a)) | c <- range(0, 10)));

    for (auto a : gsrgsr) {

    }

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
                +(  SELECT d 
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


using namespace kaixo;
using namespace kaixo::variables;

constexpr std::array<std::string_view, 3> strings{ "abc", "pqr", "xyz" };
constexpr char c1 = *(b | a <- strings, b <- a).begin();
static_assert(c1 == 'a');
constexpr char c2 = *++(b | a <- strings, b <- a).begin();
static_assert(c2 == 'b');
constexpr char c3 = *++++(b | a <- strings, b <- a).begin();
static_assert(c3 == 'c');
constexpr char c4 = *++++++(b | a <- strings, b <- a).begin();
static_assert(c4 == 'p');
constexpr char c5 = *++++++++(b | a <- strings, b <- a).begin();
static_assert(c5 == 'q');
constexpr char c6 = *++++++++++(b | a <- strings, b <- a).begin();
static_assert(c6 == 'r');
constexpr char c7 = *++++++++++++(b | a <- strings, b <- a).begin();
static_assert(c7 == 'x');
constexpr char c8 = *++++++++++++++(b | a <- strings, b <- a).begin();
static_assert(c8 == 'y');
constexpr char c9 = *++++++++++++++++(b | a <- strings, b <- a).begin();
static_assert(c9 == 'z');

struct cmchecker {
    int copies = 0;
    int moves = 0;
    constexpr int total() const { return copies + moves; }
    constexpr cmchecker() = default;
    constexpr cmchecker(const cmchecker& c) : copies(c.copies + 1) {}
    constexpr cmchecker(cmchecker&& c) noexcept : moves(c.moves + 1)  {}
    constexpr cmchecker& operator=(const cmchecker& c) { copies = c.copies + 1; return *this;  }
    constexpr cmchecker& operator=(cmchecker&& c) noexcept { moves = c.moves + 1; return *this; }
};

constexpr std::array<cmchecker, 3> checkers{};
constexpr int cmtotal1 = (a | a <- checkers)[0].total();
static_assert(cmtotal1 == 0);
constexpr int cmtotal2 = (*(b | a <- range(0, 3), b <- (d | c <- range(0, a), d <- checkers)).begin()).total();
static_assert(cmtotal2 == 0);
constexpr int cmtotal3 = ((b | b <- checkers)[a] | a <- range(0, 3))[2].total();
static_assert(cmtotal3 == 0);

constexpr auto aefa4 = (((b + c) | (b, c) <- (range(0, 10), range(0, a))) | a <- range(0, 10))[9][8];
static_assert(aefa4 == 16);