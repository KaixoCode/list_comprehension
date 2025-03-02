#include "kaixo/list_comprehension.hpp"

#include <algorithm>
#include <map>
#include <vector>
#include <print>

namespace kaixo {
    template<evaluated_range Range, class ...Vars>
    constexpr named_range<var<Vars...>, std::views::all_t<Range>> operator>>(Range&& range, var<Vars...>) {
        return { {
            .range = std::views::all(std::forward<Range>(range)),
        } };
    }    
    
    template<unevaluated_range Range, class ...Vars>
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

int main() {
    using namespace kaixo;
    using namespace kaixo::variables;

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
            a, 
            (   SELECT d 
                FROM range(0, b) AS d
                WHERE d % 2 == 0
                  AND d != 0
            ), 
            c
        FROM range(1, 11) AS c, 
             range(1,  c) AS b, 
             range(1,  b) AS a
        WHERE a * a + b * b == c * c
    );

    for (auto [a, b, c] : query2) {
        std::println("({}, {})", a, c);
        for (auto d : b) {
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