#include "kaixo/list_comprehension.hpp"

#include <algorithm>
#include <map>
#include <vector>
#include <print>

int main() {

    using namespace kaixo;
    using namespace kaixo::variables;

    constexpr bool aeio = std::random_access_iterator<range<int, int>::iterator>;
    constexpr bool aeion = std::ranges::random_access_range<range<int, int>>;

    constexpr auto aoein = (a <- range(0, 10), b <- range(0, 10));
    constexpr auto sroig = aoein[51];

}