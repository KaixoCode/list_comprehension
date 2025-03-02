#include "kaixo/list_comprehension.hpp"

#include <algorithm>
#include <map>
#include <vector>
#include <print>

int main() {

    using namespace kaixo;
    using namespace kaixo::variables;

    auto aoein = (std::max(a, b) | a <- range(0, 10), b <- range(0, 10));

    aoein[1];

}