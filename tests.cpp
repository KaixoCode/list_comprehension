//#include "list_comprehension.hpp"
#include <iostream>
#include <map>
#include <array>
#include <string_view>
#include <ranges>

namespace detail {
    template<class I>
    concept Referenceable = !std::same_as<I, void>;

    template<class I>
    concept LegacyIterator =
        requires(I i) {
            {   *i } -> Referenceable;
            {  ++i } -> std::same_as<I&>;
            { *i++ } -> Referenceable;
    }&& std::copyable<I>;

    template<class I>
    concept LegacyInputIterator =
        LegacyIterator<I> && std::equality_comparable<I> && requires(I i) {
        typename std::incrementable_traits<I>::difference_type;
        typename std::indirectly_readable_traits<I>::value_type;
        typename std::common_reference_t<std::iter_reference_t<I>&&,
            typename std::indirectly_readable_traits<I>::value_type&>;
        *i++;
        typename std::common_reference_t<decltype(*i++)&&,
            typename std::indirectly_readable_traits<I>::value_type&>;
            requires std::signed_integral<typename std::incrementable_traits<I>::difference_type>;
    };

    template<class I>
    concept LegacyForwardIterator =
        LegacyInputIterator<I> && std::constructible_from<I> &&
        std::is_lvalue_reference_v<std::iter_reference_t<I>> &&
        std::same_as<
        std::remove_cvref_t<std::iter_reference_t<I>>,
        typename std::indirectly_readable_traits<I>::value_type> &&
        requires(I i) {
            {  i++ } -> std::convertible_to<const I&>;
            { *i++ } -> std::same_as<std::iter_reference_t<I>>;
    };

    template<class I>
    concept LegacyBidirectionalIterator =
        LegacyForwardIterator<I> && requires(I i) {
            {  --i } -> std::same_as<I&>;
            {  i-- } -> std::convertible_to<const I&>;
            { *i-- } -> std::same_as<std::iter_reference_t<I>>;
    };
}

template<class T, std::size_t ... Is>
constexpr void print_tuple(auto& a, T& v, std::index_sequence<Is...>) {
    a << "(";
    ((a << std::get<Is>(v) << ", "), ...);
    a << std::get<sizeof...(Is)>(v);
    a << ")";
}
template<class ...Ty>
constexpr auto& operator<<(auto& a, std::tuple<Ty...>& v) {
    print_tuple(a, v, std::make_index_sequence<sizeof...(Ty) - 1>{});
    return a;
}

#include "list_comprehension.hpp"

#include <vector>



int main()
{

    using namespace kaixo;
    using namespace kaixo::lc_functions;

    constexpr static auto a = var<"a">;
    constexpr static auto b = var<"b">;
    constexpr static auto c = var<"c">;
    constexpr static auto d = var<"d">;
    constexpr static auto e = var<"e">;
    constexpr static auto x = var<"x">;
    constexpr static auto y = var<"y">;
    constexpr static auto z = var<"z">;
    constexpr static auto key = var<"key">;
    constexpr static auto value = var<"value">;

    //std::vector rs10 = lc[a | a <- range(2, inf), size(lc[b | b <- range(1, 20), a % b == 0]) == 20];


    std::vector oamep = lc[y | a <- range(0, 5), y <<= 2 * size(lc[b | b <- range(0, a), c <- range(0, a)])];

    //std::vector primes = lc[x | a <- range(1, inf), x <<= a*2-1, size(lc[b | b <- range(2, inf), x % b == 0, brk <<= b > sqrt(x)]) == 0];

    //
    //int val = 10;
    //auto rs10 = lc[a | b <- range(0, size), a <<= b + val];
    //std::vector rs101 = rs10;{ size=109760 }
    //val = 20;
    //std::vector rs102 = rs10;
    

    return 0;
    
    //std::vector<std::vector<int>> r0 = lc[lc[x | x <- range(0, a)] | a <- range(0, 10)];

    //std::vector r1 = lc[(a, b, c) | c <- range(1, 11), b <- range(1, c), a <- range(1, b), a*a + b*b == c*c];
    //std::vector<char> chars{ 'a', 'b', 'c' };
    //std::string r2 = lc[d | d <- chars];
    //std::vector r3 = lc[a + b | (a, b) <- (range(0, 5), range(0, 5))];
    //std::map<int, int> data{ { 1, 2 }, { 3, 4 }, { 5, 6 } };
    //std::vector r4 = lc[value + a | ((key, value), a) <- (data, range(0, 5))];
    //std::vector<std::vector<int>> xxs{ { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 } };
    //std::vector rs00 = lc[a | b <- xxs, a <- b];
    //std::vector<std::pair<int, int>> data2{ { 1, 5 }, { 5, 4 }, { 3, 4 } };
    //std::vector r10 = lc[max(a, b) | (a, b) <- data2];
    //std::vector r14 = lc[x | x <- range(0, inf), brk <<= x == 100];

    ////std::vector r15 = lc[lc[y | y <- range(0, inf)] | x <- range(0, inf)];


    //constexpr auto size = 4;
    //std::vector rs02 = lc[(a, b) | a <- range(0, size), b <- range(0, size)];
    //std::vector rs03 = lc[(a, b) | a <- range(0, size), b <- range(0, a)];
    //std::vector rs04 = lc[(a, b) | a <- range(0, size), c <<= a * 2, b <- range(0, c)];
    //std::vector rs05 = lc[(a, b) | a <- range(0, size), c <<= a * 2, b <- range(0, c), brk <<= a == size / 2];
    //std::vector rs06 = lc[a | a <- (range(0, size), range(0, size))];
    //std::vector rs07 = lc[(b, c) | a <- (range(0, size), range(0, size)), (b, c) <<= a];
    //std::vector rs08 = lc[c | a <- range(0, size), b <- range(0, size), c <<= (a, b)];
    //std::vector rs09 = lc[(b, a) | b <- range(0, size), b % 2 == 0, a <- lc[d | c <- range(0, size), d <<= c * b]];

}