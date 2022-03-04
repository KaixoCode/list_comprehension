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

namespace std {
    template<class A> vector(A&&)->vector<typename A::value_type>;    
    template<class A> vector(A&)->vector<typename A::value_type>;
}

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


    std::vector<std::vector<int>> data{ { 0, 1, 2 }, { 3, 4, 5 }, { 6, 7, 8 } };
    std::vector rs00 = lc[a | b <- data, a <- b];


    constexpr auto size = 4;
    std::vector rs02 = lc[(a, b) | a <- range(0, size), b <- range(0, size)];
    std::vector rs03 = lc[(a, b) | a <- range(0, size), b <- range(0, a)];
    std::vector rs04 = lc[(a, b) | a <- range(0, size), c <<= a * 2, b <- range(0, c)];
    std::vector rs05 = lc[(a, b) | a <- range(0, size), c <<= a * 2, b <- range(0, c), brk <<= a == size / 2];
    std::vector rs06 = lc[a | a <- (range(0, size), range(0, size))];
    std::vector rs07 = lc[(b, c) | a <- (range(0, size), range(0, size)), (b, c) <<= a];
    std::vector rs08 = lc[c | a <- range(0, size), b <- range(0, size), c <<= (a, b)];
    std::vector rs09 = lc[(b, a) | b <- range(0, size), b % 2 == 0, a <- lc[d | c <- range(0, size), d <<= c * b]];
    
    int val = 10;
    auto rs10 = lc[a | b <- range(0, size), a <<= b + val];
    std::vector rs101 = rs10;
    val = 20;
    std::vector rs102 = rs10;
    
    range r1{ 0, 5 };
    auto rs11 = lc[b | a <- range(5, size + 5), b <- r1];
    std::vector rs111 = rs11;
    //r1 = { 5, 10 };
    std::vector rs112 = rs11;

    return 0;

}