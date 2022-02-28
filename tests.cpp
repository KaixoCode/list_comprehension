#include "list_comprehension.hpp"
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
int main()
{
	using namespace kaixo;
    using namespace kaixo::lc_operators;
    using namespace kaixo::lc_functions;
    using namespace std::string_view_literals;

    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;
    constexpr auto d = var<"d">;

    constexpr auto aeino = a * b;

    constexpr auto res = lc[(a, b, c) | c <- range(0, inf), b <- range(1, 10), a <- range(1, 11), a * a + b * b == c * c];
    constexpr auto v1 = res[0];
    
    constexpr auto rs2 = lc[(a + b + c) | (a, b, c) <- (range(0, 10), range(0, 10), range(0, 10))];
    constexpr auto v2 = rs2[6];
    
    constexpr auto rs4 = lc[(a, b, c) | (a, b) <- lc[(a, b) | a <- range(0, inf), b <- range(0, 10)], c <- range(0, 10)];
    constexpr auto v4 = rs4[106];
    
    constexpr auto rs5 = lc[lc[lc[c | a <- range(0, inf), c <<= a * b * d] | b <- range(0, inf)] | d <- range(0, inf)];
    constexpr auto v5 = rs5[5][5];

    //constexpr auto aine = [] (auto& v) requires (std::same_as<void, decltype(v)>) {};

    //constexpr auto paepm = a <<= a * a + b * b == c * c;

    using twn = tuple_with_names<std::tuple<var_t<"a">, var_t<"b">, var_t<"c">>, std::tuple<int, int, int>>;
    //constexpr auto aome = has_tags<twn, var_t<"b">>;
    //constexpr auto ioen = decltype(paepm)::callable_with<twn&>;
    //constexpr twn aiofne{ { 3, 1 } };
    //constexpr auto aoinefo = a * b;
    //constexpr auto ofoaine = aoinefo(aiofne);
    //
    //constexpr auto einfan = std::invocable<decltype(aoinefo), twn&>;

    //constexpr auto rs5 = lc[c | a <- range(0ll, inf), b <<= a * a, c <<= b * b, a * 100 < c, c != 100];
    //constexpr auto v5 = rs5[3099];
    
    //constexpr twn aefa{ { 5, 6, 7 } };
    //constexpr auto rs6 = lc[c | a <- range(1, 10), c <<= b * a];
    //constexpr auto rs8 = rs6(aefa);
    //constexpr auto oaien = rs8.data.lazyData;
    //using aione = decltype(rs8)::final_named_tuple_type::type;
    //constexpr auto v8 = rs8[0];
    //
    //constexpr auto rs9 = lc[lc[lc[b | a <- range(0, inf), b <<= a * c * d] | c <- range(0, inf)] | d <- range(0, inf)];
    //using aefae = decltype(rs9)::needed_names;
    //using aefaf = decltype(rs9)::final_named_tuple_type;
    //using aoine = std::tuple_element_t<4, aefae>;
    //
    //constexpr auto v9 = rs9[20][3];


    return 0;
}