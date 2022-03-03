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

    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;
    constexpr auto d = var<"d">;


    //std::vector<std::tuple<int, int>> gaega = lc[(b, a) | a <- range_t{ 0, 6 }, b <- range_t{ 0, a }];
    
    //constexpr auto aionefo = container<decltype(lc[c | c <- range_t{ 0, a }])>;
        //using aoinooe = add_additional_t<decltype(aneoofnia)::named_tuple_type, decltype(aoienfo)>::containers;
    

    std::vector<std::tuple<int, int>> aneo1 = lc[(a, b) | a <- range(0, 3), c <<= a * 1, b <- range(0, c), b != 0];
    std::vector<std::tuple<int, int>> aneo2 = lc[(a, b) | a <- range(0, 3), b <- range(0, a), b != 0];

    std::vector<std::tuple<int, int>> feoin = lc[(a, b) | a <- range(0, 4), c <<= a * 2, b <- range(fake<int>{}, a, c)];
    
    std::vector<std::tuple<int, int>> faefa = lc[(a, b) | a <- range(0, 4), c <<= a * 2, b <- range(fake<int>{}, a, c)];

    constexpr auto ani = lc[(a, b) | a <- range(0, 10), b <- range(0, 10)];
    constexpr auto aoin = ani[121];

    return 0;
	//using namespace kaixo;
    //using namespace kaixo::lc_operators;
    //using namespace kaixo::lc_functions;
    //using namespace std::string_view_literals;
    //
    //constexpr auto a = var<"a">;
    //constexpr auto b = var<"b">;
    //constexpr auto c = var<"c">;
    //constexpr auto d = var<"d">;
    //constexpr auto i = var<"i">;
    //constexpr auto j = var<"j">;
    //constexpr auto x = var<"x">;
    //
    //constexpr auto aeino = a * b;

    //constexpr auto res = lc[(a, b, c) | c <- range(0, inf), b <- range(1, 10), a <- range(1, 11), a * a + b * b == c * c];
    //constexpr auto v1 = res[0];
    //
    //constexpr auto rs2 = lc[(a + b + c) | (a, b, c) <- (range(0, 10), range(0, 10), range(0, 10))];
    //constexpr auto v2 = rs2[6];
    //
    //constexpr auto rs4 = lc[(a, b, c) | (a, b) <- lc[(a, b) | a <- range(0, inf), b <- range(0, 10)], c <- range(0, 10)];
    //constexpr auto v4 = rs4[106];
    //
    //constexpr auto rs5 = lc[lc[lc[c * a * b | (a, c) <- (range(0, inf), range(0, inf))] | b <- range(0, inf), c <<= b * d] | d <- range(0, inf)];
    //constexpr auto v5 = rs5[5][4][5];
    //
    //using namespace std::views;
    //
    //auto rs6 = lc[lc[(i, j) | i <- range(1, 3)] | j <- range(1, inf)];
    //
    //for (auto i : rs6 | take(5))
    //{
    //    std::cout << "[";
    //    for (auto j : i)
    //        std::cout << j; 
    //    std::cout << "],";
    //}

    //constexpr auto oane =   ((a, b, c, d) | a <- range(0, 10), b <- range(0, 10), c <<= a * 10, d <<= b * c, b != c, brk <<= b == 100);
    //constexpr auto oan1 = lc[(a, b, c, d) | a <- range(0, 3), b <- range(0, 3), c <<= a * 1, d <<= b * 1, c != d, brk <<= b == 2];
    //constexpr auto enaf1 = *++oan1.begin();

    //constexpr auto aona = lc[lc[(b, c, a) | a <- range(1, 5), c <<= a * d, b <- range(1, 5), b != a] | d <- range(1, 5)];
    //constexpr auto aoenf = aona[2][15];

    //constexpr auto aienf = lc[a | c <- range(0, 10), a <- lc[b | b <- range(0, 10), b != c]];

    //constexpr auto oaine = lc[(a, c) | c <- range(0, 3), a <- range(0, c)];
    //constexpr auto oaine = lc[lc[a | x <- range(0, c)] | c <- range(0, 3), a <- (range(0, c), range(0, c), range(0, c))];

    //constexpr auto trngls = lc[(a, b, c) | c <- range(1, 100), b <- range(1, c), a <- range(1, b), a * a + b * b == c * c];
    //constexpr auto t1 = trngls[0];
    //constexpr auto t2 = trngls[1];
    //constexpr auto t3 = trngls[2];
    //constexpr auto t4 = trngls[3];
    //constexpr auto t5 = trngls[4];
    //constexpr auto t6 = trngls[5];
    //constexpr auto t7 = trngls[6];
    //constexpr auto t8 = trngls[7];
    //constexpr auto t9 = trngls[8];

    //constexpr auto aeonf = lc[lc[c | c <- range(0, d)] | d <- range(1, 10)];
    //constexpr auto eina = aeonf[9][7];
    //std::vector<std::vector<int>> afa;
    //auto aeion = lc[a | b <- afa, a <- b];
    //auto irgnos = aeion[0];
    


    //constexpr auto faef = container_type<expression_container<expression<decltype([]() {}), std::tuple<>>>> ;

    //constexpr auto aoine = has_type_v<var_t<"a">, tuple_with_names<std::tuple<var_t<"a">>, std::tuple<int>>::names>;


    //constexpr auto enaf2 = *++oan1.begin();
    //constexpr auto enaf3 = *++++oan1.begin();
    //constexpr auto enaf4 = *++++++oan1.begin();
    //constexpr auto enaf5 = *++++++++oan1.begin();
    //constexpr auto enaf6 = oan1.end() == ++++++++++oan1.begin();
    //constexpr auto enaf7 = *++++++++++++oan1.begin();
    //constexpr auto enaf8 = oan1.end() == ++++++++++++++oan1.begin();

    //using esgrr = decltype(oane)::expression_type;
    //using aeafa = decltype(oane)::content_type;
    //using efaef = decltype(oan1)::iterators;

    //using oaine1 = std::tuple_element_t<0, aeafa>;
    //using oaine2 = std::tuple_element_t<1, aeafa>;
    //using oaine3 = std::tuple_element_t<2, aeafa>;
    //using oaine4 = std::tuple_element_t<3, aeafa>;
    //using aoine = decltype(oane)::reduced_needed_names;
    //using aiefa = typename filter_on_tag<lc_alias_tag, aeafa>::type;
    //using afione1 = std::tuple_element_t<0, aiefa>;
    //using afione2 = std::tuple_element_t<1, aiefa>;

    //using ioane1 = list_comprehension<esgrr, aeafa>::container_tuple_type;
    //using gsrgsr3 = std::tuple_element_t<0, ioane1>;
    //using gsrgsr1 = std::tuple_element_t<1, ioane1>;
    //using ioane2 = list_comprehension<esgrr, aeafa>::constraint_tuple_type;
    //using gsrgsr2 = std::tuple_element_t<0, ioane2>;
    //using ioane3 = list_comprehension<esgrr, aeafa>::break_tuple_type;
    //using gsrgsr4 = std::tuple_element_t<0, ioane3>;  
    //using ioane6 = list_comprehension<esgrr, aeafa>::alias_tuple_type;
    //using gsrgsr6 = std::tuple_element_t<0, ioane6>;
    //using gsrgsr7 = std::tuple_element_t<1, ioane6>;

    //using aionf = typename filter_on_tag<lc_container_tag, aeafa>::type;

    //using oaine2 = typename std::tuple_element_t<0, aionf>::second_type;

    //using oaine2 = std::tuple_element_t<1, aeafa>;
    //using oaine3 = std::tuple_element_t<2, aeafa>;
    //using oaine4 = std::tuple_element_t<3, aeafa>;

    // 
    // c generate -> give to next
    // 
    // c <- [1..5], d <- [1..c]
    // 1, 1
    // 2, 1
    // 2, 2
    // 3, 1
    // 3, 2
    // 3, 3

    // Container  - 
    // Alias      - 
    // Break      - 
    // Constraint - 

    return 0;

    //std::vector<std::string> data{ "1jf1d", "afj3", "a09af", "a31" };
    //auto r11 = lc[lc[c | c <- a, isalpha(c)] | a <- data];

    //constexpr auto aine = [] (auto& v) requires (std::same_as<void, decltype(v)>) {};

    //constexpr auto paepm = a <<= a * a + b * b == c * c;

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

}