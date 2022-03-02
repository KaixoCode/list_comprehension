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


namespace kaixo {

    inline namespace detail {

        // Some handy tuple operations
        template<class> struct tail; template<class Ty, class ...Tys> struct tail<std::tuple<Ty, Tys...>> { using type = std::tuple<Tys...>; }; 
        template<class Ty> using tail_t = typename tail<Ty>::type;
        template<class> struct head; template<class Ty, class ...Tys> struct head<std::tuple<Ty, Tys...>> { using type = Ty; };
        template<class Ty> using head_t = typename head<Ty>::type;

        // Remove duplicate types from tuple
        template<std::size_t i, class Tuple, std::size_t... is>
        constexpr auto element_as_tuple(Tuple tuple, std::index_sequence<is...>) {
            if constexpr (!(std::is_same_v<std::tuple_element_t<i, Tuple>, std::tuple_element_t<is, Tuple>> || ...))
                return std::tuple<std::tuple_element_t<i, Tuple>>(std::get<i>(tuple)); else return std::make_tuple(); }
        template<class Tuple, std::size_t... is> constexpr auto make_tuple_unique(Tuple tuple, std::index_sequence<is...>) {
            return std::tuple_cat(element_as_tuple<is>(tuple, std::make_index_sequence<is>())...); }
        template<class... Tuples> constexpr auto make_tuple_unique(Tuples... tuples) {
            return make_tuple_unique(std::tuple_cat(tuples...), std::make_index_sequence<std::tuple_size_v<decltype(std::tuple_cat(tuples...))>>{}); }
        template<class ...Tys> using unique_tuple_t = decltype(make_tuple_unique(std::declval<std::decay_t<Tys>>()...));

        // Specialization of templated class
        template<class, template<class...> class> struct is_specialization : std::false_type {};
        template<template<class...> class Ref, class... Args> struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
        template<class Test, template<class...> class Ref> concept specialization = is_specialization<std::decay_t<Test>, Ref>::value;

        // Get index of type in tuple
        template<class T, class E, std::size_t I> struct tuple_index_r;
        template<class F, class ...R, class E, std::size_t I>
        struct tuple_index_r<std::tuple<F, R...>, E, I> : public std::conditional<std::is_same<E, F>::value,
            std::integral_constant<std::size_t, I>, tuple_index_r<std::tuple<R...>, E, I + 1>>::type{};
        template<class E, std::size_t I> struct tuple_index_r<std::tuple<>, E, I> {};
        template<class E, class Tuple> constexpr static std::size_t tuple_index = tuple_index_r<Tuple, E, 0>::value;

        // Check if type is in tuple
        template<class T, class Ty> struct in_tuple_d;
        template<class T, class ...Tys> struct in_tuple_d<T, std::tuple<Tys...>> : std::bool_constant<std::disjunction_v<std::is_same<T, Tys>...>> {};
        template<class T, class Ty> concept in_tuple = in_tuple_d<std::decay_t<T>, Ty>::value;
        template<class T, class Ty> concept not_in_tuple = !in_tuple_d<std::decay_t<T>, Ty>::value;

        // Concat tuples
        template<class ...Tys> using tuple_cat_t = decltype(std::tuple_cat(std::declval<Tys>()...));

        // Removes all types in tuple from other tuple
        struct dud {}; // dud type
        template<class Ty, class T> struct remove_from;
        template<class Ty, class ...V> struct remove_from<Ty, std::tuple<V...>> {
            using type = tail_t<unique_tuple_t<std::tuple<dud, std::conditional_t<in_tuple<V, Ty>, dud, V>...>>>; };
        template<class Ty, class V> using remove_from_t = typename remove_from<Ty, V>::type;

        template<class Ty> struct fake { using type = Ty; }; // Fake type
    }

    template<std::size_t N> struct tag_t { 
        char value[N - 1];
        constexpr tag_t(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
    };

    template<tag_t Name> struct var_t { constexpr static auto name = Name; using definitions = std::tuple<var_t<Name>>; using dependencies = std::tuple<var_t<Name>>; };
    template<tag_t Name> constexpr auto var = var_t<Name>{};

    // Concepts
    template<class Ty> concept var_type = requires() { Ty::name; };
    template<class Ty> concept collection = specialization<Ty, std::tuple>;
    template<class Ty> concept has_dependencies = requires () { typename Ty::dependencies; };
    template<class Ty> concept has_definitions = requires () { typename Ty::definitions; };
    template<class Ty, class Arg> concept has_definition_types = requires () { typename Ty::template definition_types<Arg>; };
    template<class Ty> concept container = requires(const std::decay_t<Ty> ty) { { ty.begin() }; { ty.end() }; };

    // Get iterator type by checking return type of begin on const ref of type
    template<class Ty> using iterator_type_t = decltype(std::declval<const std::decay_t<Ty>&>().begin());

    // Retrieve certain aspects of list comprehension parts
    template<class> struct get_dependencies { using type = std::tuple<>; };
    template<has_dependencies Ty> struct get_dependencies<Ty> { using type = typename Ty::dependencies; };
    template<class ...Tys> struct get_dependencies<std::tuple<Tys...>> { using type = tuple_cat_t<typename get_dependencies<Tys>::type...>; };
    template<class Ty> using get_dependencies_t = typename get_dependencies<std::decay_t<Ty>>::type;

    template<class> struct get_definitions { using type = std::tuple<>; };
    template<has_definitions Ty> struct get_definitions<Ty> { using type = typename Ty::definitions; };
    template<class ...Tys> struct get_definitions<std::tuple<Tys...>> { using type = tuple_cat_t<typename get_definitions<Tys>::type...>; };
    template<class Ty> using get_definitions_t = typename get_definitions<std::decay_t<Ty>>::type;
    
    template<class> struct get_iterator_data { using type = char; };
    template<container Ty> struct get_iterator_data<Ty> { using type = iterator_type_t<Ty>; };
    template<class ...Tys> struct get_iterator_data<std::tuple<Tys...>> { using type = std::tuple<typename get_iterator_data<Tys>::type...>; };
    template<class Ty> using get_iterator_data_t = typename get_iterator_data<std::decay_t<Ty>>::type;

    template<class, class> struct get_definition_types { using type = std::tuple<>; };
    template<class Arg, container Ty> requires (!has_definition_types<Ty, Arg>) struct get_definition_types<Arg, Ty> { using type = std::tuple<typename Ty::value_type>; };
    template<class Arg, has_definition_types<Arg> Ty> struct get_definition_types<Arg, Ty> { using type = typename Ty::template definition_types<Arg>; };
    template<class Arg, class ...Tys> struct get_definition_types<Arg, std::tuple<Tys...>> { using type = tuple_cat_t<typename get_definition_types<Arg, Tys>::type...>; };
    template<class Arg, class Ty> using get_definition_types_t = typename get_definition_types<Arg, std::decay_t<Ty>>::type;

    // Named tuple, connects types to names
    template<collection Vars = std::tuple<>, collection Types = std::tuple<>> struct named_tuple : public Types {
        static_assert(std::tuple_size_v<Vars> == std::tuple_size_v<Types>);
        using names = Vars;
        using types = Types;

        template<class T, var_type ...V> constexpr void assign(const named_tuple<std::tuple<V...>, T>& vals) { (set<V>(vals.get<V>()), ...); }
        template<in_tuple<Vars> Var> constexpr decltype(auto) get() const { return std::get<tuple_index<Var, Vars>>(*this); }
        template<not_in_tuple<Vars> Var> constexpr decltype(auto) get() const {}
        template<in_tuple<Vars> Var, class Ty> constexpr void set(Ty&& val) { std::get<tuple_index<Var, Vars>>(*this) = std::forward<Ty>(val); }
        template<not_in_tuple<Vars> Var, class Ty> constexpr void set(Ty&&) {}
    };

    template<class Lambda, collection Vars> struct expression_t : Lambda, fake<Vars> {
        using dependencies = unique_tuple_t<Vars>;
        constexpr int execute(auto& vals, auto&) const { return Lambda::operator()(vals) ? 0 : 2; }
    };

    template<class Ty> concept expression = specialization<Ty, expression_t>;

    template<class Lambda, collection Vars> struct break_condition_t : Lambda, fake<Vars> {
        using dependencies = unique_tuple_t<Vars>;
        constexpr int execute(auto& vals, auto&) const { return Lambda::operator()(vals) ? 0 : 1; }
    };

    template<class Ty> concept break_condition = specialization<Ty, break_condition_t>;

    template<class Definition, var_type Var> struct var_definition : Definition, Var {
        using dependencies = get_dependencies_t<Definition>;
        using definitions = std::tuple<Var>;
        template<class Arg> using definition_types = std::tuple<decltype(std::declval<Definition>()(std::declval<Arg>()))>;
        constexpr int execute(auto& vals, auto&) const { vals.set<Var>(Definition::operator()(vals)); return 0; }
    };

    template<class Add, class Ty> struct add_additional { using type = Ty; };
    template<class Add, has_dependencies Ty> struct add_additional<Add, Ty> { using type = typename Ty::template add_additional<Add>; };
    template<class Add, class ...Tys> struct add_additional<Add, std::tuple<Tys...>> { using type = std::tuple<typename add_additional<Add, Tys>::type...>; };
    template<class Add, class Ty> using add_additional_t = typename add_additional<Add, Ty>::type;

    template<collection Containers, collection Vars, class Additional = named_tuple<>>
    struct linked_container_t : add_additional_t<Additional, Containers> {
        using containers = add_additional_t<Additional, Containers>;

        template<class Add> using add_additional = linked_container_t<Containers, Vars, Additional>;

        using definitions = Vars;
        using dependencies = get_dependencies_t<containers>;
        template<class Arg> using definition_types = get_definition_types_t<Arg, containers>;

        class iterator {
        public:
            using iterator_data = get_iterator_data_t<containers>;
            iterator_data data;

            constexpr iterator() {}
            constexpr iterator(const containers& me, bool e) { e ? end<0>(me) : begin<0>(me); }
            
            constexpr iterator& operator=(const iterator& other) { data = other.data; return *this; }
            constexpr iterator& operator=(iterator&& other) { data = std::move(other.data); return *this; }

            constexpr iterator& operator++() { increment<0>(); return *this; }

            constexpr bool operator==(const iterator& other) const { return equal<0>(other); }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr decltype(auto) operator*() { return value(std::make_index_sequence<std::tuple_size_v<containers>>{}); }

        private:
            template<std::size_t I> constexpr void increment() {
                ++std::get<I>(data);
                if constexpr (I != std::tuple_size_v<containers> - 1) increment<I + 1>();
            }

            template<std::size_t I> constexpr void begin(const containers& me) {
                std::get<I>(data) = std::get<I>(me).begin();
                if constexpr (I != std::tuple_size_v<containers> - 1) begin<I + 1>(me);
            }

            template<std::size_t I> constexpr void end(const containers& me) {
                std::get<I>(data) = std::get<I>(me).end();
                if constexpr (I != std::tuple_size_v<containers> - 1) end<I + 1>(me);
            }

            template<std::size_t I> constexpr bool equal(const iterator& other) const {
                if constexpr (I == std::tuple_size_v<containers>) return false;
                else return std::get<I>(data) == std::get<I>(other.data) || equal<I + 1>(other);
            }

            template<std::size_t ...Is> constexpr auto value(std::index_sequence<Is...>) {
                if constexpr (sizeof...(Is) == 1) return *std::get<Is...>(data);
                else return std::tuple{ *std::get<Is>(data)... };
            }
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return iterator{ *this, false }; }
        constexpr iterator end() const { return iterator{ *this, true }; }
        constexpr void give(auto& vals) { give<0>(vals); }

        template<std::size_t I> constexpr auto give(auto& vals) {
            using type = std::tuple_element_t<I, containers>;
            if constexpr (has_dependencies<type>) std::get<I>(*this).give(vals);
            if constexpr (I != std::tuple_size_v<containers> - 1) give<I + 1>(vals);
        }
    };

    template<class Ty> concept linked_container = specialization<Ty, linked_container_t>;

    template<expression Result, collection Parts, class Additional = named_tuple<>>
    struct list_comprehension_construct {
        Result result; // Expression that determines output type
        Parts parts;   // All parts of the comprehension
        Additional additional; // Additional variables passed by an outside source

        // Get all dependencies and definitions of the parts, and deduce any leftover dependencies 
        using prior_dependencies = tuple_cat_t<get_dependencies_t<Parts>, get_dependencies_t<Result>>;
        using definitions = tuple_cat_t<get_definitions_t<Parts>, get_definitions_t<Additional>>;
        using dependencies = remove_from_t<definitions, prior_dependencies>;
    };

    template<expression Result, collection Parts, class Additional = named_tuple<>>
    struct list_comprehension : list_comprehension_construct<Result, Parts, Additional> {

        // recursively deduce types of all variables
        template<std::size_t I> struct named_tuple_type_i {
            using prev_type = typename named_tuple_type_i<I - 1>::type;
            using part_type = std::tuple_element_t<I - 1, Parts>;
            using type = named_tuple<tuple_cat_t<typename prev_type::names, get_definitions_t<part_type>>,
                tuple_cat_t<typename prev_type::types, get_definition_types_t<prev_type&, part_type>>>;
        };

        template<> struct named_tuple_type_i<0> { using type = Additional; };
        using named_tuple_type = typename named_tuple_type_i<std::tuple_size_v<Parts>>::type;

        using size_type = std::size_t;
        using value_type = decltype(std::declval<Result>()(std::declval<named_tuple_type&>()));

        class iterator {
            constexpr static int BREAK = 1;
            constexpr static int AGAIN = 2;
        public:
            using iterator_data = get_iterator_data_t<Parts>;

            constexpr iterator() {}
            constexpr iterator(list_comprehension* me, bool end) : me(me), end(!end) { if (!end) prepare(); }

            bool end = true;
            list_comprehension* me = nullptr;
            named_tuple_type values{};
            iterator_data data{};

            constexpr iterator& operator++() {
                int _code = 0;
                do increment<std::tuple_size_v<Parts> - 1>(), _code = execute<0>();
                while (!(_code & BREAK) && (_code & AGAIN));
                return *this;
            }

            constexpr bool operator==(const iterator& other) const { return other.data == data && other.end == end || end == true && other.end == true; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr decltype(auto) operator*() { return me->result(values); }

        private:
            template<std::size_t I> constexpr void increment() {
                using type = std::tuple_element_t<I, Parts>;
                if constexpr (linked_container<type>) { // if it's a linked container, do increment
                    auto& _part = std::get<I>(me->parts);
                    auto& _data = std::get<I>(data);
                    if (++_data == _part.end()) { // Increment and check for end
                        if constexpr (I != 0) increment<I - 1>(); // recurse down to next container
                        _part.give(values); // Give values to the part
                        _data = _part.begin(); // Reset to begin
                        if constexpr (I == 0) { end = true; return; } // reached end of final container, so end = true
                    }   // Assign the current value to the values tuple
                    values.assign(named_tuple<get_definitions_t<type>, get_definition_types_t<named_tuple_type&, type>>{ *_data });
                }
                else if constexpr (I == 0) end = true; // We're at the end!
                else increment<I - 1>();
            }

            template<std::size_t I> constexpr int execute() {
                using type = std::tuple_element_t<I, Parts>;
                int _code = 0;
                if constexpr (!linked_container<type>) { // containers don't execute anything
                    auto& _part = std::get<I>(me->parts);
                    auto& _data = std::get<I>(data);
                    _code |= _part.execute(values, _data);
                }
                if constexpr (I != std::tuple_size_v<Parts> - 1) return _code | execute<I + 1>(); // Recurse to next element
                else return _code;
            }

            template<std::size_t I> constexpr void begin() {
                using type = std::tuple_element_t<I, Parts>;
                if constexpr (linked_container<type>) {
                    auto& _part = std::get<I>(me->parts);
                    auto& _data = std::get<I>(data);
                    _part.give(values); // Give current values
                    _data = _part.begin(); // Set iterator to begin, and assign to values tuple
                    values.assign(named_tuple<get_definitions_t<type>, get_definition_types_t<named_tuple_type&, type>>{ *_data });
                }   // Recurse if not at end
                if constexpr (I != std::tuple_size_v<Parts> - 1) begin<I + 1>();
            }

            constexpr void prepare() {
                begin<0>(); // Set iterators to begin
                int _code = execute<0>(); // Check initial constraints
                if (!(_code & BREAK) && (_code & AGAIN)) operator++(); // Set to valid state if currently not.
            }
        };

        constexpr iterator begin() { return iterator{ this, false }; }
        constexpr iterator end() { return iterator{ this, true }; }
        constexpr value_type operator[](size_type index) {
            auto _it = begin();
            while (index--) ++_it;
            return *_it;
        }
    };

    struct inf_t {};
    constexpr inf_t inf;

    template<class Ty, class Var = dud>
    class range_t {
    public:
        using dependencies = std::conditional_t<var_type<Var>, std::tuple<Var>, std::tuple<>>;
        template<class Add> using add_additional = range_t<Ty, Var>;

        constexpr void give(auto& vals) { if constexpr (var_type<Var>) m_End = vals.get<Var>(); }

        using value_type = Ty;
        using size_type = std::size_t;
    
        constexpr range_t() : m_Start(), m_End() {}
        constexpr range_t(const Ty& a) : m_Start(), m_End(a) {}
        constexpr range_t(const Ty& a, const Ty& b) : m_Start(a), m_End(b) {}
        constexpr range_t(const Ty& a, const Var&) : m_Start(a), m_End(std::numeric_limits<Ty>::max()) {}
        constexpr range_t(const Ty& a, inf_t) : m_Start(a), m_End(std::numeric_limits<Ty>::max()) {}
    
        class iterator {
        public:
            using value_type = range_t::value_type;
            using size_type = range_t::size_type;
    
            constexpr iterator() : val() {}
            constexpr iterator(iterator&& val) : val(val.val) {}
            constexpr iterator(const iterator& val) : val(val.val) {}
            constexpr iterator(Ty val) : val(val) {}
            constexpr iterator& operator=(iterator& other) { val = other.val; return *this; }
            constexpr iterator& operator=(const iterator& other) { val = other.val; return *this; }
            constexpr iterator& operator++() { ++val; return *this; }
            constexpr iterator& operator--() { --val; return *this; }
            constexpr bool operator==(const iterator& other) const { return other.val == val; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr Ty operator*() const { return val; }
    
        private:
            Ty val;
        };
    
        using const_iterator = iterator;
    
        constexpr iterator begin() const { return iterator{ m_Start }; }
        constexpr iterator end() const { return iterator{ m_End + 1 }; }
        constexpr size_type size() const { return m_End - m_Start; }
        constexpr Ty operator[](size_type index) const { return m_Start + static_cast<Ty>(index); }
    
    private:
        Ty m_Start;
        Ty m_End;
    
        friend class iterator;
    };

    //template<class D>
    //class dependant_range_t {
    //public:
    //    using container = range_t<D>;
    //};

    namespace lc_operators {

        // Overloads for using several type groups in expressions
        template<class T, var_type Ty> constexpr decltype(auto) use(T& vals, Ty&) { return vals.get<Ty>(); }
        template<class T, expression Ty> constexpr decltype(auto) use(T& vals, Ty&& expr) { return expr(vals); }
        template<class T, class Ty> constexpr decltype(auto) use(T& vals, Ty&& val) { return std::forward<Ty>(val); }

#define create_op(op)                                                                                                        \
        template<class A, class B> constexpr decltype(auto) operator op(A&& a, B&& b) {                                      \
            return expression_t{ [a = std::forward<A>(a), b = std::forward<B>(b)]<class Ty>(Ty& vals) {                      \
                return use(vals, a) op use(vals, b); }, fake<tuple_cat_t<get_dependencies_t<A>, get_dependencies_t<B>>>{} }; \
        }

        create_op(+) create_op(-) create_op(*) create_op(/ ) create_op(== ) create_op(!= ) create_op(<= );
        create_op(>= ) create_op(> ) create_op(< ) create_op(%) create_op(<=> ) create_op(<< ) create_op(>> );
        create_op(&) create_op(| ) create_op(&&) create_op(|| );

#define create_uop(op)                                                                   \
        template<class A> constexpr decltype(auto) operator op(A&& a) {                  \
            return expression_t{ [a = std::forward<A>(a)]<class Ty>(Ty& vals) {          \
                return op use(vals, a); }, fake<tuple_cat_t<get_dependencies_t<A>>>{} }; \
        }

        create_uop(~) create_uop(!) create_uop(*) create_uop(&);
    }

}


#include <vector>
int main()
{

    using namespace kaixo;

    constexpr auto a = var<"a">;
    constexpr auto b = var<"b">;
    constexpr auto c = var<"c">;
    constexpr auto d = var<"d">;

    constexpr expression_t exp1{ [](auto& vals) {
        return std::tuple{ vals.get<var_t<"a">>(), vals.get<var_t<"d">>() }; }, 
        fake<std::tuple<var_t<"a">, var_t<"b">, var_t<"c">>>{} 
    };

    constexpr expression_t exp2{ [](auto& vals) { return vals.get<var_t<"b">>(); }, fake<std::tuple<var_t<"b">>>{} };
    constexpr expression_t exp3{ [](auto& vals) { return vals.get<var_t<"a">>() + 10; }, fake<std::tuple<var_t<"a">>>{} };

    using lctype = list_comprehension_construct<decltype(exp1), std::tuple<
        linked_container_t<std::tuple<range_t<int>>, std::tuple<var_t<"d">>>,
        linked_container_t<std::tuple<range_t<int, var_t<"d">>>, std::tuple<var_t<"a">>>,
        var_definition<decltype(exp3), var_t<"b">>,
        var_definition<decltype(exp2), var_t<"c">>
    >>;

    using oirn = linked_container_t<std::tuple<range_t<int>>, std::tuple<var_t<"a">>>::containers;
    using fadeae = std::tuple_element_t<0, oirn>;
    auto hthd2 = linked_container_t<std::tuple<range_t<int>>, std::tuple<var_t<"d">>>{ range_t{ 5, 8 } };
    auto hthd1 = linked_container_t<std::tuple<range_t<int, var_t<"d">>>, std::tuple<var_t<"a">>>{ range_t<int, var_t<"d">>{ 5, d }};
    list_comprehension lcef{ lctype{ exp1, { hthd2, hthd1, { exp3 }, { exp2 } } } };
    using feafa = decltype(lcef)::named_tuple_type::names;
    using gsroi1 = std::tuple_element_t<0, feafa>;
    using gsroi2 = std::tuple_element_t<1, feafa>;
    using gsroi3 = std::tuple_element_t<2, feafa>;
    auto oianef1 = lcef[0];
    auto oianef2 = lcef[1];
    auto oianef3 = lcef[2];
    auto oianef4 = lcef[3];
    auto oianef5 = lcef[4];
    auto oianef6 = lcef[5];
    auto oianef7 = lcef[6];

    using oaine = lctype::dependencies;
    //using gsroi1 = std::tuple_element_t<0, oaine>;
    //using gsroi2 = std::tuple_element_t<1, oaine>;
    //using gsroi3 = std::tuple_element_t<2, oaine>;

    //constexpr expression_t aenfa = a * 10 + 30 * b & 1 / c;
    //using aone = decltype(aenfa)::dependencies;
    //using oaine1 = std::tuple_element_t<0, aone>;
    //using oaine2 = std::tuple_element_t<1, aone>;
    //using oaine3 = std::tuple_element_t<2, aone>;

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