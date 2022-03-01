#pragma once
#include <iostream>
#include <string_view>
#include <utility>
#include <algorithm>
#include <concepts>
#include <cstddef>

namespace kaixo {

    template<std::size_t N> struct number_t { constexpr static inline auto value = N; };

    template<std::size_t i, class Tuple, std::size_t... is>
    constexpr auto element_as_tuple(Tuple&& tuple, std::index_sequence<is...>) {
        using T = std::remove_reference_t<Tuple>;
        if constexpr (!(std::is_same_v<std::tuple_element_t<i, T>,
            std::tuple_element_t<is, T>> || ...))
            return std::tuple<std::tuple_element_t<i, T>>(
                std::get<i>(std::forward<Tuple>(tuple)));
        else
            return std::make_tuple();
    }

    template<class Tuple, std::size_t... is>
    constexpr auto make_tuple_unique(Tuple&& tuple, std::index_sequence<is...>) {
        return std::tuple_cat(element_as_tuple<is>(std::forward<Tuple>(tuple),
            std::make_index_sequence<is>())...);
    }

    template<class... Tuples>
    constexpr auto make_tuple_unique(Tuples&&... tuples) {
        auto all = std::tuple_cat(std::forward<Tuples>(tuples)...);
        return make_tuple_unique(std::move(all),
            std::make_index_sequence<std::tuple_size_v<decltype(all)>>{});
    }

    template<class ...Tys> using unique_tuple_t = decltype(make_tuple_unique(std::declval<std::decay_t<Tys>&&>()...));

    template<class> struct remove_front;
    template<class Ty, class ...Tys> struct remove_front<std::tuple<Ty, Tys...>> { using type = std::tuple<Tys...>; };

    template<class T, class Ty> struct has_type;
    template<class T, class ...Tys> struct has_type<T, std::tuple<Tys...>> : std::bool_constant<std::disjunction_v<std::is_same<T, Tys>...>> {};
    template<class T, class Ty> concept has_type_v = has_type<std::decay_t<T>, Ty>::value;

    template<class ...Tys> using tuple_cat_t = decltype(std::tuple_cat(std::declval<Tys>()...));

    template<class Ty, class T> struct types_not_in;
    template<class Ty, class ...V> struct types_not_in<Ty, std::tuple<V...>> {
        struct dud {};
        using type = typename remove_front<unique_tuple_t<
            std::tuple<dud, std::conditional_t<has_type_v<V, Ty>, dud, V>...>>>::type;
    };

    template<class Ty> concept has_value_type = requires() { typename Ty::value_type; };
    template<class Ty> concept has_range_value_type = !has_value_type<Ty> && requires() { typename std::ranges::range_value_t<Ty>; };

    template<class Ty> struct value_type;
    template<has_value_type Ty> struct value_type<Ty> { using type = typename Ty::value_type; };
    template<has_range_value_type Ty> struct value_type<Ty> { using type = typename std::ranges::range_value_t<Ty>; };
    template<class Ty> using value_type_t = typename value_type<Ty>::type;

    template<class Ty> using iterator_type_t = decltype(std::declval<std::decay_t<Ty>&>().begin());

    template<class Ty>
    concept container_type = requires(const std::decay_t<Ty> ty) {
        typename value_type_t<std::decay_t<Ty>>;
        typename iterator_type_t<std::decay_t<Ty>>;
        { ty.begin() } -> std::same_as<iterator_type_t<std::decay_t<Ty>>>;
        { ty.end() } -> std::same_as<iterator_type_t<std::decay_t<Ty>>>;
    };

    template<class Ty> concept has_names = requires () { typename std::decay_t<Ty>::names; };
    template<class Ty> concept is_named_container = container_type<Ty> && has_names<Ty>;

    template<typename Test, template<typename...> class Ref>    struct is_specialization : std::false_type {};
    template<template<typename...> class Ref, typename... Args> struct is_specialization<Ref<Args...>, Ref> : std::true_type {};
    template<typename Test, template<typename...> class Ref>      concept specialization = is_specialization<std::decay_t<Test>, Ref>::value;

    template<typename T, typename C, std::size_t I> struct tuple_index_r;
    template<typename H, typename ...R, typename C, std::size_t I>
    struct tuple_index_r<std::tuple<H, R...>, C, I> : public std::conditional<std::is_same<C, H>::value,
        std::integral_constant<std::size_t, I>, tuple_index_r<std::tuple<R...>, C, I + 1>>::type{};
    template<typename C, std::size_t I> struct tuple_index_r<std::tuple<>, C, I> {};
    template<typename T, typename C>    struct tuple_index_of : public std::integral_constant<std::size_t, tuple_index_r<T, C, 0>::value> {};

    template<class Ty>         struct as_tuple { using type = std::tuple<Ty>; };
    template<class A, class B> struct as_tuple<std::pair<A, B>> { using type = std::tuple<std::remove_const_t<A>, B>; };
    template<class ...Tys>     struct as_tuple<std::tuple<Tys...>> { using type = std::tuple<Tys...>; };
    template<class Ty>         using  as_tuple_t = typename as_tuple<Ty>::type;

    // Determines what type to store, if reference, stores reference, 
    // otherwise by value, always const because list comprehension never changes input values
    template<class Ty>
    using stored_type_t = std::conditional_t<std::is_lvalue_reference_v<Ty>, const std::decay_t<Ty>&, const std::decay_t<Ty>>;

    // ===============================================================

    // String literal wrapper for template parameter
    template<std::size_t N>
    struct tag {
        char value[N - 1];
        constexpr tag(const char(&val)[N]) : value() { std::copy_n(val, N - 1, value); }
        constexpr operator std::string_view() const { return { value, N - 1 }; }
        constexpr std::string_view str() const { return { value, N - 1 }; }
    };

    // ===============================================================

    struct lc_constraint_tag {};
    template<class Ty> concept lc_constraint = std::same_as<typename Ty::type_tag, lc_constraint_tag>;
    struct lc_break_tag {};
    template<class Ty> concept lc_break = std::same_as<typename Ty::type_tag, lc_break_tag>;
    struct lc_alias_tag {};
    template<class Ty> concept lc_alias = std::same_as<typename Ty::type_tag, lc_alias_tag>;
    struct lc_container_tag {};
    template<class Ty> concept lc_container = std::same_as<typename Ty::type_tag, lc_container_tag>;

    template<class Ty>
    concept valid_lc_arg = requires() {
        typename Ty::dependencies;
        typename Ty::definitions;
        typename Ty::type_tag;
        typename Ty::names;
    };

    template<class Ty> struct get_definitions { using type = std::tuple<>; };
    template<valid_lc_arg Ty> struct get_definitions<Ty> { using type = typename Ty::definitions; };
    template<class Ty> struct get_dependencies { using type = std::tuple<>; };
    template<valid_lc_arg Ty> struct get_dependencies<Ty> { using type = typename Ty::dependencies; };


    // Simple lambda wrappers for expression and break condition to distinguish them easily.
    template<class Lambda, class Vars> 
    struct expression : public Lambda { 
        constexpr expression(Lambda lambda, Vars) 
            : Lambda(lambda) {}

        using type_tag = lc_constraint_tag;
        using dependencies = unique_tuple_t<typename Vars::type>;
        using definitions = std::tuple<>;
        using names = std::tuple<>;
        template<class> using types = std::tuple<>;

        using Lambda::operator(); 
    };

    template<class Lambda, class Vars>
    struct break_condition : public Lambda {
        constexpr break_condition(Lambda lambda, Vars)
            : Lambda(lambda) {}

        using type_tag = lc_break_tag;
        using dependencies = unique_tuple_t<typename Vars::type>;
        using definitions = std::tuple<>;
        using names = std::tuple<>;
        template<class> using types = std::tuple<>;

        using Lambda::operator();
    };

    // ===============================================================

    template<tag Name> struct var_t {
        constexpr static auto name = Name;
        constexpr decltype(auto) operator()(auto& vals) const { return vals.get<Name>(); }
    };

    template<tag Name> constexpr auto var = var_t<Name>{};
    template<class Ty> concept is_var_type = requires() { Ty::name; };
    template<is_var_type ...Vars> struct tuple_of_vars { using vars = std::tuple<Vars...>; };
    template<class Ty, class ...As> concept has_tags = (has_type_v<As, typename std::decay_t<Ty>::names> && ...);

    template<is_var_type Var, specialization<expression> Expression>
    struct var_alias {
        using var = Var;
        using expression_type = std::decay_t<Expression>;

        using type_tag = lc_alias_tag;
        using dependencies = typename expression_type::dependencies;
        using definitions = std::tuple<Var>;
        using names = std::tuple<Var>;
        template<class Ty> using types = std::tuple<decltype(std::declval<expression_type>()(std::declval<Ty&>()))>;

        stored_type_t<Expression> expr;

        constexpr decltype(auto) operator()(auto& vals) const { return expr(vals); }

        template<class Ty>
        constexpr static bool callable_with = std::invocable<Expression, Ty>;
    };

    template<class> struct get_dependencies;
    template<class ...Tys> struct get_dependencies<std::tuple<Tys...>> { using type = tuple_cat_t<typename Tys::dependencies...>; };

    template<class> struct get_definitions; 
    template<class ...Tys> struct get_definitions<std::tuple<Tys...>> { using type = tuple_cat_t<typename Tys::definitions...>; };

    template<class, class> struct filter_on_tag;
    template<class Tag, class ...Tys> struct filter_on_tag<Tag, std::tuple<Tys...>> { 
        struct dud {};
        template<size_t I>
        using nth_type = std::tuple_element_t<I, std::tuple<Tys...>>;

        template<std::size_t ...Is>
        constexpr static auto create(std::index_sequence<Is...>)-> typename remove_front<unique_tuple_t<
            std::tuple<dud, std::conditional_t<std::same_as<typename nth_type<Is>::type_tag, Tag>, std::pair<nth_type<Is>, number_t<Is>>, dud>...>>>::type;

        using type = decltype(create(std::make_index_sequence<sizeof...(Tys)>{}));
    };

    // ===============================================================

    // Tuple with names linked, uses index of name to find correct element of tuple
    template<specialization<std::tuple> Names, specialization<std::tuple> Tuple>
    struct tuple_with_names : public Tuple {
        using type = Tuple;
        using names = Names;

        template<specialization<tuple_with_names> Ty>
        constexpr tuple_with_names& assign(Ty&& vals) {
            // Assign all values of the passed tuple_with_names
            do_assign(std::make_index_sequence<std::tuple_size_v<typename std::decay_t<Ty>::names>>{}, std::forward<Ty>(vals));
            return *this;
        }

        template<tag Name, class Ty> constexpr void set(Ty&& val) {
            std::get<tuple_index_of<Names, var_t<Name>>::value>(*this) = std::forward<Ty>(val);
        }

        template<tag Name> constexpr decltype(auto) get() const {
            return std::get<tuple_index_of<Names, var_t<Name>>::value>(*this);
        }

    private:
        template<specialization<tuple_with_names> Ty, std::size_t ...Ns>
        constexpr void do_assign(std::index_sequence<Ns...>, Ty&& vals) {
            (try_assign<Ns>(vals), ...);
        }

        template<std::size_t Ns, specialization<tuple_with_names> Ty>
        constexpr void try_assign(Ty& vals) {
            using Var = std::tuple_element_t<Ns, typename std::decay_t<Ty>::names>;
            if constexpr (has_type_v<Var, Names>)
                set<Var::name>(vals.get<Var::name>());
        }
    };

    template<specialization<std::tuple> Ty>    struct var_alias_names;
    template<specialization<var_alias> ...Tys> struct var_alias_names<std::tuple<Tys...>> { using type = std::tuple<typename Tys::var...>; };
    template<specialization<std::tuple> Ty>    using  var_alias_names_t = typename var_alias_names<Ty>::type;

    // Convert a tuple of var_aliases to a tuple of resulting types, using the Input type
    // the Input type is an instance of tuple_with_names
    template<specialization<std::tuple> Ty, specialization<tuple_with_names> Input>
    struct var_alias_types;
    template<specialization<tuple_with_names> Input, specialization<var_alias> ...Tys>
    struct var_alias_types<std::tuple<Tys...>, Input> {
        using type = std::tuple<decltype(std::declval<Tys>().expr(std::declval<Input&>()))...>;
    };
    template<specialization<std::tuple> Ty, specialization<tuple_with_names> Input>
    using var_alias_types_t = typename var_alias_types<Ty, Input>::type;

    // ===============================================================

    // Zip 2 containers into a single container of tuples, tuple_cat's if containers have tuples themselves
    template<container_type A, container_type B>
    class zip_t {
        using container_type_a = std::decay_t<A>;
        using container_type_b = std::decay_t<B>;
        using value_type_a = value_type_t<container_type_a>;
        using value_type_b = value_type_t<container_type_b>;
        using iterator_a = iterator_type_t<container_type_a>;
        using iterator_b = iterator_type_t<container_type_b>;
    public:
        using value_type = tuple_cat_t<as_tuple_t<value_type_a>, as_tuple_t<value_type_b>>;
        using size_type = std::size_t;

        using type_tag = lc_container_tag;
        using dependencies = tuple_cat_t<typename get_dependencies<container_type_a>::type, typename get_dependencies<container_type_a>::type>;
        using definitions = std::tuple<>;
        using names = std::tuple<>;
        template<class Ty> using types = std::tuple<>;

        explicit constexpr zip_t(A a, B b) : a(std::forward<A>(a)), b(std::forward<B>(b)) {}

        class iterator {
        public:
            using iterator_category = std::input_iterator_tag;
            using value_type = zip_t::value_type;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using size_type = zip_t::size_type;

            constexpr iterator() {};
            constexpr iterator(const iterator& other) : it_a(other.it_a), it_b(other.it_b) {}
            constexpr iterator(iterator_a it_a, iterator_b it_b) : it_a(it_a), it_b(it_b) {}
            constexpr iterator& operator=(const iterator& other) { it_a = other.it_a, it_b = other.it_b; return *this; }
            constexpr iterator operator++(int) { iterator _it = *this; operator++(); return _it; }
            constexpr iterator& operator++() { ++it_a, ++it_b; return *this; }

            constexpr iterator& increment(auto& vals) {
                if constexpr (lc_container<container_type_a>) it_a.increment(vals);
                else ++it_a;                
                if constexpr (lc_container<container_type_b>) it_b.increment(vals);
                else ++it_b;
                return *this;
            }

            constexpr iterator operator--(int) { iterator _it = *this; operator--(); return _it; }
            constexpr iterator& operator--() { --it_a, --it_b; return *this; }

            constexpr iterator& decrement(auto& vals) {
                if constexpr (lc_container<container_type_a>) it_a.decrement(vals);
                else --it_a;
                if constexpr (lc_container<container_type_b>) it_b.decrement(vals);
                else --it_b;
                return *this;
            }

            constexpr bool operator==(const iterator& other) const { return other.it_a == it_a || other.it_b == it_b; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr value_type operator*() const { return std::tuple_cat(std::tuple{ *it_a }, std::tuple{ *it_b }); }

        private:
            iterator_a it_a;
            iterator_b it_b;
        };

        using const_iterator = iterator;

        constexpr size_type size() const { return std::min(a.size(), b.size()); }
        constexpr iterator begin() const { return iterator{ a.begin(), b.begin() }; }
        constexpr iterator end() const { return iterator{ a.end(), b.end() }; }
        constexpr value_type operator[](size_type index) const { return std::tuple_cat(std::tuple{ a[index] }, std::tuple{ b[index] }); }

    private:
        stored_type_t<A> a;
        stored_type_t<B> b;
    };

    template<class A, class B> zip_t(A&&, B&&)->zip_t<A&&, B&&>;
    template<class A, class B> zip_t(A&, B&&)->zip_t<A&, B&&>;
    template<class A, class B> zip_t(A&&, B&)->zip_t<A&&, B&>;
    template<class A, class B> zip_t(A&, B&)->zip_t<A&, B&>;

    // Map a function over a container
    template<class Fun, container_type Ty>
    class map_t {
        using container_type = std::decay_t<Ty>;
        using container_value_type = value_type_t<container_type>;
        using function_type = std::decay_t<Fun>;
        using container_iterator = iterator_type_t<container_type>;
    public:
        using value_type = decltype(std::declval<Fun>()(*std::declval<container_iterator>()));
        using size_type = std::size_t;

        using type_tag = lc_container_tag;
        using dependencies = typename get_dependencies<container_type>::type;
        using definitions = std::tuple<>;
        using names = std::tuple<>;
        template<class Ty> using types = std::tuple<>;

        explicit constexpr map_t(Fun transform, Ty container)
            : transform(std::forward<Fun>(transform)), container(std::forward<Ty>(container)) {}

        class iterator {
        public:
            using iterator_category = std::input_iterator_tag;
            using value_type = map_t::value_type;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using size_type = map_t::size_type;

            constexpr iterator() {};
            constexpr iterator(const iterator& other) : it(other.it), transform(&other.transform) {}
            constexpr iterator(const function_type& transform, container_iterator it) : it(it), transform(transform) {}
            constexpr iterator& operator=(const iterator& other) { transform = other.transform; it = other.it; return *this; }
            constexpr iterator operator++(int) { iterator _it = *this; ++it; return _it; }
            constexpr iterator& operator++() { ++it; return *this; }

            constexpr iterator& increment(auto& vals) {
                if constexpr (lc_container<container_type>) it.increment(vals);
                else ++it;
                return *this;
            }

            constexpr iterator operator--(int) { iterator _it = *this; --it; return _it; }
            constexpr iterator& operator--() { --it; return *this; }

            constexpr iterator& decrement(auto& vals) {
                if constexpr (lc_container<container_type>) it.decrement(vals);
                else --it;
                return *this;
            }

            constexpr bool operator==(const iterator& other) const { return other.it == it; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr value_type operator*() const { return transform(*it); }

        private:
            container_iterator it;
            const function_type* transform = nullptr;
        };

        using const_iterator = iterator;

        constexpr size_type size() const { return container.size(); }
        constexpr iterator begin() const { return iterator{ transform, container.begin() }; }
        constexpr iterator end() const { return iterator{ transform, container.end() }; }
        constexpr value_type operator[](size_type index) const { return transform(container[index]); }

    private:
        function_type transform;
        stored_type_t<Ty> container;
    };

    template<class A, class B> map_t(A&, B&)->map_t<A&, B&>;
    template<class A, class B> map_t(A&, B&&)->map_t<A&, B&&>;
    template<class A, class B> map_t(A&&, B&)->map_t<A&&, B&>;
    template<class A, class B> map_t(A&&, B&&)->map_t<A&&, B&&>;

    // Cartesian product of 2 containers, result is 1 container of tuples,
    // tuple_cat's if original containers contain tuples
    template<container_type A, container_type B>
    class cart_t {
        using container_type_a = std::decay_t<A>;
        using container_type_b = std::decay_t<B>;
        using value_type_a = value_type_t<container_type_a>;
        using value_type_b = value_type_t<container_type_b>;
        using iterator_a = iterator_type_t<container_type_a>;
        using iterator_b = iterator_type_t<container_type_b>;
    public:
        using value_type = tuple_cat_t<as_tuple_t<value_type_a>, as_tuple_t<value_type_b>>;
        using size_type = std::size_t;

        using type_tag = lc_container_tag;
        using dependencies = tuple_cat_t<typename get_dependencies<container_type_a>::type, typename get_dependencies<container_type_a>::type>;
        using definitions = std::tuple<>;
        using names = std::tuple<>;
        template<class Ty> using types = std::tuple<>;

        explicit constexpr cart_t(A a, B b) : a(std::forward<A>(a)), b(std::forward<B>(b)) {}

        class iterator {
        public:
            using iterator_category = std::input_iterator_tag;
            using value_type = cart_t::value_type;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using size_type = cart_t::size_type;

            constexpr iterator() {};
            constexpr iterator(const iterator& other) : cart(other.cart), it_a(other.it_a), it_b(other.it_b) {}
            constexpr iterator(const cart_t& cart, iterator_a it_a, iterator_b it_b) : it_a(it_a), it_b(it_b), cart(&cart) {}
            constexpr iterator& operator=(const iterator& other) { cart = other.cart; it_a = other.it_a, it_b = other.it_b; return *this; }

            constexpr iterator operator++(int) { iterator _it = *this; operator++(); return _it; }
            constexpr iterator& operator++() {
                if (++it_b == cart->b.end()) {
                    it_b = cart->b.begin();
                    ++it_a;
                }
                return *this;
            }

            constexpr iterator& increment(auto& vals) {
                if constexpr (lc_container<container_type_b>) it_b.increment(vals);
                else ++it_b;

                if (it_b == cart->b.end()) {
                    if constexpr (lc_container<container_type_b>) cart->b(vals);
                    it_b = cart->b.begin();
                    if constexpr (lc_container<container_type_a>) it_a.increment(vals);
                    else ++it_a;
                }
                return *this;
            }

            constexpr iterator operator--(int) { iterator _it = *this; operator--(); return _it; }
            constexpr iterator& operator--() {
                if (--it_b == cart->b.begin()) {
                    it_b = cart->b.end();
                    --it_a;
                }
                return *this;
            }

            constexpr iterator& decrement(auto& vals) {
                if constexpr (lc_container<container_type_b>) it_b.decrement(vals);
                else --it_b;

                if (it_b == cart->b.end()) {
                    if constexpr (lc_container<container_type_b>) cart->b(vals);
                    it_b = cart->b.begin();
                    if constexpr (lc_container<container_type_a>) it_a.decrement(vals);
                    else --it_a;
                }
                return *this;
            }

            constexpr bool operator==(const iterator& other) const { return other.it_a == it_a && other.it_b == it_b; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr value_type operator*() const { return std::tuple_cat(std::tuple{ *it_a }, std::tuple{ *it_b }); }

        private:
            iterator_a it_a;
            iterator_b it_b;
            const cart_t* cart = nullptr;
        };

        using const_iterator = iterator;

        constexpr size_type size() const { return a.size() * b.size(); }
        constexpr iterator begin() const { return iterator{ *this, a.begin(), b.begin() }; }
        constexpr iterator end() const { return iterator{ *this, a.end(), b.begin() }; }
        constexpr value_type operator[](size_type index) const {
            return std::tuple_cat(std::tuple{ a[index / b.size()] }, std::tuple{ b[index % b.size()] });
        }

    private:
        stored_type_t<A> a;
        stored_type_t<B> b;
    };

    template<class A, class B> cart_t(A&&, B&&)->cart_t<A&&, B&&>;
    template<class A, class B> cart_t(A&, B&&)->cart_t<A&, B&&>;
    template<class A, class B> cart_t(A&&, B&)->cart_t<A&&, B&>;
    template<class A, class B> cart_t(A&, B&)->cart_t<A&, B&>;

    // Named container, holds a container, but has names for all indices 
    // in the tuple that the container holds.
    template<container_type Container, specialization<std::tuple> Names>
    class named_container {
    public:
        using container_type = std::decay_t<Container>;
        using value_type = value_type_t<container_type>;
        using size_type = std::size_t;
        using container_iterator = iterator_type_t<container_type>;
        using container_const_iterator = iterator_type_t<container_type>;

        using type_tag = lc_container_tag;
        using dependencies = typename get_dependencies<container_type>::type;
        using definitions = Names;
        using names = Names;
        template<class> using types = as_tuple_t<value_type>;

        using names = Names;

        constexpr named_container(Container container)
            : container(std::forward<Container>(container)) {}

        struct iterator {
            constexpr iterator() {}
            constexpr iterator(container_const_iterator it) : it(it) {}
            constexpr iterator(const iterator& other) : it(other.it) {}
            constexpr iterator& operator=(const iterator& other) { it = other.it; return *this; }

            constexpr iterator& operator++() { ++it; return *this; }
            constexpr iterator& operator--() { --it; return *this; }
            constexpr decltype(auto) operator*() { return *it; }
            constexpr bool operator==(const iterator& other) const { return other.it == it; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }

            template<class Ty>
            constexpr iterator& increment(Ty& vals) {
                if constexpr (lc_container<container_type>) it.increment(vals);
                else ++it;
                return *this;
            }

            template<class Ty>
            constexpr iterator& decrement(Ty& vals) {
                if constexpr (lc_container<container_type>) it.decrement(vals);
                else --it;
                return *this;
            }
        private:
            container_const_iterator it;
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return { container.begin() }; }
        constexpr iterator end() const { return { container.end() }; }
        constexpr size_type size() const { return container.size(); }
        constexpr decltype(auto) operator[](size_type index) const { return container[index]; }

    private:
        stored_type_t<Container> container;
    };

    // List comprehension base, only has data, is used when still constructing final object
    // Expression is what'll be outputted, Container determines the values going into the Expression
    // Constraints is a tuple of Expressions that filter the final output.
    template<
        specialization<expression> Expression,
        specialization<std::tuple> Content,
        specialization<tuple_with_names> Lazy = tuple_with_names<std::tuple<>, std::tuple<>>>
    class list_comprehension_base {
    public:
        constexpr list_comprehension_base(Expression&& expr, Content&& content)
            : expr(expr), content(content) {}

        constexpr list_comprehension_base(list_comprehension_base&& move)
            : expr(std::move(move.expr)), content(std::move(move.content)),
            lazyData(std::move(move.lazyData)) {}

        constexpr list_comprehension_base(const list_comprehension_base& move)
            : expr(move.expr), content(move.content), lazyData(move.lazyData) {}
        
        constexpr list_comprehension_base(const auto& move, auto& lazy)
            : expr(move.expr), content(move.content), lazyData(lazy) {}

        Expression expr;
        Content content;
        Lazy lazyData;

        using lazy_type = std::decay_t<Lazy>;
        using expression_type = std::decay_t<Expression>;
        using content_type = std::decay_t<Content>;

        using needed_names = tuple_cat_t<typename get_dependencies<Content>::type, typename expression_type::dependencies>;
        using names_we_have = tuple_cat_t<typename get_definitions<Content>::type, typename Lazy::names>;
        using reduced_needed_names = typename types_not_in<names_we_have, needed_names>::type;
    };

    template<
        specialization<expression> Expression,
        specialization<std::tuple> Content>
        list_comprehension_base(Expression&&, Content&&)
        ->list_comprehension_base<Expression, Content>;

    // Final list comprehension object
    template<specialization<expression> Expression,
        specialization<std::tuple> Content,
        specialization<tuple_with_names> Lazy = tuple_with_names<std::tuple<>, std::tuple<>>>
    class list_comprehension {
    public:
        using base_type = list_comprehension_base<Expression, Content, Lazy>;

        // Extract all components
        //using constraints = typename filter_on_tag<lc_constraint_tag, Content>::type;
        using containers = typename filter_on_tag<lc_container_tag, Content>::type;
        //using breaking_conditions = typename filter_on_tag<lc_break_tag, Content>::type;
        //using var_aliases = typename filter_on_tag<lc_alias_tag, Content>::type;

        template<std::size_t I>
        struct named_tuple_type_i {
            using prev = typename named_tuple_type_i<I - 1>::type;
            using curr = std::tuple_element_t<I - 1, Content>;
            using names = typename curr::names;
            using types = typename curr::template types<prev>;

            using type = tuple_with_names<tuple_cat_t<typename prev::names, names>, tuple_cat_t<typename prev::type, types>>;
        };

        template<> struct named_tuple_type_i<0> { using type = Lazy; };
        using named_tuple_type = typename named_tuple_type_i<std::tuple_size_v<Content>>::type;

        using value_type = decltype(std::declval<Expression>()(std::declval<named_tuple_type&>()));
        using size_type = std::size_t;

        template<class> struct get_iterators;
        template<class ...Tys> struct get_iterators<std::tuple<Tys...>> { using type = std::tuple<typename Tys::first_type::const_iterator...>; };
        template<class Ty> using get_iterators_t = typename get_iterators<Ty>::type;

        using iterators = get_iterators_t<containers>;

        constexpr list_comprehension(base_type&& base) : data(std::move(base)) {}
        constexpr list_comprehension(list_comprehension&& move) : data(std::move(move.data)) {}
        constexpr list_comprehension(const list_comprehension& move) : data(move.data) {}

        class iterator {
        public:
            using iterator_category = std::input_iterator_tag;
            using value_type = list_comprehension::value_type;
            using reference = value_type;
            using difference_type = std::ptrdiff_t;
            using size_type = list_comprehension::size_type;

            constexpr iterator() {};
            constexpr iterator(const iterator& other) : me(other.me), its(other.its), index(other.index) { ; }
            constexpr iterator(const list_comprehension* me, bool as_begin) : me(me), its() { prepare(as_begin); }
            constexpr iterator& operator=(const iterator& other) { me = other.me; its = other.its; index = other.index; return *this; }

            constexpr iterator operator++(int) { iterator _it = *this; operator++(); return _it; }
            constexpr iterator& operator++() { 
                increment_impl(std::make_index_sequence<std::tuple_size_v<Content>>{});
                return *this; 
            }

            constexpr iterator& increment() {
                increment_impl(std::make_index_sequence<std::tuple_size_v<Content>>{});
                return *this;
            }

            constexpr iterator operator--(int) { iterator _it = *this; operator--(); return _it; }
            constexpr iterator& operator--() { return *this; }

            constexpr bool operator==(const iterator& other) const { return other.its == its && other.index == index; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr value_type operator*() {
                if (std::is_constant_evaluated() && index == 0) throw; // To prevent access after end at compiletime
                return me->data.expr(values);
            }

        private:
            named_tuple_type values;
            iterators its;
            std::size_t index = 0;
            const list_comprehension* me = nullptr;

            enum Code {
                Normal = 0x0, Break = 0x1, Again = 0x2
            };

            template<std::size_t ...Is>
            constexpr void increment_impl(std::index_sequence<Is...>) {
                int res = 0;
                do res = increment_impl1(std::make_index_sequence<std::tuple_size_v<containers>>{})
                    | (increment_impl_part2<Is>(std::get<Is>(me->data.content)) | ...);
                while (!(res & Code::Break) && (res & Code::Again));
            }

            template<std::size_t ...Is>
            constexpr int increment_impl1(std::index_sequence<Is...>) {
                return (increment_impl_part1<std::tuple_size_v<containers> - Is - 1>(
                    std::get<std::tuple_element_t<std::tuple_size_v<containers> - Is - 1, containers>::second_type::value>(me->data.content)) | ...);
            }

            template<std::size_t I, class Ty>
            constexpr Code increment_impl_part1(const Ty& val) { return Code::Normal; }

            template<std::size_t I, lc_container Ty>
            constexpr Code increment_impl_part1(const Ty& val) {
                using type = std::tuple_element_t<I, containers>::first_type;
                if (index == I + 1) {
                    // If increment gives end, assign begin and add to index
                    auto& it = std::get<I>(its);
                    it.increment(values);
                    if (it == val.end()) {
                        it = val.begin();
                        --index;

                        // If at end, reset and break
                        if (index == 0)
                            return Code::Break;
                    } // If not end, set index to 0
                    else index = std::tuple_size_v<containers>;

                    // Finally assign new value to values
                    values.assign(tuple_with_names<typename type::names, typename type::template types<void>>{ *it });
                }

                return Code::Normal;
            }            

            template<std::size_t I, lc_container Ty>
            constexpr Code increment_impl_part2(const Ty& val) { return Code::Normal; }

            template<std::size_t I, lc_constraint Ty>
            constexpr Code increment_impl_part2(const Ty& val) {
                using type = std::tuple_element_t<I, Content>;
                if (!val(values)) return Code::Again;
                return Code::Normal;
            }

            template<std::size_t I, lc_alias Ty>
            constexpr Code increment_impl_part2(const Ty& val) {
                using type = std::tuple_element_t<I, Content>;
                values.set<type::var::name>(val.expr(values));
                return Code::Normal;
            }

            template<std::size_t I, lc_break Ty>
            constexpr Code increment_impl_part2(const Ty& val) {
                using type = std::tuple_element_t<I, Content>;
                if (val(values)) { // If break passes, reset iterators and set index to end
                    initialize(std::make_index_sequence<std::tuple_size_v<containers>>{});
                    index = 0;
                    return Code::Break;
                }
                return Code::Normal;
            }

            template<std::size_t ...Is>
            constexpr void initialize(std::index_sequence<Is...>) {
                ((std::get<Is>(its) = std::get<std::tuple_element_t<Is, containers>::second_type::value>(me->data.content).begin()
                    ), ...);
            }

            template<std::size_t ...Is>
            constexpr void set_values(std::index_sequence<Is...>) {
                ((values.assign(tuple_with_names<typename std::tuple_element_t<Is, containers>::first_type::names, 
                    typename std::tuple_element_t<Is, containers>::first_type::template types<void>>{ *std::get<Is>(its) })), ...);
            }

            template<std::size_t ...Is>
            constexpr int initial_check(std::index_sequence<Is...>) {
                return (increment_impl_part2<Is>(std::get<Is>(me->data.content)) | ...);
            }

            constexpr void prepare(bool as_begin) {
                values.assign(me->data.lazyData);
                
                if (!as_begin) index = 0;
                else index = std::tuple_size_v<containers>;

                initialize(std::make_index_sequence<std::tuple_size_v<containers>>{});
                set_values(std::make_index_sequence<std::tuple_size_v<containers>>{});
                int code = initial_check(std::make_index_sequence<std::tuple_size_v<Content>>{});
                if (code & Break) return;
                if (code & Again) operator++();
            }
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return iterator{ this, true }; }
        constexpr iterator end() const { return iterator{ this, false }; }
        constexpr value_type operator[](size_type index) const {
            auto _it = begin();
            while (index--) ++_it;
            return *_it;
        }

    //private:
        base_type data;

        friend class iterator;
    };

    inline namespace lc_operators {

        // 'container, container'; For parallel iteration, creates a new zipped container.
        template<container_type A, container_type B> requires (!is_named_container<A> && !is_named_container<B>)
            constexpr zip_t<A&&, B&&> operator,(A&& a, B&& b) {
            return zip_t<A&&, B&&>{ std::forward<A>(a), std::forward<B>(b) };
        }

        // 'var, var'; Converts 2 vars to tuple of vars
        template<is_var_type A, is_var_type B> constexpr auto operator,(A&, B&) { return tuple_of_vars<A, B>{}; }
        template<is_var_type A, is_var_type ...Bs> constexpr auto operator,(tuple_of_vars<Bs...>&&, A&) { return tuple_of_vars<Bs..., A>{}; }
        template<is_var_type A, is_var_type ...Bs> constexpr auto operator,(A&, tuple_of_vars<Bs...>&&) { return tuple_of_vars<A, Bs...>{}; }

        // ===============================================================

        // - Container, necessary sugar for 'x <- container' syntax, just forwards container
        template<container_type Container>
        constexpr Container&& operator-(Container&& c) { return std::forward<Container>(c); }

        // Convert container into a named container
        template<is_var_type Var, container_type Container>
        constexpr auto operator<(Var&, Container&& c) {
            // If the container has a tuple as its value type, we need to wrap it into 
            // another tuple to prevent cart_t from concatenating the tuples.
            if constexpr (specialization<value_type_t<Container>, std::tuple>) {
                const auto mapped = map_t{ [] <typename Ty>(Ty && val) {
                    return std::tuple<Ty>{ std::forward<Ty>(val) };
                }, std::forward<Container>(c) };
                return named_container<decltype(mapped), std::tuple<std::decay_t<Var>>>{ mapped };
            }
            else // Otherwise it's a normal container that can simply be named
                return named_container<Container&&, std::tuple<std::decay_t<Var>>>{ std::forward<Container>(c) };
        }

        // Splits the tuple into several vars, value_type of Container must 
        // be std::tuple, and size must match size of tuple_of_vars
        template<container_type Container, is_var_type ...Vars>
        constexpr auto operator<(tuple_of_vars<Vars...>&&, Container&& c) {
            static_assert(std::tuple_size_v<value_type_t<std::decay_t<Container>>> == sizeof...(Vars),
                "tuple_of_vars is not the same size as tuple in container");
            return named_container<Container&&, std::tuple<std::decay_t<Vars>...>>{ std::forward<Container>(c) };
        }

        // ===============================================================

        // Expr | Container
        template<specialization<expression> Expression, valid_lc_arg Container>
        constexpr auto operator|(Expression&& expr, Container&& cont) {
            return list_comprehension_base{
                std::forward<Expression>(expr),
                std::tuple{ cont }
            };
        }

        // Var | Container
        template<is_var_type Var, valid_lc_arg Container>
        constexpr auto operator|(Var&, Container&& cont) {
            return list_comprehension_base{
                expression{ []<has_tags<Var> T>(T& val) { return val.get<Var::name>(); }, std::type_identity<std::tuple<Var>>{} },
                std::tuple{ cont }
            };
        }

        // Vars | Container
        template<valid_lc_arg Container, is_var_type ...Vars>
        constexpr auto operator|(tuple_of_vars<Vars...>&&, Container&& cont) {
            return list_comprehension_base{
                expression{ []<has_tags<Vars...> T>(T& val) { return std::tuple{ val.get<Vars::name>()... }; },
                    std::type_identity<std::tuple<Vars...>>{} },
                std::tuple{ cont }
            };
        }

        // ===============================================================

        // 'list comprehension base, named container'; Creates new named container of cartesian product with combined names
        template<specialization<list_comprehension_base> A, valid_lc_arg B>
        constexpr auto operator,(A&& a, B&& b) {
            return list_comprehension_base{
                std::move(a.expr),
                std::tuple_cat(a.content, std::tuple{ std::forward<B>(b) })
            };
        }

        //// 'list comprehension base, var alias'; Adds a var alias.
        //template<specialization<list_comprehension_base> A, specialization<var_alias> B>
        //constexpr auto operator,(A&& a, B&& b) {
        //    return list_comprehension_base{
        //        std::move(a.expr),
        //        std::move(a.container),
        //        std::tuple_cat(a.aliases, std::tuple{ std::forward<B>(b) }),
        //        std::move(a.constraints),
        //        std::move(a.breaks)
        //    };
        //}

        //// 'list comprehension base, expression'; Adds a constraint.
        //template<specialization<list_comprehension_base> A, specialization<expression> B>
        //constexpr auto operator,(A&& a, B&& b) {
        //    return list_comprehension_base{
        //        std::move(a.expr),
        //        std::move(a.container),
        //        std::move(a.aliases),
        //        std::tuple_cat(a.constraints, std::tuple{ std::forward<B>(b) }),
        //        std::move(a.breaks)
        //    };
        //}

        //// 'list comprehension base, break condition'; Adds a break condition.
        //template<specialization<list_comprehension_base> A, specialization<break_condition> B>
        //constexpr auto operator,(A&& a, B&& b) {
        //    return list_comprehension_base{
        //        std::move(a.expr),
        //        std::move(a.container),
        //        std::move(a.aliases),
        //        std::move(a.constraints),
        //        std::tuple_cat(a.breaks, std::tuple{ std::forward<B>(b) })
        //    };
        //}

        // ===============================================================

        // 'expression, var'; Same as above, but with 1 expression
        template<is_var_type B>
        constexpr auto operator,(specialization<expression> auto&& expr1, B&) {
            return expression{ [expr1 = std::move(expr1)]<has_tags<B> T>(T& vals) {
                return std::tuple_cat(
                    std::tuple{ expr1(vals) },
                    std::tuple<std::decay_t<decltype(vals.get<B::name>())>>{ vals.get<B::name>() }
                );
            }, std::type_identity<tuple_cat_t<std::tuple<B>, typename std::decay_t<decltype(expr1)>::dependencies>>{}};
        }

        // 'var, expression'; Same as above
        template<is_var_type A>
        constexpr auto operator,(A&, specialization<expression> auto&& expr2) {
            return expression{ [expr2 = std::move(expr2)]<has_tags<A> T>(T& vals) {
                return std::tuple_cat(
                    std::tuple<std::decay_t<decltype(vals.get<A::name>())>>{ vals.get<A::name>() },
                    std::tuple{expr2(vals)}
                );
            }, std::type_identity<tuple_cat_t<std::tuple<A>, typename std::decay_t<decltype(expr2)>::dependencies>>{} };
        }

        // 'expression, vars'; Same as above
        template<is_var_type ...Bs>
        constexpr auto operator,(specialization<expression> auto&& expr1, tuple_of_vars<Bs...>&&) {
            return expression{ [expr1 = std::move(expr1)]<has_tags<Bs...> T> (T& vals) {
                return std::tuple_cat(
                    std::tuple{ expr1(vals) },
                    std::tuple<std::decay_t<decltype(vals.get<Bs::name>())>>{ vals.get<Bs::name>() }...
                );
            }, std::type_identity<tuple_cat_t<std::tuple<Bs...>, typename std::decay_t<decltype(expr1)>::dependencies>>{} };
        }

        // 'vars, expression'; Same as above
        template<is_var_type ...As>
        constexpr auto operator,(tuple_of_vars<As...>&&, specialization<expression> auto&& expr2) {
            return expression{ [expr2 = std::move(expr2)]<has_tags<As...> T>(T& vals) {
                return std::tuple_cat(
                    std::tuple<std::decay_t<decltype(vals.get<As::name>())>>{ vals.get<As::name>() }...,
                    std::tuple{expr2(vals)}
                );
            }, std::type_identity<tuple_cat_t<std::tuple<As...>, typename std::decay_t<decltype(expr2)>::dependencies>>{} };
        }

        // 'expression, expression'; Same as above, but both arguments are expressions
        constexpr auto operator,(specialization<expression> auto&& expr1, specialization<expression> auto&& expr2) {
            return expression{ [expr1 = std::move(expr1), expr2 = std::move(expr2)] (auto& vals) {
                return std::tuple_cat(
                    std::tuple{ expr1(vals) },
                    std::tuple{ expr2(vals) }
                );
            }, std::type_identity<tuple_cat_t<typename std::decay_t<decltype(expr1)>::dependencies, 
                typename std::decay_t<decltype(expr2)>::dependencies>>{} };
        }

        // ===============================================================

        struct brk_t {};
        constexpr brk_t brk;

        constexpr auto operator<<=(const brk_t&, specialization<expression> auto&& expr1) {
            return break_condition{ std::forward<decltype(expr1)>(expr1),
                std::type_identity<typename std::decay_t<decltype(expr1)>::dependencies>{} };
        }

        template<is_var_type A>
        constexpr auto operator<<=(const brk_t&, A&) {
            return break_condition{ []<has_tags<A> T>(T& vals) { return vals.get<A::name>(); },
                std::type_identity<std::tuple<A>>{} };
        }

        template<is_var_type A>
        constexpr auto operator<<=(const A&, specialization<expression> auto&& expr1) {
            return var_alias<A, decltype(expr1)>{ std::forward<decltype(expr1)>(expr1) };
        }

        // ===============================================================

        // Only valid argument for operators if not variable, expression, or container
        template<class Ty>
        concept valid_op_type = (!is_var_type<Ty> && !container_type<Ty> && !specialization<Ty, expression>);

        // macro for defining binary operator
#define create_op(op)                                                                                                           \
        template<is_var_type A, is_var_type B>                                                                                  \
        constexpr auto operator op(const A&, const B&) {                                                                        \
            return expression{ []<has_tags<A, B> T>(T& vals)                                                                    \
                { return vals.get<A::name>() op vals.get<B::name>(); },                                                         \
                std::type_identity<std::tuple<A, B>>{} };                                                                       \
        }                                                                                                                       \
                                                                                                                                \
        template<is_var_type B>                                                                                                 \
        constexpr auto operator op(specialization<expression> auto&& expr1, const B&) {                                         \
            return expression{ [expr1 = std::forward<decltype(expr1)>(expr1)]<has_tags<B> T>(T& vals)                           \
                { return expr1(vals) op vals.get<B::name>(); },                                                                 \
                std::type_identity<tuple_cat_t<std::tuple<B>, typename std::decay_t<decltype(expr1)>::dependencies>>{} };       \
        }                                                                                                                       \
                                                                                                                                \
        template<is_var_type A>                                                                                                 \
        constexpr auto operator op(const A&, specialization<expression> auto&& expr2) {                                         \
            return expression{ [expr2 = std::forward<decltype(expr2)>(expr2)]<has_tags<A> T>(T& vals)                           \
                { return vals.get<A::name>() op expr2(vals); },                                                                 \
                std::type_identity<tuple_cat_t<std::tuple<A>, typename std::decay_t<decltype(expr2)>::dependencies>>{} };       \
        }                                                                                                                       \
                                                                                                                                \
        constexpr auto operator op(specialization<expression> auto&& expr1, specialization<expression> auto&& expr2) {          \
            return expression{ [expr1 = std::forward<decltype(expr1)>(expr1),                                                   \
                expr2 = std::forward<decltype(expr2)>(expr2)] (auto& vals)                                                      \
                    { return expr1(vals) op expr2(vals); },                                                                     \
                std::type_identity<tuple_cat_t<typename std::decay_t<decltype(expr1)>::dependencies,                            \
                typename std::decay_t<decltype(expr2)>::dependencies>>{} };                                                     \
        }                                                                                                                       \
                                                                                                                                \
        template<is_var_type A, valid_op_type B>                                                                                \
        constexpr auto operator op(const A&, B& b) {                                                                            \
            return expression{ [&]<has_tags<A> T>(T& vals)                                                                      \
                { return vals.get<A::name>() op b; },                                                                           \
                std::type_identity<std::tuple<A>>{} };                                                                          \
        }                                                                                                                       \
                                                                                                                                \
        template<valid_op_type B>                                                                                               \
        constexpr auto operator op(specialization<expression> auto&& expr1, B& b) {                                             \
            return expression{ [&, expr1 = std::forward<decltype(expr1)>(expr1)] (auto& vals)                                   \
                { return expr1(vals) op b; },                                                                                   \
                std::type_identity<typename std::decay_t<decltype(expr1)>::dependencies>{} };                                   \
        }                                                                                                                       \
                                                                                                                                \
        template<is_var_type A, valid_op_type B>                                                                                \
        constexpr auto operator op(B& b, const A&) {                                                                            \
            return expression{ [&]<has_tags<A> T>(T& vals) { return b op vals.get<A::name>(); },                                \
                std::type_identity<std::tuple<A>>{} };                                                                          \
        }                                                                                                                       \
                                                                                                                                \
        template<valid_op_type B>                                                                                               \
        constexpr auto operator op(B& b, specialization<expression> auto&& expr1) {                                             \
            return expression{ [&, expr1 = std::forward<decltype(expr1)>(expr1)] (auto& vals)                                   \
                { return b op expr1(vals); },                                                                                   \
                std::type_identity<typename std::decay_t<decltype(expr1)>::dependencies>{} };                                   \
        }                                                                                                                       \
                                                                                                                                \
        template<is_var_type A, valid_op_type B>                                                                                \
        constexpr auto operator op(const A&, B&& b) {                                                                           \
            return expression{ [b = std::move(b)]<has_tags<A> T>(T& vals)                                                       \
            { return vals.get<A::name>() op b; },                                                                               \
                std::type_identity<std::tuple<A>>{} };                                                                          \
        }                                                                                                                       \
                                                                                                                                \
        template<valid_op_type B>                                                                                               \
        constexpr auto operator op(specialization<expression> auto&& expr1, B&& b) {                                            \
            return expression{ [b = std::move(b), expr1 = std::forward<decltype(expr1)>(expr1)] (auto& vals)                    \
                { return expr1(vals) op b; },                                                                                   \
                std::type_identity<typename std::decay_t<decltype(expr1)>::dependencies>{} };                                   \
        }                                                                                                                       \
                                                                                                                                \
        template<is_var_type A, valid_op_type B>                                                                                \
        constexpr auto operator op(B&& b, const A&) {                                                                           \
            return expression{ [b = std::move(b)]<has_tags<A> T>(T& vals)                                                       \
                { return b op vals.get<A::name>(); },                                                                           \
                std::type_identity<std::tuple<A>>{} };                                                                          \
        }                                                                                                                       \
                                                                                                                                \
        template<valid_op_type B>                                                                                               \
        constexpr auto operator op(B&& b, specialization<expression> auto&& expr1) {                                            \
            return expression{ [b = std::move(b), expr1 = std::forward<decltype(expr1)>(expr1)] (auto& vals)                    \
                { return b op expr1(vals); },                                                                                   \
                std::type_identity<typename std::decay_t<decltype(expr1)>::dependencies>{} };                                   \
        }                                                                                                                                  

        create_op(+) create_op(-) create_op(*) create_op(/ ) create_op(== ) create_op(!= ) create_op(<= );
        create_op(>= ) create_op(> ) create_op(< ) create_op(%) create_op(<=> ) create_op(<< ) create_op(>> );
        create_op(&) create_op(| ) create_op(&&) create_op(|| );

        // Macro for defining unary operator
#define create_uop(op)                                                                                                       \
        template<is_var_type A>                                                                                              \
        constexpr auto operator op(const A&) {                                                                               \
            return expression{ []<has_tags<A> T>(T& vals) { return op vals.get<A::name>(); },                                \
                std::type_identity<std::tuple<A>>{} };                                                                       \
        }                                                                                                                    \
                                                                                                                             \
        constexpr auto operator op(specialization<expression> auto&& expr1) {                                                \
            return expression{ [expr1 = std::move(expr1)](auto& vals) { return op expr1(vals); },                            \
                std::type_identity<decltype(expr1)::dependencies>{} };                                                       \
        }

        create_uop(-) create_uop(~) create_uop(!) create_uop(*) create_uop(&);

#undef create_op
#undef create_uop
    }

    // ===============================================================

    // Construct the final list comprehension using the operator[] in 
    // this global constexpr object.
    struct lc_op {
        template<class A, class B, class F>
        constexpr auto operator[](list_comprehension_base<A, B, F>&& val) const {
            if constexpr (std::tuple_size_v<typename list_comprehension_base<A, B, F>::reduced_needed_names> == 0)
                return list_comprehension<A, B, F>{ std::move(val) };
            else
                return expression{ [this, val = std::move(val)]<class Ty> (Ty& vals) {
                    return operator[](list_comprehension_base<A, B, Ty>{ val, vals });
                }, std::type_identity<typename list_comprehension_base<A, B, F>::reduced_needed_names>{} };
        }
    };
    constexpr lc_op lc;

    // ===============================================================

    // Infinite type, used by range
    struct inf_t {};
    constexpr inf_t inf;

    // Simple range class
    struct dud {};
    template<class Ty, class B = dud>
    class range {
    public:
        using value_type = Ty;
        using size_type = std::size_t;

        using type_tag = lc_container_tag;
        using dependencies = std::conditional_t<std::same_as<B, dud>, std::tuple<>, std::tuple<B>>;
        using definitions = std::tuple<>;
        using names = std::tuple<>;
        template<class> using types = as_tuple_t<Ty>;

        constexpr range(const Ty& a) : m_Start(), m_End(a) {}
        constexpr range(const Ty& a, const Ty& b) : m_Start(a), m_End(b) {}
        constexpr range(const Ty& a, inf_t) : m_Start(a), m_End(std::numeric_limits<Ty>::max()) {}
        constexpr range(const Ty& a, B&) requires(is_var_type<B>) : m_Start(a), m_End(std::numeric_limits<Ty>::max() - 1) {}

        class iterator {
        public:
            using value_type = range::value_type;
            using size_type = range::size_type;

            constexpr iterator() : val() {}
            constexpr iterator(const iterator& val) : val(val.val), me(val.me) {}
            constexpr iterator(const range* me, Ty val) : val(val), me(me) {}
            constexpr iterator& operator=(const iterator& other) { val = other.val; me = other.me; return *this; }
            constexpr iterator& operator++() { ++val; return *this; }
            constexpr iterator& operator--() { --val; return *this; }
            constexpr bool operator==(const iterator& other) const { return other.val == val; }
            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr Ty operator*() const { return val; }

            template<class Ty>
            constexpr iterator& increment(Ty& vals) {
                operator++();
                //if constexpr (has_type_v<B, typename Ty::names>) {
                //    if (val >= vals.get<B::name>())
                //        val = me->m_End + 1;
                //}
                return *this;
            }

            template<class Ty>
            constexpr iterator& decrement(Ty& vals) {
                operator--();
                return *this;
            }

        private:
            Ty val;
            const range* me = nullptr;
        };

        using const_iterator = iterator;

        constexpr iterator begin() const { return iterator{ this, m_Start }; }
        constexpr iterator end() const { return iterator{ this, m_End + 1 }; }
        constexpr size_type size() const { return m_End - m_Start; }
        constexpr Ty operator[](size_type index) const { return m_Start + static_cast<Ty>(index); }

    private:
        Ty m_Start;
        Ty m_End;

        friend class iterator;
    };

    //template<specialization<expression> Type>
    //class expression_container {
    //public:
    //    constexpr expression_container(Type&& expr) : expr(expr) {}

    //    using value_type = void;
    //    using size_type = std::size_t;

    //    using type_tag = lc_container_tag;
    //    using dependencies = typename Type::dependencies;
    //    using definitions = std::tuple<>;
    //    using names = std::tuple<>;
    //    template<class Ty> using types = as_tuple_t<decltype(std::declval<Type>()(std::declval<Ty&>()))>;

    //    class iterator {
    //    public:
    //        using value_type = range::value_type;
    //        using size_type = range::size_type;

    //        constexpr iterator() : val() {}
    //        constexpr iterator(const iterator& val) : val(val.val), me(val.me) {}
    //        constexpr iterator(const range* me, Ty val) : val(val), me(me) {}
    //        constexpr iterator& operator=(const iterator& other) { val = other.val; me = other.me; return *this; }
    //        constexpr iterator& operator++() { ++val; return *this; }
    //        constexpr iterator& operator--() { --val; return *this; }
    //        constexpr bool operator==(const iterator& other) const { return other.val == val; }
    //        constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
    //        constexpr Ty operator*() const { return val; }

    //        template<class Ty>
    //        constexpr iterator& increment(Ty& vals) {
    //            operator++();
    //            //if constexpr (has_type_v<B, typename Ty::names>) {
    //            //    if (val >= vals.get<B::name>())
    //            //        val = me->m_End + 1;
    //            //}
    //            return *this;
    //        }

    //        template<class Ty>
    //        constexpr iterator& decrement(Ty& vals) {
    //            operator--();
    //            return *this;
    //        }

    //    private:
    //        Ty val;
    //        const range* me = nullptr;
    //    };

    //private:
    //    Type expr;
    //};

    // ===============================================================

    namespace lc_functions {
        template<class Ty>
        concept valid_arg = lc_operators::valid_op_type<Ty> || is_var_type<Ty> || specialization<Ty, expression>;

        template<valid_arg Ty>
        constexpr decltype(auto) get_arg_value(const Ty& val, auto& vals) {
            if constexpr (is_var_type<Ty>) return vals.get<Ty::name>();
            else if constexpr (specialization<Ty, expression>) return val(vals);
            else if constexpr (lc_operators::valid_op_type<Ty>) return val;
        }
    }
}
// Define expression overloads for std functions 
#define lc_std_fun(y, x)                                                              \
    template<valid_arg ...Args> constexpr auto x(Args&&... args) {                    \
        return kaixo::expression{ [...args = std::forward<Args>(args)] (auto& vals) { \
            return :: y x(get_arg_value(args, vals)...);                              \
        } };                                                                          \
    }

#ifndef KAIXO_LC_FUNCTIONAL
#define KAIXO_LC_FUNCTIONAL 1
#endif
#if KAIXO_LC_FUNCTIONAL == 1
#include <functional>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, bind_front);
        lc_std_fun(std::, bind);
        lc_std_fun(std::, ref);
        lc_std_fun(std::, cref);
        lc_std_fun(std::, invoke);
    }
}
#endif

#ifndef KAIXO_LC_ANY
#define KAIXO_LC_ANY 1
#endif
#if KAIXO_LC_ANY == 1
#include <any>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, any_cast);
        lc_std_fun(std::, make_any);
    }
}
#endif

#ifndef KAIXO_LC_ALGORITHMS
#define KAIXO_LC_ALGORITHMS 1
#endif
#if KAIXO_LC_ALGORITHMS == 1
#include <algorithm>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, adjacent_find);
        lc_std_fun(std::, binary_search);
        lc_std_fun(std::, bsearch);
        lc_std_fun(std::, clamp);
        lc_std_fun(std::, copy_backward);
        lc_std_fun(std::, copy_n);
        lc_std_fun(std::, count);
        lc_std_fun(std::, count_if);
        lc_std_fun(std::, equal);
        lc_std_fun(std::, equal_range);
        lc_std_fun(std::, fill);
        lc_std_fun(std::, fill_n);
        lc_std_fun(std::, find);
        lc_std_fun(std::, find_end);
        lc_std_fun(std::, find_first_of);
        lc_std_fun(std::, find_if);
        lc_std_fun(std::, find_if_not);
        lc_std_fun(std::, for_each);
        lc_std_fun(std::, for_each_n);
        lc_std_fun(std::, generate);
        lc_std_fun(std::, generate_n);
        lc_std_fun(std::, includes);
        lc_std_fun(std::, inplace_merge);
        lc_std_fun(std::, iter_swap);
        lc_std_fun(std::, lexicographical_compare);
        lc_std_fun(std::, lower_bound);
        lc_std_fun(std::, make_heap);
        lc_std_fun(std::, max);
        lc_std_fun(std::, max_element);
        lc_std_fun(std::, merge);
        lc_std_fun(std::, min);
        lc_std_fun(std::, min_element);
        lc_std_fun(std::, minmax);
        lc_std_fun(std::, minmax_element);
        lc_std_fun(std::, mismatch);
        lc_std_fun(std::, move);
        lc_std_fun(std::, move_backward);
        lc_std_fun(std::, next_permutation);
        lc_std_fun(std::, nth_element);
        lc_std_fun(std::, partial_sort);
        lc_std_fun(std::, partial_sort_copy);
        lc_std_fun(std::, partition);
        lc_std_fun(std::, partition_copy);
        lc_std_fun(std::, partition_point);
        lc_std_fun(std::, pop_heap);
        lc_std_fun(std::, prev_permutation);
        lc_std_fun(std::, push_heap);
        lc_std_fun(std::, qsort);
        lc_std_fun(std::, remove);
        lc_std_fun(std::, remove_copy);
        lc_std_fun(std::, replace);
        lc_std_fun(std::, replace_copy);
        lc_std_fun(std::, replace_copy_if);
        lc_std_fun(std::, reverse);
        lc_std_fun(std::, reverse_copy);
        lc_std_fun(std::, rotate);
        lc_std_fun(std::, rotate_copy);
        lc_std_fun(std::, sample);
        lc_std_fun(std::, search);
        lc_std_fun(std::, search_n);
        lc_std_fun(std::, shift_left);
        lc_std_fun(std::, shift_right);
        lc_std_fun(std::, set_difference);
        lc_std_fun(std::, set_intersection);
        lc_std_fun(std::, set_symmetric_difference);
        lc_std_fun(std::, set_union);
        lc_std_fun(std::, sort);
        lc_std_fun(std::, sort_heap);
        lc_std_fun(std::, stable_partition);
        lc_std_fun(std::, stable_sort);
        lc_std_fun(std::, swap);
        lc_std_fun(std::, swap_ranges);
        lc_std_fun(std::, transform);
        lc_std_fun(std::, unique);
        lc_std_fun(std::, unique_copy);
        lc_std_fun(std::, upper_bound);
    }
}
#endif

#ifndef KAIXO_LC_ITERATOR
#define KAIXO_LC_ITERATOR 1
#endif
#if KAIXO_LC_ITERATOR == 1
#include <iterator>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, advance);
        lc_std_fun(std::, back_inserter);
        lc_std_fun(std::, begin);
        lc_std_fun(std::, data);
        lc_std_fun(std::, distance);
        lc_std_fun(std::, empty);
        lc_std_fun(std::, end);
        lc_std_fun(std::, front_inserter);
        lc_std_fun(std::, inserter);
        lc_std_fun(std::, make_move_iterator);
        lc_std_fun(std::, make_reverse_iterator);
        lc_std_fun(std::, next);
        lc_std_fun(std::, prev);
        lc_std_fun(std::, rbegin);
        lc_std_fun(std::, rend);
        lc_std_fun(std::, size);
    }
}
#endif

#ifndef KAIXO_LC_MEMORY
#define KAIXO_LC_MEMORY 1
#endif
#if KAIXO_LC_MEMORY == 1
#include <memory>
#include <memory_resource>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, addressof);
        lc_std_fun(std::, align);
        lc_std_fun(std::, assume_aligned);
        lc_std_fun(std::, calloc);
        lc_std_fun(std::, free);
        lc_std_fun(std::, malloc);
        lc_std_fun(std::, realloc);
        lc_std_fun(std::, destroy);
        lc_std_fun(std::, destroy_at);
        lc_std_fun(std::, destroy_n);
        lc_std_fun(std::pmr::, get_default_resource);
        lc_std_fun(std::, make_obj_using_allocator);
        lc_std_fun(std::pmr::, new_delete_resource);
        lc_std_fun(std::pmr::, null_memory_resource);
        lc_std_fun(std::pmr::, pool_options);
        lc_std_fun(std::pmr::, set_default_resource);
        lc_std_fun(std::, to_address);
        lc_std_fun(std::, uninitialized_construct_using_allocator);
        lc_std_fun(std::, uninitialized_copy);
        lc_std_fun(std::, uninitialized_copy_n);
        lc_std_fun(std::, uninitialized_default_construct);
        lc_std_fun(std::, uninitialized_default_construct_n);
        lc_std_fun(std::, uninitialized_fill);
        lc_std_fun(std::, uninitialized_fill_n);
        lc_std_fun(std::, uninitialized_move);
        lc_std_fun(std::, uninitialized_move_n);
        lc_std_fun(std::, uninitialized_value_construct);
        lc_std_fun(std::, uninitialized_value_construct_n);
    }
}
#endif

#ifndef KAIXO_LC_NUMERIC
#define KAIXO_LC_NUMERIC 1
#endif
#if KAIXO_LC_NUMERIC == 1
#include <numeric>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, accumulate);
        lc_std_fun(std::, adjacent_difference);
        lc_std_fun(std::, inclusive_scan);
        lc_std_fun(std::, inner_product);
        lc_std_fun(std::, iota);
        lc_std_fun(std::, reduce);
        lc_std_fun(std::, partial_sum);
        lc_std_fun(std::, transform_exclusive_scan);
        lc_std_fun(std::, transform_inclusive_scan);
        lc_std_fun(std::, transform_reduce);

        lc_std_fun(std::, bit_cast);
        lc_std_fun(std::, gcd);
        lc_std_fun(std::, lcm);
        lc_std_fun(std::, lerp);
        lc_std_fun(std::, abs);
        lc_std_fun(std::, acos);
        lc_std_fun(std::, acosh);
        lc_std_fun(std::, asin);
        lc_std_fun(std::, asinh);
        lc_std_fun(std::, atan);
        lc_std_fun(std::, atan2);
        lc_std_fun(std::, atanh);
        lc_std_fun(std::, cbrt);
        lc_std_fun(std::, ceil);
        lc_std_fun(std::, copysign);
        lc_std_fun(std::, cos);
        lc_std_fun(std::, cosh);
        lc_std_fun(std::, div);
        lc_std_fun(std::, erf);
        lc_std_fun(std::, erfc);
        lc_std_fun(std::, exp);
        lc_std_fun(std::, exp2);
        lc_std_fun(std::, expm1);
        lc_std_fun(std::, fabs);
        lc_std_fun(std::, fdim);
        lc_std_fun(std::, floor);
        lc_std_fun(std::, fma);
        lc_std_fun(std::, fmax);
        lc_std_fun(std::, fmin);
        lc_std_fun(std::, fmod);
        lc_std_fun(std::, fpclassify);
        lc_std_fun(std::, frexp);
        lc_std_fun(std::, hypot);
        lc_std_fun(std::, ilogb);
        lc_std_fun(std::, isfinite);
        lc_std_fun(std::, isgreater);
        lc_std_fun(std::, isgreaterequal);
        lc_std_fun(std::, isinf);
        lc_std_fun(std::, isless);
        lc_std_fun(std::, islessequal);
        lc_std_fun(std::, islessgreater);
        lc_std_fun(std::, isnan);
        lc_std_fun(std::, isnormal);
        lc_std_fun(std::, isunordered);
        lc_std_fun(std::, ldexp);
        lc_std_fun(std::, lgamma);
        lc_std_fun(std::, log);
        lc_std_fun(std::, log10);
        lc_std_fun(std::, log1p);
        lc_std_fun(std::, log2);
        lc_std_fun(std::, logb);
        lc_std_fun(std::, modf);
        lc_std_fun(std::, nan);
        lc_std_fun(std::, nearbyint);
        lc_std_fun(std::, nextafter);
        lc_std_fun(std::, pow);
        lc_std_fun(std::, remainder);
        lc_std_fun(std::, remquo);
        lc_std_fun(std::, rint);
        lc_std_fun(std::, round);
        lc_std_fun(std::, scalbn);
        lc_std_fun(std::, signbit);
        lc_std_fun(std::, sin);
        lc_std_fun(std::, sinh);
        lc_std_fun(std::, sqrt);
        lc_std_fun(std::, tan);
        lc_std_fun(std::, tanh);
        lc_std_fun(std::, tgamma);
        lc_std_fun(std::, trunc);
        lc_std_fun(std::, midpoint);
        lc_std_fun(std::, assoc_laguerre);
        lc_std_fun(std::, assoc_legendre);
        lc_std_fun(std::, beta);
        lc_std_fun(std::, comp_ellint_1);
        lc_std_fun(std::, comp_ellint_2);
        lc_std_fun(std::, comp_ellint_3);
        lc_std_fun(std::, cyl_bessel_i);
        lc_std_fun(std::, cyl_bessel_j);
        lc_std_fun(std::, cyl_bessel_k);
        lc_std_fun(std::, cyl_neumann);
        lc_std_fun(std::, ellint_1);
        lc_std_fun(std::, ellint_2);
        lc_std_fun(std::, ellint_3);
        lc_std_fun(std::, expint);
        lc_std_fun(std::, hermite);
        lc_std_fun(std::, laguerre);
        lc_std_fun(std::, legendre);
        lc_std_fun(std::, riemann_zeta);
        lc_std_fun(std::, sph_bessel);
        lc_std_fun(std::, sph_legendre);
        lc_std_fun(std::, sph_neumann);
    }
}
#endif

#ifndef KAIXO_LC_STRING
#define KAIXO_LC_STRING 1
#endif
#if KAIXO_LC_STRING == 1
#include <string>
#include <cstring>
#include <cwctype>
#include <cuchar>
namespace kaixo {
    namespace lc_functions {
        lc_std_fun(std::, atof);
        lc_std_fun(std::, atoi);
        lc_std_fun(std::, isalnum);
        lc_std_fun(std::, isalpha);
        lc_std_fun(std::, isblank);
        lc_std_fun(std::, iscntrl);
        lc_std_fun(std::, isdigit);
        lc_std_fun(std::, isgraph);
        lc_std_fun(std::, islower);
        lc_std_fun(std::, isprint);
        lc_std_fun(std::, ispunct);
        lc_std_fun(std::, isspace);
        lc_std_fun(std::, isupper);
        lc_std_fun(std::, isxdigit);
        lc_std_fun(std::, memchr);
        lc_std_fun(std::, memcmp);
        lc_std_fun(std::, memcpy);
        lc_std_fun(std::, memmove);
        lc_std_fun(std::, memset);
        lc_std_fun(std::, strcat);
        lc_std_fun(std::, strchr);
        lc_std_fun(std::, strcmp);
        lc_std_fun(std::, strcoll);
        lc_std_fun(std::, strcpy);
        lc_std_fun(std::, strcspn);
        lc_std_fun(std::, strerror);
        lc_std_fun(std::, strlen);
        lc_std_fun(std::, strncat);
        lc_std_fun(std::, strncmp);
        lc_std_fun(std::, strncpy);
        lc_std_fun(std::, strpbrk);
        lc_std_fun(std::, strrchr);
        lc_std_fun(std::, strspn);
        lc_std_fun(std::, strstr);
        lc_std_fun(std::, strtof);
        lc_std_fun(std::, strtok);
        lc_std_fun(std::, strtol);
        lc_std_fun(std::, strtoul);
        lc_std_fun(std::, strxfrm);
        lc_std_fun(std::, tolower);
        lc_std_fun(std::, toupper);
        lc_std_fun(std::, copy);
        lc_std_fun(std::, btowc);
        lc_std_fun(std::, c16rtomb);
        lc_std_fun(std::, c32rtomb);
        lc_std_fun(std::, mblen);
        lc_std_fun(std::, mbrlen);
        lc_std_fun(std::, mbrtoc16);
        lc_std_fun(std::, mbrtoc32);
        lc_std_fun(std::, mbrtowc);
        lc_std_fun(std::, mbsinit);
        lc_std_fun(std::, mbsrtowcs);
        lc_std_fun(std::, mbstowcs);
        lc_std_fun(std::, mbtowc);
        lc_std_fun(std::, wcrtomb);
        lc_std_fun(std::, wcsrtombs);
        lc_std_fun(std::, wcstombs);
        lc_std_fun(std::, wctob);
        lc_std_fun(std::, wctomb);
        lc_std_fun(std::, iswalnum);
        lc_std_fun(std::, iswalpha);
        lc_std_fun(std::, iswblank);
        lc_std_fun(std::, iswcntrl);
        lc_std_fun(std::, iswctype);
        lc_std_fun(std::, iswdigit);
        lc_std_fun(std::, iswgraph);
        lc_std_fun(std::, iswlower);
        lc_std_fun(std::, iswprint);
        lc_std_fun(std::, iswpunct);
        lc_std_fun(std::, iswspace);
        lc_std_fun(std::, iswupper);
        lc_std_fun(std::, iswxdigit);
        lc_std_fun(std::, towctrans);
        lc_std_fun(std::, towlower);
        lc_std_fun(std::, towupper);
        lc_std_fun(std::, wcscat);
        lc_std_fun(std::, wcschr);
        lc_std_fun(std::, wcscmp);
        lc_std_fun(std::, wcscoll);
        lc_std_fun(std::, wcscpy);
        lc_std_fun(std::, wcscspn);
        lc_std_fun(std::, wcslen);
        lc_std_fun(std::, wcsncat);
        lc_std_fun(std::, wcsncmp);
        lc_std_fun(std::, wcsncpy);
        lc_std_fun(std::, wcspbrk);
        lc_std_fun(std::, wcsrchr);
        lc_std_fun(std::, wcsspn);
        lc_std_fun(std::, wcsstr);
        lc_std_fun(std::, wcstof);
        lc_std_fun(std::, wcstok);
        lc_std_fun(std::, wcstol);
        lc_std_fun(std::, wcstoul);
        lc_std_fun(std::, wcsxfrm);
        lc_std_fun(std::, wctrans);
        lc_std_fun(std::, wctype);
        lc_std_fun(std::, wmemchr);
        lc_std_fun(std::, wmemcmp);
        lc_std_fun(std::, wmemcpy);
        lc_std_fun(std::, wmemmove);
        lc_std_fun(std::, wmemset);
    }
}
#endif

#undef lc_std_fun
#undef lc_mem_fun
