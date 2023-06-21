#pragma once
#include "kaixo/list_comprehension.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                         Range Container

                Simple arithmetic range with start
               and end value, supports partials and
                         infinite ranges

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {
    /**
     * Type for conveying infinity.
     */
    constexpr struct inf_t {} inf{};

    /**
     * Simple range object, allows for variable
     * dependent, and infinite ranges.
     */
    template<class ...As> struct range;

    template<arithmetic A> range(A, A) -> range<A>;
    template<arithmetic A> range(A, inf_t) -> range<A, inf_t>;
    template<is_partial A> range(A, inf_t) -> range<decay_t<A>, inf_t>;
    template<is_partial A, arithmetic B> range(A, B) -> range<decay_t<A>, B>;
    template<arithmetic A, is_partial B> range(A, B) -> range<A, decay_t<B>>;
    template<is_partial A, is_partial B> range(A, B) -> range<decay_t<A>, decay_t<B>>;

    /**
     * Default range between 2 numbers.
     */
    template<arithmetic Ty>
    struct range<Ty> {
        Ty a{};
        Ty b{};
        constexpr range(Ty a, Ty b) : a(a), b(b) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            Ty value{};

            constexpr iterator& operator++() { ++value; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; ++value; return b; }
            constexpr Ty operator*() { return value; }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a }; }
        constexpr iterator end() const { return { std::max(b + 1, a) }; }
    };

    /**
     * Range from value to infinity.
     */
    template<arithmetic Ty>
    struct range<Ty, inf_t> {
        Ty a{};

        constexpr range(Ty a, inf_t) : a(a) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            std::optional<Ty> value{};

            constexpr iterator& operator++() { ++value.value(); return *this; }
            constexpr iterator operator++(int) { iterator b = *this; ++value.value(); return b; }
            constexpr Ty operator*() { return value.value(); }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a }; }
        constexpr iterator end() const { return {}; }
    };

    /**
     * Range from dependent variable to value.
     */
    template<is_partial A, arithmetic Ty>
    struct range<A, Ty> {
        using is_range = int;
        using depend = depend<A>;

        A a{};
        Ty b{};

        template<class T>
        constexpr range(T&& a, Ty b) : a(std::forward<T>(a)), b(b) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ (Ty)kaixo::evaluate(a, tuple), b };
        }
    };

    /**
     * Range from dependent variable to infinity.
     */
    template<is_partial A>
    struct range<A, inf_t> {
        using is_range = int;
        using depend = depend<A>;

        A a;

        template<class T>
        constexpr range(T&& a, inf_t) : a(std::forward<T>(a)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ kaixo::evaluate(a, tuple), inf };
        }
    };

    /**
     * Range from value to dependent variable.
     */
    template<arithmetic Ty, is_partial B>
    struct range<Ty, B> {
        using is_range = int;
        using depend = depend<B>;

        Ty a{};
        B b{};

        template<class T>
        constexpr range(Ty a, T&& b) : a(a), b(std::forward<T>(b)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ a, (Ty)kaixo::evaluate(b, tuple) };
        }
    };

    /**
     * Range from dependent variable to dependent variable.
     */
    template<is_partial A, is_partial B>
    struct range<A, B> {
        using is_range = int;
        using depend = concat_t<depend<A>, depend<B>>;

        A a;
        B b;

        template<class T, class Q>
        constexpr range(T&& a, Q&& b) : a(std::forward<T>(a)), b(std::forward<Q>(b)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ kaixo::evaluate(a, tuple), kaixo::evaluate(b, tuple) };
        }
    };
}