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

    struct _with_increment {};

    /**
     * Simple range object, allows for variable
     * dependent, and infinite ranges.
     */
    template<class ...As> struct range;

    template<arithmetic A> 
    range(A, inf_t) -> range<A, inf_t>;

    template<arithmetic A, arithmetic B> 
    range(A, B) -> range<std::common_type_t<A, B>>;

    template<arithmetic A, arithmetic B, arithmetic C> 
    range(A, B, C) -> range<_with_increment, std::common_type_t<A, B, C>>;

    template<arithmetic A, arithmetic C> 
    range(A, inf_t, C) -> range<_with_increment, std::common_type_t<A, C>, inf_t>;

    template<class ...As> requires (is_partial<As> || ...)
    range(As...) -> range<decay_t<As>...>;

    /**
     * Default range between 2 numbers.
     */
    template<arithmetic Ty>
    struct range<Ty> {
        Ty a{};
        Ty b{};

        template<class A, class B>
        constexpr range(A a, B b)
            : a(static_cast<Ty>(a)),
              b(static_cast<Ty>(b)) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            Ty value{};

            constexpr iterator& operator++() { ++value; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; ++value; return b; }
            constexpr Ty operator*() const { return value; }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a }; }
        constexpr iterator end() const { return { std::max(b + 1, a) }; }
    };
    
    /**
     * Default range between 2 numbers with increment.
     */
    template<arithmetic Ty>
    struct range<_with_increment, Ty> {
        Ty a{};
        Ty b{};
        Ty c{};

        template<class A, class B, class C>
        constexpr range(A a, B b, C c) 
            : a(static_cast<Ty>(a)), 
              b(static_cast<Ty>(b)), 
              c(static_cast<Ty>(c)) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            Ty value{};
            Ty incr{};

            constexpr iterator& operator++() { value += incr; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; value += incr; return b; }
            constexpr Ty operator*() const { return value; }

            constexpr bool operator==(const iterator& b) const {
                if constexpr (floating_point<Ty>)
                    return b.value >= value - incr / 2 && b.value <= value + incr / 2;
                else return value == b.value; 
            }
        };

        constexpr iterator begin() const { return { a, c }; }
        constexpr iterator end() const { return { std::max(b + c, a), c }; }
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

            Ty value{};

            constexpr iterator& operator++() { ++value; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; ++value; return b; }
            constexpr Ty operator*() const { return value; }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a }; }
        constexpr iterator end() const { return { std::numeric_limits<Ty>::max() }; }
    };
    
    /**
     * Range from value to infinity with increment.
     */
    template<arithmetic Ty>
    struct range<_with_increment, Ty, inf_t> {
        Ty a{};
        Ty c{};

        template<class A, class B>
        constexpr range(A a, inf_t, B c) 
            : a(static_cast<Ty>(a)), 
              c(static_cast<Ty>(c)) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Ty;

            Ty value{};
            Ty incr{};

            constexpr iterator& operator++() { value += incr; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; value += incr; return b; }
            constexpr Ty operator*() const { return value; }
            constexpr bool operator==(const iterator& b) const { return value == b.value; }
        };

        constexpr iterator begin() const { return { a, c }; }
        constexpr iterator end() const { return { std::numeric_limits<Ty>::max(), c }; }
    };

    /**
     * Partial range.
     */
    template<class A, class B>
        requires (is_partial<A> || is_partial<B>)
    struct range<A, B> {
        using is_range = int;
        using depend = concat_t<depend<A>, depend<B>>;

        [[no_unique_address]] A a;
        [[no_unique_address]] B b;

        template<class T, class Q>
        constexpr range(T&& a, Q&& b) : a(std::forward<T>(a)), b(std::forward<Q>(b)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ kaixo::evaluate(a, tuple), kaixo::evaluate(b, tuple) };
        }

        KAIXO_EVALUATE_CALL_OPERATOR;
    };

    /**
     * Partial range with step size.
     */
    template<class A, class B, class C>
        requires (is_partial<A> || is_partial<B> || is_partial<C>)
    struct range<A, B, C> {
        using is_range = int;
        using depend = concat_t<depend<A>, depend<B>>;

        [[no_unique_address]] A a;
        [[no_unique_address]] B b;
        [[no_unique_address]] C c;

        template<class T, class Q, class R>
        constexpr range(T&& a, Q&& b, R&& c) 
            : a(std::forward<T>(a)), b(std::forward<Q>(b)), c(std::forward<R>(c)) {}

        constexpr auto evaluate(is_named_tuple auto& tuple) const {
            return kaixo::range{ 
                kaixo::evaluate(a, tuple), 
                kaixo::evaluate(b, tuple),
                kaixo::evaluate(c, tuple), 
            };
        }

        KAIXO_EVALUATE_CALL_OPERATOR;
    };
}