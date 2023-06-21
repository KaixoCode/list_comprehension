#pragma once
#include <type_traits>
#include <concepts>
#include <cstddef>
#include <utility>
#include <algorithm>
#include "kaixo/type_utils.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                      Unevaluated Expressions

             Expressions containing variables for which
                    values can be inserted later

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {

    /**
     * Forward as tuple, but only lvalue references are kept, rest is copied.
     * @param ...args arguments
     */
    template<class ...Args>
    constexpr auto ref_tuple(Args&& ...args) {
        using type = std::tuple<std::conditional_t<lvalue_reference<Args>, Args, decay_t<Args>>...>;
        return type{ std::forward<Args>(args)... };
    }

    namespace has {
        template<class Ty> concept depend_v = requires (Ty) { typename Ty::depend; };
        template<class Ty> concept define_v = requires (Ty) { typename Ty::define; };
        template<class Ty> concept var_v = requires (Ty) { typename Ty::var; };
        template<class Ty> concept range_v = requires (Ty) { typename Ty::range; };

        template<class Ty> struct depend_impl : std::bool_constant<depend_v<Ty>> {};
        template<class Ty> struct define_impl : std::bool_constant<define_v<Ty>> {};
        template<class Ty> struct var_impl : std::bool_constant<var_v<Ty>> {};
        template<class Ty> struct range_impl : std::bool_constant<range_v<Ty>> {};

        constexpr auto depend = type_trait<depend_impl>{};
        constexpr auto define = type_trait<define_impl>{};
        constexpr auto var = type_trait<var_impl>{};
        constexpr auto range = type_trait<range_impl>{};
    }

    namespace grab {
        template<class Ty> struct depend_impl { using type = info<>; };
        template<has::depend_v Ty> struct depend_impl<Ty> { using type = typename Ty::depend; };
        template<class Ty> struct define_impl { using type = info<>; };
        template<has::define_v Ty> struct define_impl<Ty> { using type = typename Ty::define; };
        template<class Ty> struct var_impl { using type = info<>; };
        template<has::var_v Ty> struct var_impl<Ty> { using type = typename Ty::var; };
        template<class Ty> struct range_impl { using type = info<>; };
        template<has::range_v Ty> struct range_impl<Ty> { using type = typename Ty::range; };

        template<class Ty> using depend = depend_impl<Ty>::type;
        template<class Ty> using define = define_impl<Ty>::type;
        template<class Ty> using var = var_impl<Ty>::type;
        template<class Ty> using range = range_impl<Ty>::type;
    }

    template<class Ty> using depend = grab::depend<decay_t<Ty>>;
    template<class Ty> using define = grab::define<decay_t<Ty>>;

    template<class Ty> concept explicit_range = requires() { typename Ty::is_range; };
    template<class Ty> concept is_range = std::ranges::range<Ty>;
    template<class Ty> concept is_partial = depend<Ty>::size != 0 || requires() { typename decay_t<Ty>::is_partial; };
    template<class Ty> concept is_partial_range = is_partial<Ty> && explicit_range<Ty>;
    template<class Ty> concept is_var = is_partial<Ty> && requires() { { decay_t<Ty>::name }; };
    template<class Ty> concept is_operator = requires() { typename Ty::is_operator; };
    template<class Ty> concept is_range_kind = is_range<Ty> || is_partial_range<Ty>;

    /**
     * Links value to a variable.
     * @tparam Ty value type
     * @tparam Var variable
     */
    template<class Ty, is_var Var>
    struct named_value {
        using value_type = Ty;
        using define = info<Var>;
        using depend = depend<Ty>;

        [[no_unique_address]] value_type value;

        constexpr decltype(auto) evaluate(auto& tuple) const;
        constexpr decltype(auto) execute(auto&, auto& tuple) const;
    };

    template<class Ty> concept is_named_value = specialization<Ty, named_value> && !is_partial<Ty>;

    /**
     * Variable.
     * @tparam Name variable name
     */
    template<string_literal Name>
    struct var {
        constexpr static string_literal name = Name;

        using depend = info<var>;

        template<class Ty>
        constexpr auto operator=(Ty&& value) const {
            return named_value<decay_t<Ty>, var>{ std::forward<Ty>(value) };
        }

        constexpr decltype(auto) evaluate(auto& tuple) const {
            constexpr bool contains = decay_t<decltype(tuple)>::template contains<var>;
            if constexpr (contains) return tuple.get<var>();
            else return var{};
        }
    };

    /**
     * Specialization for ignored var "_".
     */
    template<>
    struct var < string_literal<2, char>{ "_" } > {
        constexpr static string_literal name = "_";

        using is_partial = int;
        using depend = info<>;

        constexpr dud evaluate(auto&) const { return dud{}; }
    };

    /**
     * Wrap reference into reference wrapper, otherwise decay.
     * @tparam Ty type
     */
    template<class Ty>
    using wrap_reference = std::conditional_t<
        lvalue_reference<Ty>,
        std::reference_wrapper<remove_reference_t<Ty>>,
        decay_t<Ty>>;

    /**
     * Tuple of named values.
     * @tparam ...Args named values
     */
    template<is_named_value ...Args>
    struct named_tuple {
        // Wrap lvalue references in reference wrappers, this allows for overriding 
        // what reference is stored in the tuple, instead of just assigning to the 
        // reference that was stored on construction.
        using tuple_type = std::tuple<wrap_reference<typename Args::value_type>...>;
        using define = concat_t<kaixo::define<Args>...>::unique;

        template<is_var Var>
        constexpr static bool contains = define::template occurs<Var>;

        tuple_type value{};

        constexpr named_tuple(Args&&...args) : value(args.value...) {}
        constexpr named_tuple(tuple_type&& val) : value(std::move(val)) {}
        constexpr named_tuple(const tuple_type& val) : value(val) {}

        /**
         * Get value linked to variable.
         * @tparam Var variable
         */
        template<is_var Var, class Self>
        constexpr decltype(auto) get(this Self&& self) {
            static_assert(contains<decay_t<Var>>, "Tuple does not contain variable");
            constexpr std::size_t index = define::template index<Var>;
            using type = typename info<Args...>::template element<index>::type::value_type;
            if constexpr (lvalue_reference<type>) {
                return std::get<index>(std::forward<Self>(self).value).get();
            }
            else {
                return std::move(std::get<index>(std::forward<Self>(self).value));
            }
        }

        /**
         * Assign value to variable.
         * @tparam Var variable
         * @param value value
         */
        template<is_var Var, class Self, class Ty>
        constexpr decltype(auto) set(this Self&& self, Ty&& value) {
            static_assert(contains<decay_t<Var>>, "Variable is not part of tuple");
            constexpr std::size_t index = define::template index<Var>;
            std::get<index>(std::forward<Self>(self).value) = std::forward<Ty>(value);
        }

        /**
         * Assign another named tuple, either contains all variables of
         * this named tuple, or it contains none of them. Does not handle the
         * case where some variables are already in this tuple, and some are not.
         * @param val other named tuple
         */
        template<class Self, is_named_value ...Tys>
        constexpr decltype(auto) assign(this Self&& self, named_tuple<Tys...>&& val) {
            if constexpr ((contains<typename kaixo::define<Tys>::type>&& ...)) {
                ((std::forward<Self>(self).set<typename kaixo::define<Tys>::type>(
                    std::move(val).get<typename kaixo::define<Tys>::type>())), ...);
                return std::forward<Self>(self);
            }
            else {
                return named_tuple<Args..., Tys...>{
                    std::tuple_cat(std::forward<Self>(self).value, std::move(val).value)
                };
            }
        }
    };

    template<class Ty> concept is_named_tuple = specialization<Ty, named_tuple>;

    /**
     * Has an evaluate function that works for a certain tuple.
     */
    template<class A, class Tuple>
    concept has_evaluate_for = requires(A & a, Tuple & tuple) { { a.evaluate(tuple) }; };

    /**
     * Evaluate a variable in an expression.
     * @param var value
     * @param tuple named tuple
     */
    template<class A>
    constexpr decltype(auto) evaluate(A&& val, is_named_tuple auto& tuple) {
        if constexpr (has_evaluate_for<decay_t<A>, decay_t<decltype(tuple)>>)
            return val.evaluate(tuple);
        else return std::forward<A>(val);
    }

    template<class Ty, is_var Var>
    constexpr decltype(auto) named_value<Ty, Var>::execute(auto&, auto& tuple) const {
        return tuple.assign(named_tuple{ Var{} = evaluate(tuple) });
    }

    template<class Ty, is_var Var>
    constexpr decltype(auto) named_value<Ty, Var>::evaluate(auto& tuple) const {
        return kaixo::evaluate(value, tuple);
    }

    /**
     * Valid expression parts, one must be a partial value
     * and the rest may not be an operator or a range of any kind.
     */
    template<class ...As>
    concept are_valid_expression = (is_partial<decay_t<As>> || ...)
        && ((!is_range_kind<decay_t<As>> && !is_operator<decay_t<As>>) && ...);

    /**
     * Are valid arguments to an overloaded function for
     * partial values. Means at least one of the values is partial.
     */
    template<class ...As>
    concept are_valid_arguments = (is_partial<decay_t<As>> || ...);

    /**
     * Binary operator expression.
     * @tparam A first type
     * @tparam B second type
     * @tparam Op operator
     */
    template<class A, class B, is_operator Op>
        requires are_valid_expression<A, B>
    struct binary_operation {
        using depend = concat_t<depend<A>, depend<B>>::unique;

        [[no_unique_address]] A a{};
        [[no_unique_address]] B b{};

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return Op::evaluate(
                kaixo::evaluate(std::forward<Self>(self).a, tuple),
                kaixo::evaluate(std::forward<Self>(self).b, tuple));
        }
    };

    /**
     * Unary operator expression.
     * @tparam A first type
     * @tparam Op operator
     */
    template<is_partial A, is_operator Op>
    struct unary_operation {
        using depend = depend<A>;

        [[no_unique_address]] A a{};

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return Op::evaluate(kaixo::evaluate(std::forward<Self>(self).a, tuple));
        }
    };

    /**
     * Tuple expression, when evaluated creates a tuple.
     * @tparam ...As values, expression, or variables.
     */
    template<class ...As>
        requires are_valid_expression<As...>
    struct tuple_operation {
        using depend = concat_t<depend<As>...>::unique;

        std::tuple<std::conditional_t<lvalue_reference<As>, As, decay_t<As>>...> parts;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            auto res = [&]<std::size_t ...Is>(std::index_sequence<Is...>) {
                return ref_tuple(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).parts), tuple)...);
            }(std::index_sequence_for<As...>{});
            // Check whether the resulting tuple still has partial values.
            if constexpr (as_info<decltype(res)>::template count_filter < []<is_partial>{} > != 0)
                return move_tparams_t<decltype(res), tuple_operation>{ std::move(res) };
            else return res;
        }
    };

    /**
     * Tuple of vars, used in deconstruction of tuples, and
     * in resulting expressions to yield a tuple.
     */
    template<is_var ...As>
    struct var_tuple {
        using depend = concat_t<depend<As>...>::unique;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return tuple_operation<As...>{ std::tuple{ As{}... } }.evaluate(tuple);
        }
    };

    template<class Ty> concept is_var_tuple = specialization<Ty, var_tuple>;

    /**
     * All operators to construct the unevaluated expression objects.
     */
    namespace operators {
        // Variable pack operators
        template<is_var A, is_var B> constexpr auto operator,(const A&, const B&) { return var_tuple<A, B>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(var_tuple<Bs...>, const A&) { return var_tuple<Bs..., A>{}; }
        template<is_var A, is_var ...Bs> constexpr auto operator,(const A&, var_tuple<Bs...>) { return var_tuple<A, Bs...>{}; }

        // Tuple expression operators
        template<class A, class B>
            requires are_valid_expression<A, B>
        constexpr auto operator,(A&& a, B&& b) {
            return tuple_operation<decay_t<A>, decay_t<B>>{ std::tuple{ std::forward<A>(a), std::forward<B>(b) } };
        }

        template<class A, class ...Bs>
            requires are_valid_expression<Bs...>
        constexpr auto operator,(A&& a, tuple_operation<Bs...>&& b) {
            return tuple_operation<decay_t<A>, Bs...>{ std::tuple_cat(std::forward_as_tuple(std::forward<A>(a)), b.parts) };
        }

        template<class A, class ...Bs>
            requires are_valid_expression<Bs...>
        constexpr auto operator,(tuple_operation<Bs...>&& b, A&& a) {
            return tuple_operation<Bs..., decay_t<A>>{ std::tuple_cat(std::move(b).parts, std::forward_as_tuple(std::forward<A>(a))) };
        }

#define KAIXO_BINARY_OPERATOR(name, op)                              \
        struct name {                                                \
            using is_operator = int;                                 \
            template<class A, class B>                               \
            constexpr static decltype(auto) evaluate(A&& a, B&& b) { \
                return a op b;                                       \
            }                                                        \
        };                                                                                             \
                                                                                                       \
        template<class A, class B> requires are_valid_expression<A, B>                                 \
        constexpr binary_operation<decay_t<A>, decay_t<B>, name> operator op(A&& a, B&& b) {                 \
            return binary_operation<decay_t<A>, decay_t<B>, name>{ std::forward<A>(a), std::forward<B>(b) }; \
        }

#define KAIXO_UNARY_OPERATOR(name, op)                        \
        struct name {                                         \
            using is_operator = int;                          \
            template<class A>                                 \
            constexpr static decltype(auto) evaluate(A&& a) { \
                return op std::forward<A>(a);                 \
            }                                                 \
        };                                                                  \
                                                                            \
        template<is_partial A>                                              \
        constexpr unary_operation<decay_t<A>, name> operator op(A&& a) {    \
            return unary_operation<decay_t<A>, name>{ std::forward<A>(a) }; \
        }

        KAIXO_UNARY_OPERATOR(increment, ++);
        KAIXO_UNARY_OPERATOR(decrement, --);
        KAIXO_UNARY_OPERATOR(boolean_not, !);
        KAIXO_UNARY_OPERATOR(bitwise_not, ~);

        KAIXO_BINARY_OPERATOR(add, +);
        KAIXO_BINARY_OPERATOR(subtract, -);
        KAIXO_BINARY_OPERATOR(multiply, *);
        KAIXO_BINARY_OPERATOR(divide, / );
        KAIXO_BINARY_OPERATOR(modulo, %);
        KAIXO_BINARY_OPERATOR(less_than, < );
        KAIXO_BINARY_OPERATOR(less_or_equal, <= );
        KAIXO_BINARY_OPERATOR(greater_than, > );
        KAIXO_BINARY_OPERATOR(greater_or_equal, >= );
        KAIXO_BINARY_OPERATOR(equal, == );
        KAIXO_BINARY_OPERATOR(not_equal, != );
        KAIXO_BINARY_OPERATOR(left_shift, << );
        KAIXO_BINARY_OPERATOR(right_shift, >> );
        KAIXO_BINARY_OPERATOR(boolean_and, &&);
        KAIXO_BINARY_OPERATOR(boolean_or, || );
        KAIXO_BINARY_OPERATOR(bitwise_and, &);
        KAIXO_BINARY_OPERATOR(bitwise_or, | );
        KAIXO_BINARY_OPERATOR(bitwise_xor, ^);
        KAIXO_BINARY_OPERATOR(spaceship, <=> );
        KAIXO_BINARY_OPERATOR(add_assign, +=);
        KAIXO_BINARY_OPERATOR(subtract_assign, -=);
        KAIXO_BINARY_OPERATOR(multiply_assign, *=);
        KAIXO_BINARY_OPERATOR(divide_assign, /=);
        KAIXO_BINARY_OPERATOR(modulo_assign, %=);
        KAIXO_BINARY_OPERATOR(left_shift_assign, <<=);
        KAIXO_BINARY_OPERATOR(right_shift_assign, >>=);
        KAIXO_BINARY_OPERATOR(and_assign, &=);
        KAIXO_BINARY_OPERATOR(or_assign, |=);
        KAIXO_BINARY_OPERATOR(xor_assign, ^=);
    }

    namespace default_variables {
        constexpr var<"a"> a{};
        constexpr var<"b"> b{};
        constexpr var<"c"> c{};
        constexpr var<"d"> d{};
        constexpr var<"e"> e{};
        constexpr var<"f"> f{};
        constexpr var<"g"> g{};
        constexpr var<"h"> h{};
        constexpr var<"i"> i{};
        constexpr var<"j"> j{};
        constexpr var<"k"> k{};
        constexpr var<"l"> l{};
        constexpr var<"m"> m{};
        constexpr var<"n"> n{};
        constexpr var<"o"> o{};
        constexpr var<"p"> p{};
        constexpr var<"q"> q{};
        constexpr var<"r"> r{};
        constexpr var<"s"> s{};
        constexpr var<"t"> t{};
        constexpr var<"u"> u{};
        constexpr var<"v"> v{};
        constexpr var<"w"> w{};
        constexpr var<"x"> x{};
        constexpr var<"y"> y{};
        constexpr var<"z"> z{};
        constexpr var<"_"> _{}; // Special ignore variable
    }
}