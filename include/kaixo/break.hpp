#pragma once
#include "kaixo/list_comprehension.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                        Break Expression

            List Comprehension executable which stops
           iteration when expression evaluates to true

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {
    /**
     * A break expression stops the list comprehension
     * when its contained expression evaluates to true.
     */
    template<class A>
    struct break_expression {
        using depend = depend<A>;

        [[no_unique_address]] A expr;

        template<class Self>
        constexpr auto evaluate(this Self&& self, is_named_tuple auto& tuple) {
            using result = decay_t<decltype(kaixo::evaluate(std::forward<Self>(self).expr, tuple))>;
            return break_expression<result>{ kaixo::evaluate(std::forward<Self>(self).expr, tuple) };
        }

        template<class Self>
        constexpr decltype(auto) execute(this Self&& self, return_code& code, is_named_tuple auto& tuple) {
            if (kaixo::evaluate(std::forward<Self>(self).expr, tuple)) code = return_code::stop;
            return tuple;
        }
    };

    constexpr struct break_t {
        template<class A>
        constexpr auto operator=(A&& a) const {
            return break_expression{ std::forward<A>(a) };
        }
    } brk{};
}