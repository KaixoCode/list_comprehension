#pragma once
#include "kaixo/list_comprehension.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                          Range Inserter

            List Comprehension executable for inserting
                 values directly into a container

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {
    /**
     * Construct a pair from a tuple of size 2.
     * @param t tuple
     */
    template<specialization<std::tuple> Tuple>
        requires (std::tuple_size_v<decay_t<Tuple>> == 2)
    constexpr auto tuple_to_pair(Tuple&& t) {
        using pair_type = move_tparams_t<decay_t<Tuple>, std::pair>;
        return pair_type(std::get<0>(t), std::get<1>(t));
    }

    /**
     * Extension for a list comprehension to insert values
     * into a range directly.
     * @tparam Range range, must be non-const reference
     * @tparam Ty expression
     */
    template<is_range Range, class Ty>
    struct range_inserter {
        using depend = depend<Ty>;

        std::reference_wrapper<Range> value;
        Ty expr;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, auto& tuple) {
            return range_inserter{ value.get(), std::forward<Self>(self).expr };
        }

        template<class Self>
        constexpr decltype(auto) execute(this Self&& self, auto& code, auto& tuple) {
            using value_type = decay_t<Range>::value_type;
            using result_type = decltype(kaixo::evaluate(std::forward<Self>(self).expr, tuple));
            auto& container = std::forward<Self>(self).value.get();
            // Need a special case for value type of pair, as there's no conversion
            // operator between a tuple with 2 elements and a pair.
            if constexpr (!specialization<result_type, std::pair> && specialization<value_type, std::pair>) {
                container.insert(container.end(), tuple_to_pair(kaixo::evaluate(std::forward<Self>(self).expr, tuple)));
            } else {
                container.insert(container.end(), kaixo::evaluate(std::forward<Self>(self).expr, tuple));
            }
            return tuple;
        }
    };

    namespace operators {
        template<is_range Range, is_partial Ty>
        constexpr auto operator<<(Range& range, Ty&& expr) {
            return range_inserter<Range, decay_t<Ty>>{ range, std::forward<Ty>(expr) };
        }
    }
}