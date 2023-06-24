#pragma once
#include "kaixo/list_comprehension.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                          Zipped Range

             Enables parallel iteration of containers
                    with support for partials

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {
    /**
     * Zipped range for parallel iteration.
     */
    template<class ...As> struct zipped_range;
    template<class ...As> zipped_range(std::tuple<As...>&&) -> zipped_range<As...>;

    template<is_range ...As>
    struct zipped_range<As...> {
        using define = concat_t<define<As>...>;
        using depend = concat_t<depend<As>...>;

        using reference = std::tuple<std::ranges::range_reference_t<As>...>;
        using value_type = std::tuple<std::ranges::range_value_t<As>...>;

        std::tuple<As...> ranges;

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using reference = reference;
            using value_type = value_type;

            using iterators = std::tuple<std::ranges::iterator_t<As>...>;

            iterators iters{};

            constexpr iterator& operator++() {
                sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                    ((++std::get<Is>(iters)), ...);
                });
                return *this;
            }

            constexpr iterator operator++(int) {
                iterator b = *this;
                operator++();
                return b;
            }

            constexpr reference operator*() const {
                return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                    return reference{ std::forward_as_tuple((*std::get<Is>(iters))...) };
                });
            }

            constexpr bool operator==(const iterator& o) const {
                return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                    return ((std::get<Is>(iters) == std::get<Is>(o.iters)) || ...);
                });
            }
        };

        constexpr iterator begin() const {
            return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                return iterator{ std::make_tuple(std::ranges::begin(std::get<Is>(ranges))...) };
            });
        }

        constexpr iterator end() const {
            return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                return iterator{ std::make_tuple(std::ranges::end(std::get<Is>(ranges))...) };
            });
        }
    };

    template<class Ty>
    concept is_zipped_range = specialization<Ty, zipped_range>;

    template<class ...As>
        requires (((is_partial<As> || is_range<As>) && ...) && (is_partial<As> || ...))
    struct zipped_range<As...> {
        using is_range = int;
        using depend = concat_t<depend<As>...>;

        std::tuple<As...> ranges;

        template<class Self>
        constexpr auto evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return sequence<sizeof...(As)>([&]<std::size_t ...Is>() {
                return kaixo::zipped_range{
                    std::make_tuple(
                        wrap_range(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).ranges), tuple))...)
                };
            });
        }

        KAIXO_EVALUATE_CALL_OPERATOR;
    };

    template<class Ty>
    concept is_partial_zipped_range = is_zipped_range<Ty> && is_partial<Ty>;

    namespace operators {
        template<class A, class B>
            requires ((is_range_kind<A> || is_range_kind<B>)
        && !is_lc<A> && !is_partial_lc<A>
            && !is_zipped_range<A> && !is_partial_zipped_range<A>)
            constexpr auto operator,(A&& a, B&& b) {
            return zipped_range{
                std::make_tuple(wrap_range(std::forward<A>(a)), wrap_range(std::forward<B>(b)))
            };
        }

        template<class A, is_range_kind B>
            requires (is_zipped_range<A> || is_partial_zipped_range<A>)
        constexpr auto operator,(A&& a, B&& b) {
            return zipped_range{
                std::tuple_cat(std::forward<A>(a).ranges, std::make_tuple(wrap_range(std::forward<B>(b))))
            };
        }
    }
}