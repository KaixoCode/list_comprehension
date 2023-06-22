#pragma once
#include "kaixo/expression.hpp"

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                        List Comprehension

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

namespace kaixo {
    template<class Range> struct range_wrapper;

    // Owning range
    template<is_range Range>
        requires (!lvalue_reference<Range>)
    struct range_wrapper<Range> {
        using range_type = decay_t<Range>;

        range_type value;

        using value_type = std::ranges::range_value_t<const range_type>;
        using reference = std::ranges::range_reference_t<const range_type>;
        using iterator = std::ranges::iterator_t<const range_type>;

        constexpr iterator begin() const { return value.begin(); }
        constexpr iterator end() const { return value.end(); }
    };

    // Reference to range
    template<is_range Range>
        requires lvalue_reference<Range>
    struct range_wrapper<Range> {
        using range_type = remove_reference_t<Range>;

        std::reference_wrapper<range_type> value;

        using value_type = std::ranges::range_value_t<range_type>;
        using reference = std::ranges::range_reference_t<range_type>;
        using iterator = std::ranges::iterator_t<range_type>;

        constexpr iterator begin() const { return value.get().begin(); }
        constexpr iterator end() const { return value.get().end(); }
    };

    template<class Ty> concept is_range_wrapper = specialization<Ty, range_wrapper>;

    /**
     * Wrap a range into either an owning or reference wrapper.
     * @param range range
     */
    template<is_range Range>
        requires (!is_range_wrapper<Range>)
    constexpr range_wrapper<Range&&> wrap_range(Range&& range) {
        return range_wrapper<Range&&>{ { std::forward<Range>(range) } };
    }

    /**
     * Overload for non-ranges, just returns exact input back.
     * @param ty non-range
     */
    template<class Ty>
    constexpr Ty&& wrap_range(Ty&& ty) {
        return std::forward<Ty>(ty);
    }

    /**
     * Return code for executables in comprehension.
     */
    enum class return_code {
        none = 0, // Continue like normal
        stop = 1, // Stop iteration now, and set to end
        skip = 2, // Skip current values, and increment again
    };

    constexpr return_code choose_code(return_code a, return_code b) {
        return (return_code)std::max((int)a, (int)b);
    }

    /**
     * Tests if expression is executable with a certain named tuple.
     * @tparam Ty type
     * @tparam Tuple named tuple
     */
    template<class Ty, class Tuple>
    concept is_executable_with = requires(decay_t<Ty>&val, return_code & code, decay_t<Tuple>&tuple) {
        { val.execute(code, tuple) } -> is_named_tuple;
    };

    /**
     * Execute a list comprehension extension.
     * @param e instance of the extension
     * @param code out parameter, set return code
     * @param tuple named tuple
     */
    template<class E>
    constexpr decltype(auto) execute(E&& e, return_code& code, is_named_tuple auto& tuple) {
        // Default executables
        if constexpr (is_executable_with<E, decltype(tuple)>)
            return std::forward<E>(e).execute(code, tuple);
        // Constraints: expressions resulting in bool to skip values in comprehension.
        else if constexpr (std::convertible_to<decltype(evaluate(std::forward<E>(e), tuple)), bool>) {
            code = bool(evaluate(std::forward<E>(e), tuple)) ? return_code::none : return_code::skip;
            return tuple;
        } else return tuple;
    }

    // Not partial, so full type is itself.
    template<class R, is_named_tuple T> 
    struct full_type : std::type_identity<R> {};

    // Evaluate partial and recurse.
    template<is_partial R, is_named_tuple T> 
    struct full_type<R, T> 
        : full_type<decltype(std::declval<R&>().evaluate(std::declval<T&>())), T> {};

    /**
     * Get the complete type, given a named tuple.
     * @tparam R partial type
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    using full_type_t = full_type<R, T>::type;

    // Not anything, so does not add new values.
    template<class R, is_named_tuple T>
    struct defined_values : std::type_identity<T> {};

    // Specialization for executables,  check return type of execute method.
    template<class R, is_named_tuple T>   
        requires is_executable_with<R, T>
    struct defined_values<R, T> 
        : std::type_identity<decay_t<decltype(std::declval<R&>().execute(
            std::declval<return_code&>(), std::declval<T&>()))>> {};

    // Specialization for ranges, reference type should always be a named tuple.
    template<is_range R, is_named_tuple T> 
    struct defined_values<R, T> 
        : std::type_identity<prepend_t<as_info<T>, typename R::reference>> {};

    // Evaluate partials to full type and recurse.
    template<is_partial_range R, is_named_tuple T> 
    struct defined_values<R, T> : defined_values<full_type_t<R, T>, T> {};

    /**
     * Get the defined named values of type R.
     * @tparam R type to get defined named values of
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    using defined_values_t = typename defined_values<R, T>::type;

    template<is_named_tuple Tuple, class ...Parts>
    struct named_tuple_type;

    template<is_named_tuple Tuple, class Part, class ...Parts>
    struct named_tuple_type<Tuple, Part, Parts...>
        : named_tuple_type<defined_values_t<Part, Tuple>, Parts...> {};

    template<is_named_tuple Tuple, class Part>
    struct named_tuple_type<Tuple, Part> {
        using type = defined_values_t<Part, Tuple>;
    };

    /**
     * Get the type of the named tuple given all parts.
     * @tparam Tuple named tuple
     * @tparam Parts parts
     */
    template<class ...Parts>
    using named_tuple_type_t = named_tuple_type<named_tuple<>, Parts...>::type;

    // Use dud for non-partials.
    template<class R, is_named_tuple T>
    struct intermediate_value : std::type_identity<dud> {};

    // Partial range stores optional to its full type.
    template<is_partial_range R, is_named_tuple T>
    struct intermediate_value<R, T> : std::type_identity<std::optional<full_type_t<R, T>>> {};

    /**
     * Get the intermediate value of type R. For partial ranges
     * this is used to store the full range once evaluated.
     * @tparam R type to get intermediate value of
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    using intermediate_value_t = intermediate_value<R, T>::type;

    // Not a range, so no iterator, use dud.
    template<class R, is_named_tuple T>
    struct iterator_data : std::type_identity<dud> {};

    // Normal range, just get its iterator type.
    template<is_range R, is_named_tuple T> 
    struct iterator_data<R, T> : std::type_identity<std::ranges::iterator_t<R>> {};

    // Evaluate partial range to full type and recurse.
    template<is_partial_range R, is_named_tuple T>
    struct iterator_data<R, T> : iterator_data<full_type_t<R, T>, T> {};

    /**
     * Get the iterator type of a range, given a named tuple.
     * @tparam R (partial) range
     * @tparam T named tuple
     */
    template<class R, is_named_tuple T>
    using iterator_data_t = iterator_data<R, T>::type;

    /**
     * Overload for non-tuple, return single value as tuple.
     * @param arg value
     */
    template<class Ty>
    constexpr std::tuple<Ty&&> flatten_tuple(Ty&& arg) {
        return std::forward_as_tuple(std::forward<Ty>(arg));
    }

    /**
     * Overload for pair, flatten both values in pair and concat.
     * @param pair pair
     */
    template<specialization<std::pair> Pair>
    constexpr decltype(auto) flatten_tuple(Pair&& pair) {
        return std::tuple_cat(
            flatten_tuple(std::forward<Pair>(pair).first),
            flatten_tuple(std::forward<Pair>(pair).second));
    }

    /**
     * Overload for tuple, flatten all values in tuple and concat.
     * @param tuple tuple
     */
    template<specialization<std::tuple> Tuple>
    constexpr decltype(auto) flatten_tuple(Tuple&& tuple) {
        constexpr std::size_t size = std::tuple_size_v<decay_t<Tuple>>;
        return sequence<size>([&]<std::size_t ...Is>() {
            return std::tuple_cat(flatten_tuple(std::get<Is>(std::forward<Tuple>(tuple)))...);
        });
    }

    /**
     * Flatten a tuple type containing tuples.
     * @tparam Ty tuple
     */
    template<class Ty>
    using flatten_tuple_type_t = decltype(flatten_tuple(std::declval<Ty>()));

    template<class T, class V, class Q>
    struct zip_as_named_tuple;

    /**
     * Zip value types and variables as a named_tuple.
     * @tparam T tuple of value types
     * @tparam V pack of variables
     * @tparam Q type to copy cvref from
     */
    template<template<class...> class R, class ...Tys, is_var ...Vars, class Q>
    struct zip_as_named_tuple<R<Tys...>, info<Vars...>, Q>
        : std::type_identity<named_tuple<named_value<add_cvref_t<Tys, Q>, Vars>...>> {};

    /**
     * Determine the reference type of a named range based on
     * provided variables and the range.
     * @tparam Range range
     * @tparam ...Vars defined variables
     */
    template<is_range Range, is_var ...Vars>
    struct determine_named_range_reference;

    // One-to-one relation, just match reference type 
    // of range with the variable.
    template<is_range Range, is_var Var>
    struct determine_named_range_reference<Range, Var>
        : std::type_identity<named_tuple<
            named_value<std::ranges::range_reference_t<Range>, Var>>> {
        constexpr static bool flatten = false;
    };

    // Range results in tuple with same size as amount of variables.
    // Deconstruct tuple and match every value with its respective variable.
    template<is_range Range, is_var ...Vars>
        requires (as_info<std::ranges::range_reference_t<Range>>::size == sizeof...(Vars))
    struct determine_named_range_reference<Range, Vars...>
        : zip_as_named_tuple<
            decay_t<std::ranges::range_reference_t<Range>>, 
            info<Vars...>, 
            std::ranges::range_reference_t<Range>> {
        constexpr static bool flatten = false;
    };

    // Variable count matches flattened range result.
    template<is_range Range, is_var ...Vars>
        requires (
            as_info<std::ranges::range_reference_t<Range>>::size != sizeof...(Vars) && 
            as_info<flatten_tuple_type_t<std::ranges::range_reference_t<Range>>>::size == sizeof...(Vars))
    struct determine_named_range_reference<Range, Vars...> 
        : zip_as_named_tuple<
            flatten_tuple_type_t<std::ranges::range_reference_t<Range>>, 
            info<Vars...>, 
            std::ranges::range_reference_t<Range>> {
        constexpr static bool flatten = true;
    };

    template<class Range, is_var ...Vars> struct named_range;
    template<class T, class ...V> named_range(const T&, const V&...) -> named_range<T, V...>;
    template<class T, class ...V> named_range(T&&, const V&...) -> named_range<T, V...>;

    /**
     * Range linked to variables.
     * @tparam Range range
     * @tparam ...Vars variables
     */
    template<is_range Range, is_var ...Vars>
    struct named_range<Range, Vars...> {
        using define = info<Vars...>;

        using range_type = Range;
        using _range_info = determine_named_range_reference<Range, Vars...>;
        using reference = _range_info::type;
        using value_type = decay_t<reference>;

        [[no_unique_address]] Range rng;

        constexpr named_range(Range&& range, const Vars&...) : rng(std::move(range)) {}
        constexpr named_range(const Range& range, const Vars&...) : rng(range) {}

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using reference = reference;
            using value_type = value_type;

            std::ranges::iterator_t<const Range> it{};

            constexpr iterator& operator++() { ++it; return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const { return it == other.it; }

            constexpr reference operator*() {
                if constexpr (_range_info::flatten)
                    return reference{ flatten_tuple(*it) };
                else return reference{ *it };
            }
        };

        constexpr iterator begin() const { return iterator{ std::ranges::begin(rng) }; }
        constexpr iterator end() const { return iterator{ std::ranges::end(rng) }; }
    };

    /**
     * Partial range linked to variables, still depends on variables.
     * @tparam Range range
     * @tparam ...Vars variables
     */
    template<is_partial Range, is_var ...Vars>
    struct named_range<Range, Vars...> {
        using is_range = int;

        using define = info<Vars...>;
        using depend = depend<Range>;

        [[no_unique_address]] Range rng;

        constexpr named_range(Range&& range, const Vars&...) : rng(std::move(range)) {}
        constexpr named_range(const Range& range, const Vars&...) : rng(range) {}

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return kaixo::named_range{
                wrap_range(kaixo::evaluate(std::forward<Self>(self).rng, tuple)), Vars{}...
            };
        }
    };

    /**
     * List comprehension object.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
    struct list_comprehension {
        using reference = std::conditional_t<is_partial<R>,
            decltype(evaluate(std::declval<R&>(), std::declval<named_tuple_type_t<Parts...>&>())), R>;
        using value_type = decay_t<reference>;

        struct iterator {
            using iterator_category = std::input_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = value_type;
            using reference = reference;

            constexpr iterator() : at_end(true) {}
            constexpr iterator(const list_comprehension& self) : self(&self) { prepare(); }

            constexpr iterator& operator++() { increment<sizeof...(Parts) - 1>(); return *this; }
            constexpr iterator operator++(int) { iterator b = *this; operator++(); return b; }

            constexpr bool operator!=(const iterator& other) const { return !operator==(other); }
            constexpr bool operator==(const iterator& other) const {
                return end() == true && other.end() == true // If end, iterators don't matter.
                    || other.end() == end() && other.iterators == iterators;
            }

            constexpr reference operator*() {
                if (end()) throw; // Can't access past end
                return kaixo::evaluate(self->result, values.value());
            }

        private:
            using named_tuple_type = named_tuple_type_t<Parts...>;
            using iterator_datas = std::tuple<iterator_data_t<Parts, named_tuple_type>...>;
            using intermediate_values = std::tuple<intermediate_value_t<Parts, named_tuple_type>...>;

            intermediate_values intermediate{};
            iterator_datas iterators{};
            std::optional<named_tuple_type> values{};
            const list_comprehension* self = nullptr;
            bool at_end = false;

            constexpr bool end() const { return at_end; }
            constexpr void set_end() { at_end = true; }

            template<std::size_t I>
            constexpr void increment() {
                using type = info<Parts...>::template element<I>::type;
                if constexpr (is_range_kind<type>) {
                    auto& part = std::get<I>(self->parts);
                    auto& intr = std::get<I>(intermediate);
                    auto& iter = std::get<I>(iterators);

                    auto at_end = [&] {
                        if constexpr (is_partial_range<type>)
                            return ++iter == std::ranges::end(intr.value());
                        else return ++iter == std::ranges::end(part);
                    };

                    auto to_begin = [&] {
                        if constexpr (is_partial_range<type>) {
                            intr = evaluate(part, values.value());
                            iter = std::ranges::begin(intr.value());
                        }
                        else iter = std::ranges::begin(part);
                    };

                    do {
                        if (at_end()) {
                            if constexpr (I != 0) increment<I - 1>();
                            to_begin();
                            if constexpr (I == 0) { set_end(); return; }
                        }

                        values.value().assign(*iter);

                        if constexpr (I != sizeof...(Parts) - 1) {
                            return_code code = evaluate_i<I + 1>();
                            if (end() || code == return_code::stop) { set_end(); return; }
                            else if (code == return_code::skip) continue;
                        }
                        return;
                    } while (true);
                }
                else if constexpr (I == 0) { set_end(); return; }// We're at the end!
                else return increment<I - 1>();
            }

            template<std::size_t I>
            constexpr return_code evaluate_i() {
                using type = info<Parts...>::template element<I>::type;

                if constexpr (!is_range_kind<type>) {
                    auto& part = std::get<I>(self->parts);

                    return_code code = return_code::none;

                    execute(part, code, values.value());

                    if (code != return_code::none) return code;
                    if constexpr (I == sizeof...(Parts) - 1) return code;
                    else return evaluate_i<I + 1>();
                }
                else return return_code::none;
            }

            template<std::int64_t I, class Tuple>
            constexpr return_code initialize(Tuple&& cur_values, return_code code) {
                if constexpr (I == sizeof...(Parts)) {
                    values.emplace(std::forward<Tuple>(cur_values).value);
                    return code;
                } else {
                    using type = info<Parts...>::template element<I>::type;
                    auto& part = std::get<I>(self->parts);
                    if constexpr (is_range_kind<type>) {
                        auto& intr = std::get<I>(intermediate);
                        auto& iter = std::get<I>(iterators);

                        auto at_end = [&] {
                            if constexpr (is_partial_range<type>)
                                return iter == std::ranges::end(intr.value());
                            else return iter == std::ranges::end(part);
                        };

                        if constexpr (is_partial_range<type>) {
                            intr = evaluate(part, cur_values);
                            iter = std::ranges::begin(intr.value());
                        }
                        else iter = std::ranges::begin(part);

                        if (at_end()) {
                            set_end();
                            return return_code::stop;
                        }

                        return initialize<I + 1>(std::forward<Tuple>(cur_values).assign(*iter), code);
                    } else {
                        return_code new_code = return_code::none;
                        using returned_tuple = decltype(execute(part, new_code, cur_values));
                        // If returns same tuple (did not add values), don't evaluate
                        // when already determined this initial value is invalid.
                        if constexpr (kaixo::reference<returned_tuple>) {
                            if (code != return_code::none) {
                                return initialize<I + 1>(std::forward<Tuple>(cur_values), code);
                            }
                        }
                        decltype(auto) res = execute(part, new_code, cur_values);
                        return initialize<I + 1>(res, choose_code(new_code, code));
                    }
                }
            }

            constexpr void prepare() {
                return_code _code = initialize<0>(named_tuple<>{}, return_code::none); // Set iterators to begin
                if (_code == return_code::skip) operator++();
                if (_code == return_code::stop) set_end();
            }
        };

        using const_iterator = iterator;

        [[no_unique_address]] R result;
        std::tuple<Parts...> parts;

        constexpr iterator begin() const { return iterator(*this); }
        constexpr iterator end() const { return iterator(); }

        constexpr bool empty() const { return begin() == end(); }

        constexpr reference operator[](std::size_t index) const {
            auto _it = begin();
            while (index > 0) ++_it, --index;
            return *_it;
        }

        template<class Ty> 
            requires (!specialization<Ty, std::initializer_list> && constructible<Ty, iterator, iterator>)
        constexpr operator Ty() const { return Ty{ begin(), end() }; }
    };

    template<class R, class ...As> list_comprehension(R&&, std::tuple<As...>&&) -> list_comprehension<R, As...>;

    /**
     * Partial list comprehension, still misses some variables.
     * @tparam R result expression
     * @tparam ...Parts comprehension parts
     */
    template<class R, class ...Parts>
        requires (concat_t<depend<R>, depend<Parts>...>::unique
            ::template remove<typename concat_t<define<Parts>...>::unique>::size != 0)
    struct list_comprehension<R, Parts...> {
        using is_range = int;

        using depend = concat_t<depend<R>, depend<Parts>...>::unique
            ::template remove<typename concat_t<define<Parts>...>::unique>;

        [[no_unique_address]] R result;
        std::tuple<Parts...> parts;

        template<class Self>
        constexpr decltype(auto) evaluate(this Self&& self, is_named_tuple auto& tuple) {
            return sequence<sizeof...(Parts)>([&]<std::size_t ...Is>() {
                return kaixo::list_comprehension{ // No more dependencies
                    kaixo::evaluate(std::forward<Self>(self).result, tuple),
                    std::make_tuple(std::move(kaixo::evaluate(std::get<Is>(std::forward<Self>(self).parts), tuple))...)
                };
            });
        }

        template<class Self, is_named_value ...Tys>
        constexpr decltype(auto) operator()(this Self&& self, Tys&& ...vals) {
            named_tuple tpl{ std::forward<Tys>(vals)... };
            return std::forward<Self>(self).evaluate(tpl);
        }
    };

    template<class Ty> concept is_lc = specialization<Ty, list_comprehension>;
    template<class Ty> concept is_partial_lc = is_lc<Ty> && is_partial<Ty>;

    namespace operators {
        constexpr decltype(auto) operator-(is_range auto& r) { return wrap_range(r); }
        constexpr decltype(auto) operator-(is_range auto&& r) { return wrap_range(std::move(r)); }
        constexpr decltype(auto) operator-(is_partial auto& r) { return r; }
        constexpr decltype(auto) operator-(is_partial auto&& r) { return std::move(r); }

        constexpr decltype(auto) operator<(is_var auto v, is_range auto&& r) {
            return named_range{ std::move(r), v };
        }

        constexpr decltype(auto) operator<(is_var auto v, is_partial auto& r) {
            return named_range{ std::move(r), v };
        }

        constexpr decltype(auto) operator<(is_var auto v, is_partial auto&& r) {
            return named_range{ std::move(r), v };
        }

        template<is_var ...Vars>
        constexpr decltype(auto) operator<(var_tuple<Vars...>, is_range auto&& r) {
            return named_range{ std::move(r), Vars{}... };
        }

        template<is_var ...Vars>
        constexpr decltype(auto) operator<(var_tuple<Vars...>, is_partial auto& r) {
            return named_range{ std::move(r), Vars{}... };
        }

        template<is_var ...Vars>
        constexpr decltype(auto) operator<(var_tuple<Vars...>, is_partial auto&& r) {
            return named_range{ std::move(r), Vars{}... };
        }

        template<class A, class B>
        constexpr decltype(auto) construct_lc(A&& a, B&& b) {
            return list_comprehension<decay_t<A>, decay_t<B>>{
                std::forward<A>(a), std::tuple{ std::forward<B>(b) }
            };
        };

        template<class Ty, class Part>
            requires (is_lc<Ty> || is_partial_lc<Ty>)
        constexpr auto operator,(Ty&& lc, Part&& part) {
            return list_comprehension{ std::forward<Ty>(lc).result,
                std::tuple_cat(std::forward<Ty>(lc).parts, std::tuple(std::forward<Part>(part)))
            };
        };

        template<is_partial A, class B>
        constexpr auto operator|(A&& a, B&& b) {
            return construct_lc(std::forward<A>(a), std::forward<B>(b));
        }

        template<is_var ...As, class B>
        constexpr auto operator|(var_tuple<As...>, B&& b) {
            return construct_lc(tuple_operation<As...>{ std::tuple{ As{}... } }, std::forward<B>(b));
        }
    }
}