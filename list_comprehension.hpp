#include <vector>
#include <deque>
#include <forward_list>
#include <set>
#include <map>
#include <list>
#include <unordered_set>
#include <unordered_map>
#include <stack>
#include <queue>
#include <span>
#include <functional>
#include <iterator>
#include <tuple>
#include <map>
#include <optional>
#include <bitset>
#include <any>

namespace kaixo {

    // Utils
    template<class Type>
    using container_for = std::conditional_t<std::is_same_v<Type, char>, std::string, std::vector<Type>>;

    template<class Type, class ...Tys>
    concept has_emplace_back = requires(Type t, Tys...tys) { 
        t.emplace_back(tys...); 
    };

    template<class Type, class ...Tys>
    concept has_try_emplace = requires(Type t, Tys...tys) {
        t.try_emplace(tys...); 
    };

    template<class Type>
    concept has_begin_end = requires(Type a) {
        a.begin();
        a.end();
        a.size();
    };

    template<class Type>
    concept has_clear = requires(Type a) { 
        a.clear(); 
    };

    template<class> struct return_type;
    template<class R, class...T> struct return_type<R(T...)> { 
        using type = R; 
    };

    template<class> struct to_refs;
    template<class ...Args> struct to_refs<std::tuple<Args...>> { 
        using type = std::tuple<Args&...>; 
    };

    template<class Ty>
    struct to_val_or_ref { 
        using type = Ty; 
    };

    template<class Ty>
    struct to_val_or_ref<const Ty&> { 
        using type = Ty; 
    };

    /*
     * Expression storage, basically type erased lambda storage 
     * through inheritance. Works as shared ptr with ref count.
     */
    template<class Ty>
    struct expr_storage_base {
        size_t refs = 1;
        virtual Ty get() = 0;
    };

    template<class R, std::invocable<> Ty> requires std::same_as<R, std::invoke_result_t<Ty>>
    struct lambda_expr_storage : expr_storage_base<R> {
        Ty lambda;

        lambda_expr_storage(Ty t) 
            : lambda(t) {}

        R get() { return lambda(); }
    };

    template<class Ty>
    struct expr_storage {
        using value_type = std::decay_t<Ty>;
        
        expr_storage() : storage(nullptr) {}
        expr_storage(expr_storage&& other) : storage(other.storage) { other.storage = nullptr; }
        expr_storage(const expr_storage& other) : storage(other.storage) { if (storage) storage->refs++; }
        ~expr_storage() { clean(); }

        template<std::invocable<> T> requires std::same_as<Ty, std::invoke_result_t<T>>
        expr_storage(T t) : storage(new lambda_expr_storage<Ty, decltype(t)>{ t }) {}

        expr_storage& operator=(expr_storage&& other) {
            clean();
            storage = other.storage;
            other.storage = nullptr;
            return *this;
        }

        expr_storage& operator=(const expr_storage& other) {
            clean();
            storage = other.storage;
            storage->refs++;
            return *this;
        }

        template<std::invocable<> T> requires std::same_as<Ty, std::invoke_result_t<T>>
        auto operator=(T t) { 
            clean(); 
            storage = new lambda_expr_storage<Ty, decltype(t)>{ t }; 
            return *this; 
        }

        inline void clean() { 
            if (storage && --storage->refs == 0) 
                delete storage; 
        }

        expr_storage_base<Ty>* storage;
    };

    /**
     * Variable storage, stores either a value or a reference (as pointer).
     * Also works as a shared pointer, makes sure the contained value stays
     * alive as long as any list comprehension object uses this variable.
     */
    template<class Ty> requires (!std::is_reference_v<Ty>)
    struct var_storage_base {
        enum _state { n, v, r } state = n;
        
        var_storage_base() : ref(nullptr), state(n) {}
        var_storage_base(var_storage_base&&) = delete;
        var_storage_base(const var_storage_base&) = delete;

        var_storage_base(Ty& val) : ref(&val), state(r) {}
        var_storage_base(Ty&& val) : value(std::move(val)), state(v) {}
        var_storage_base(const Ty& val) : value(val), state(v) {}

        ~var_storage_base() { clean(); }

        inline void operator=(Ty& val) {
            if constexpr (!std::is_trivially_destructible_v<Ty>)
                clean(); 
            ref = &val; 
            state = r; 
        }

        template<class T> requires std::constructible_from<Ty, T>
        inline void operator=(T&& val) { 
            if constexpr (!std::is_trivially_destructible_v<Ty>)
                clean();
            new (&value) Ty{ std::forward<T>(val) };
            state = v; 
        }

        size_t refs = 1;
        
        inline Ty& get() {
            if constexpr (std::is_default_constructible_v<Ty>)
                if (state == n)
                    new (&value) Ty, state = v;
            
            return state == v ? value : *ref; 
        };

        inline void clean() { 
            if constexpr (!std::is_trivially_destructible_v<Ty>)
                if (state == v) value.~Ty();
        }

        union {
            Ty value;
            Ty* ref;
        };
    };

    template<class Ty> requires (!std::is_reference_v<Ty>)
    struct var_storage {
        using value_type = Ty;

        var_storage() : storage(new var_storage_base<Ty>{}) {}
        var_storage(var_storage&& other) : storage(other.storage) { other.storage = nullptr; }
        var_storage(const var_storage& other) : storage(other.storage) { storage->refs++; }

        ~var_storage() { clean(); }

        var_storage(value_type& t) : storage(new var_storage_base<Ty>{ t }) {}
        var_storage(value_type&& t) : storage(new var_storage_base<Ty>{ std::move(t) }) {}
        var_storage(const value_type& t) : storage(new var_storage_base<Ty>{ t }) {}       

        var_storage& operator=(var_storage& other) { clean(), storage = other.storage, storage->refs++; return *this; }
        var_storage& operator=(var_storage&& other) { clean(), storage = other.storage, other.storage = nullptr; return *this; }
        var_storage& operator=(const var_storage& other) { clean(), storage = other.storage, storage->refs++; return *this; }

        template<class Arg> requires std::constructible_from<Ty, Arg>
        inline var_storage& operator=(Arg&& arg) { *storage = std::forward<Arg>(arg); return *this; };

        inline void clean() { 
            if (storage && --storage->refs == 0)
                delete storage; 
        }

        var_storage_base<Ty>* storage;
    };

    /**
     * Basis for an expr and var, has certain storage that will generate
     * the result, either variable or from expression stored in a lambda.
     */
    struct is_an_expr {};
    template<class Ty, class Storage = expr_storage<Ty>> struct expr;
    template<class Ty> requires (!std::is_reference_v<Ty>) struct var;
    template<class Ty, class Storage = expr_storage<Ty>>
    struct expr_base : is_an_expr {
        using type = Ty;

        expr_base() = default;
        expr_base(expr_base&&) = default;
        expr_base(const expr_base&) = default;

        // When a var is converted to an expression, it will become a lambda
        // that captures by value, and returns the result of the invocation of run_expression
        template<class Type> requires std::same_as<Storage, expr_storage<Ty>>
        expr_base(const var<Type>& v) : storage([v]() -> Ty { return v.run_expression(); }) {}

        template<class ...Args> requires std::constructible_from<Storage, Args...>
        expr_base(Args&&...args) : storage(std::forward<Args>(args)...) {}

        expr_base& operator=(expr_base&&) = default;
        expr_base& operator=(const expr_base&) = default;

        template<class Args> requires std::assignable_from<Storage, Args>
        expr_base& operator=(Args&& args) {
            storage = std::forward<Args>(args);
            return *this;
        }

        /**
         * Evaluate the expression
         * @return result
         */
        inline Ty run_expression() const { return storage.storage->get(); }

        /**
         * Cast to type.
         * @return expr of new type
         */
        template<class Ty> inline expr<Ty> to() {
            return expr<Ty>{ [e = *this] () -> Ty { return static_cast<Ty>(e.run_expression()); } };
        }

        Storage storage;
    };

    template<class Ty, class Storage>
    struct expr : expr_base<Ty, Storage> {
        using type = Ty;
        using value_type = std::decay_t<Ty>;
        using expr_base<Ty, Storage>::expr_base;
        using expr_base<Ty, Storage>::operator=;
    };

    template<class ContainerSyntax, class VarAliases, class ...LinkedContainers>
    struct list_comprehension;

    template<class Ty>
    struct var_alias;

    template<class Ty> requires (!std::is_reference_v<Ty>)
    struct var : expr<Ty&, var_storage<Ty>> {
        using type = Ty&;
        using value_type = Ty;
        using expr<Ty&, var_storage<Ty>>::expr;

        var(var&& other) { this->storage = std::move(other.storage); }
        var(const var& other) { this->storage = other.storage; }

        var& operator=(var&& other) { this->storage = std::move(other.storage); return *this; }
        var& operator=(var& other) { this->storage = other.storage; return *this; }
        var& operator=(const var& other) { this->storage = other.storage; return *this; }

        template<class Args>
        var& operator=(Args&& args) {
            this->storage = std::forward<Args>(args);
            return *this;
        }

        /**
         * Assignment operator for creating a name alias for the result of a 
         * list comprehension object.
         */
        template<class ContainerSyntax, class ...LinkedContainers> requires std::same_as<typename ContainerSyntax::container, Ty>
        auto operator=(list_comprehension<ContainerSyntax, LinkedContainers...>&& l) {
            l.name_alias.storage = this->storage;
            return std::move(l);
        }

        /**
         * Operator for creating intermediate variable assignments in a list
         * comprehension object.
         * @return the variable alias object that connects the given expression to this variable
         */
        var_alias<Ty> operator<<=(expr<Ty> e) const {
            return { e, *this };
        }
    };

    /**
     * Variable alias object that is used for intermediate variable assignments
     * in a list comprehension object.
     */
    template<class Ty>
    struct var_alias {
        expr<Ty> e;
        var<Ty> v;
    };

    template<class Type>
    expr(Type)->expr<decltype(std::declval<std::decay_t<Type>>()())>;

    template<class Type>
    expr(expr<Type>)->expr<Type>;

    template<class Type>
    var(Type)->var<Type>;

    /**
     * Wrapper for a container, used when creating a cartesian product, works
     * with any class that defines a begin and end method.
     */
    template<class Type, has_begin_end Container>
    struct container {
        using type = Type;
        Container container;

        inline auto begin() { return container.begin(); }
        inline auto end() { return container.end(); }
        inline auto size() const { return container.size(); }
    };

    /**
     * A struct that links a var to a container, this is stored in the final
     * link comprehension object.
     */
    template<class Type, has_begin_end Container>
    struct linked_container {
        using type = typename Type::type;
        using iterator = decltype(std::declval<Container>().begin());

        constexpr static bool has_var = false;

        Type variable;
        Container container;

        iterator begin() { return container.begin(); }
        iterator end() { return container.end(); }
        size_t size() const { return container.size(); }
    };

    /**
     * Specialization for storing a variable that results in a container.
     * So we can get the actual result of the variable by calling run_expression()
     */
    template<class Type, has_begin_end VarType>
    struct linked_container<Type, container<typename VarType::value_type, var<VarType>>> {
        using type = typename Type::type;
        using iterator = decltype(std::declval<VarType>().begin());

        constexpr static bool has_var = true;

        Type variable;
        container<typename VarType::value_type, var<VarType>> container;

        iterator begin() { return container.container.run_expression().begin(); }
        iterator end() { return container.container.run_expression().end(); }
        size_t size() const { return container.container.run_expression().size(); }
    };

    /**
     * This is the part before the '|', and defines what type of container the
     * result will be stored in. Also contains a generate method to easily generate an entry
     * for the resulting container given the values currently in the variables.
     */
    template<class Container, class ...Types>
    struct container_syntax {
        using container = Container;

        std::tuple<expr<Types>...> expressions;

        /**
         * Generate the next entry directly into the given container
         * @param container container to generate entry into.
         */
        template<class T>
        inline void generate(T& container) { return m_Gen(container, std::make_index_sequence<sizeof...(Types)>{}); }

    private:
        template<class T, size_t ...Is>
        inline void m_Gen(T& container, std::index_sequence<Is...>) {
            if constexpr (std::same_as<std::string, T>)
                container.push_back(std::get<Is>(expressions).run_expression()...);
            else if constexpr (has_emplace_back<T, typename expr<Types>::type...>)
                container.emplace_back(std::get<Is>(expressions).run_expression()...);
            else if constexpr (has_try_emplace<T, typename expr<Types>::type...>)
                container.try_emplace(std::get<Is>(expressions).run_expression()...);
            else
                container.emplace(std::get<Is>(expressions).run_expression()...);
        }
    };

    /**
     * Everything combined into a single object, contains the container syntax, all
     * the linked containers, and also some constraints, which are expressions that evaluate to 'bool'
     */
    template<class ContainerSyntax, class VarAliases, class ...LinkedContainers>
    struct list_comprehension {
        using container = typename ContainerSyntax::container;
        using value_type = container::value_type;
        using reference = container::reference;
        using size_type = container::size_type;

        constexpr static auto container_count = sizeof...(LinkedContainers);
        constexpr static auto alias_count = std::tuple_size_v<VarAliases>;
        constexpr static auto sequence = std::make_index_sequence<container_count>{};
        constexpr static auto alias_sequence = std::make_index_sequence<alias_count>{};
        constexpr static bool has_var_as_container = (LinkedContainers::has_var || ...);
        
        ContainerSyntax syntax;
        std::tuple<LinkedContainers...> containers;
        std::vector<expr<bool>> constraints;
        std::vector<expr<bool>> breakpoints;
        VarAliases aliases;
        var<container> name_alias;

        container get() { return this->m_Get(0); }
        container take(std::size_t n) { return this->m_Get(n); }

        expr<container> get_as_expr() { return expr{ [cpy = std::move(*this)] () mutable { return cpy.m_Get(0); } }; }
        
        /**
         * Checks if it generates at most n elements, immediately stops 
         * and returns false if it exceeds n.
         * @param n
         * @return expression that evaluates to true if size is below n
         */
        expr<bool> max_size(size_t n) { return expr{ [n, cpy = std::move(*this)] () mutable -> bool { return cpy.m_Get(n + 1).size() == n; } }; }

    private:
        inline container m_Get(std::size_t n) {
            container& result = name_alias.run_expression();

            // Some std containers don't have 'clear', like queue, and priority queue
            // Then we will clear using assignment to a new container;
            if constexpr (has_clear<container>) result.clear();
            else result = {};

            bool needs_reset[sizeof...(LinkedContainers)];
            std::fill(std::begin(needs_reset), std::end(needs_reset), false);

            // Check whether container has any content 
            if (std::get<0>(containers).size() == 0)
                return result;

            std::tuple<LinkedContainers::iterator...> its;
            std::tuple<LinkedContainers::iterator...> ends;
            set_begin(its, sequence); // Initialize all iterators to begin
            
            int index = container_count - 1;
            bool done = false;
            while (!done) { // This will loop through all the values in the cartesian product of the linked containers.
                set_values(its, sequence); // Set all vars to the values that the iterators point to.
                set_end(ends, sequence);

                // Assign variable aliases before checking constraints/breakpoints
                set_aliases(aliases, alias_sequence);

                // Check for breakpoint expressions
                bool _break = false;
                for (auto& b : breakpoints)
                    if (b.run_expression()) {
                        _break = true;
                        break;
                    }

                if (_break)
                    break;

                // Check if all constraints match.
                bool _match = true;
                for (auto& c : constraints) 
                    if (!c.run_expression()) {
                        _match = false;
                        break;
                    }

                if (_match) { // If all matched, generate an entry, and add to result.
                    syntax.generate(result);
                    if (n && result.size() == n)
                        break;
                }

                if (!check_end(its, ends, index, sequence))
                    increment(its, index, sequence); // Increment the iterator
                while (check_end(its, ends, index, sequence)) { // And check if it's now at the end.
                    set_begin(its, index, sequence); // Reset the iterator
                    index--;                         // And go to the next index to increment that one.
                    if (index == -1) {               // If we're at the end, we're done.
                        done = true;
                        break;
                    }

                    if (!check_end(its, ends, index, sequence)) {
                        increment(its, index, sequence); // Otherwise increment the iterator and loop to check if also at the end.
                        
                        needs_reset[index + 1] = true;
                    }
                }
                if constexpr (has_var_as_container) {
                    for (int i = 0; i < sizeof...(LinkedContainers); i++) {
                        if (needs_reset[i]) {
                            set_values(its, sequence);
                            set_begin(its, i, sequence);
                            needs_reset[i] = false;
                        }
                    }
                }
                index = container_count - 1; // Reset index back to 0 for the next iteration.
            }

            return result;
        }

        /**
         * Some helper functions because dealing with tuples
         * is a living nightmare when also working with changing indices...
         */
        template<class T, std::size_t ...Is>
        inline void set_begin(T& tuple, std::index_sequence<Is...>) {
            ((void(std::get<Is>(tuple) = std::move(std::get<Is>(containers).begin())),
                has_var_as_container && std::get<Is>(containers).size() ? (std::get<Is>(containers).variable = *std::get<Is>(containers).begin(), true) : true),
                ...);
        }

        template<class T, std::size_t ...Is>
        inline void set_end(T& tuple, std::index_sequence<Is...>) {
            ((void(std::get<Is>(tuple) = std::move(std::get<Is>(containers).end()))), ...);
        }

        template<class T, std::size_t ...Is>
        inline void set_aliases(T& tuple, std::index_sequence<Is...>) {
            ((void(std::get<Is>(tuple).v = std::get<Is>(tuple).e.run_expression())), ...);
        }

        template<class T, std::size_t ...Is>
        inline void set_begin(T& tuple, std::size_t i, std::index_sequence<Is...>) {
            ((Is == i ? (std::get<Is>(tuple) = std::move(std::get<Is>(containers).begin()), true) : true), ...);
        }

        template<class T, std::size_t ...Is>
        inline bool check_end(T& tuple, T& ends, size_t i, std::index_sequence<Is...>) {
            bool is_end = false;
            ((Is == i ? (is_end = std::get<Is>(tuple) == std::get<Is>(ends), true) : true), ...);
            return is_end;
        }

        template<class T, std::size_t ...Is>
        inline void set_values(T& tuple, std::index_sequence<Is...>) {
            (void(std::get<Is>(containers).variable = *std::get<Is>(tuple)), ...);
        }

        template<class T, std::size_t ...Is>
        inline void increment(T& tuple, std::size_t index, std::index_sequence<Is...>) {
            ((Is == index ? (++std::get<Is>(tuple), true) : true), ...);
        }
    };

    /**
     * Some sugar on top that allows syntax like
     * 'lc[a | a <- range(0, 10)]' for 'true' list comprehension.
     * lcv is for generating an expression out of the list comprehension
     * for use inside other list comprehensions, and lcl is for lazy
     * evaluation, basically, doesn't evaluate the list comprehension, which
     * allows a call to list_comprehension::take(size_t), where you can lazily get
     * the first n results.
     */
    struct lce {
        template<class ContainerSyntax, class ...LinkedContainers>
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return std::move(l).get(); };
    } lc;

    struct lcev {
        template<class ContainerSyntax, class ...LinkedContainers>
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return std::move(l).get_as_expr(); };
    } lcv;
    
    struct lcel {
        template<class ContainerSyntax, class ...LinkedContainers>
        auto operator[](list_comprehension<ContainerSyntax, LinkedContainers...>&& l) { return std::move(l); };
    } lcl;

    /*
     * 
     *  Some handy utilities for more complex list comprehension.
     * 
     */

    /**
     * Simple tuple of vars, with operator overloads to be compatible.
     * with the list comprehension object.
     */
    template<class ...Args>
    struct tuple_of_vars {
        using type = std::tuple<Args...>;
        std::tuple<var<Args>...> vars;

        void operator=(type&& a) { vars = a; }
        void operator=(const type& a) { vars = a; }
    };

    /**
     * Basically a wrapper around a string_view to be compatible
     * with the list comprehension.
     */
    struct literalview {
        using value_type = char;
        using reference = char&;
        using const_reference = const char&;
        using iterator = char*;
        using const_iterator = const char*;
        using difference_type = std::ptrdiff_t;
        using size_type = size_t;

        literalview(const char* d) : data(d) {}
       
        iterator begin() { return const_cast<char*>(data.data()); }
        iterator end() { return const_cast<char*>(data.data() + data.size()); }
        const_iterator begin() const { return data.data(); }
        const_iterator end() const { return data.data() + data.size(); }
        const_iterator cbegin() const { return data.data(); }
        const_iterator cend() const { return data.data() + data.size(); }
        size_type size() const { return data.size(); }
    
    private: 
        std::string_view data;
    };

    /**
     * Used for parallel iteration of containers. Stores multiple containers (value or reference)
     * and defines an iterator that returns a tuple of references to the values.
     */
    template<has_begin_end... Containers>
    struct tuple_of_containers {
        constexpr static auto seq = std::make_index_sequence<sizeof...(Containers)>{};

        // Value type is a tuple of all the results of the iterator, this
        // ensures that if a container iterator returns a tuple, all of the types are added to this tuple.
        using value_type = decltype(std::tuple_cat(
            std::tuple{ *std::declval<typename std::decay_t<Containers>::iterator>() }...));

        // Simple iterator, the '==' operator checks whether 1 of the iterators
        // is equal, this ensures it will stop iterating once one of the iterators reaches the end.
        template<bool Const>
        struct toc_iterator {
            using iterator_category = std::forward_iterator_tag;
            using value_type = tuple_of_containers::value_type;
            using difference_type = std::ptrdiff_t;
            using pointer = value_type*;
            using reference = value_type&;

            toc_iterator& operator++() { return increment_r(seq); }
            toc_iterator operator++(int) { return increment(seq); }
            value_type operator*() { return get(seq); }
            bool operator==(const toc_iterator& o) const { return equal(o, seq); }
            std::tuple<std::conditional_t<Const, typename std::decay_t<Containers>::const_iterator, typename std::decay_t<Containers>::iterator>...> iterators;

        private:
            template<size_t ...Is>
            value_type get(std::index_sequence<Is...>) { 
                return { std::tuple_cat(std::tuple{ *std::get<Is>(iterators) }...) };
            }

            template<size_t ...Is>
            toc_iterator& increment_r(std::index_sequence<Is...>) {
                ((++std::get<Is>(iterators)), ...);
                return *this;
            }

            template<size_t ...Is>
            toc_iterator& increment(std::index_sequence<Is...>) {
                ((std::get<Is>(iterators)++), ...);
                return *this;
            }

            template<size_t ...Is>
            bool equal(const toc_iterator& other, std::index_sequence<Is...>) const {
                return ((std::get<Is>(iterators) == std::get<Is>(other.iterators)) || ...);
            }
        };

        using iterator = toc_iterator<false>;
        using const_iterator = toc_iterator<true>;
        using reference = value_type&;
        using const_reference = const value_type&;
        using difference_type = std::iterator_traits<iterator>::difference_type;
        using size_type = std::size_t;

        std::tuple<Containers...> containers;

        iterator begin() { return seq_begin(seq); };
        iterator end() { return seq_end(seq); };
        const_iterator begin() const { return seq_begin(seq); };
        const_iterator end() const { return seq_end(seq); };
        const_iterator cbegin() const { return seq_begin(seq); };
        const_iterator cend() const { return seq_end(seq); };
        size_type size() const { return seq_size(seq); }
        size_type max_size() const { return seq_max_size(seq); }
        bool empty() const { return empty(seq); }

        bool operator==(const tuple_of_containers& o) const { return equal(o, seq); }
        bool operator!=(const tuple_of_containers& o) const { return !equal(o, seq); }
        void swap(tuple_of_containers& o) { o.containers.swap(containers); }

    private:
        template<size_t ...Is>
        size_type seq_size(std::index_sequence<Is...>) const {
            size_t _size = std::get<0>(containers).size();
            ((std::get<Is>(containers).size() < _size ? (_size = std::get<Is>(containers).size(), false) : false), ...);
            return _size;
        }
        template<size_t ...Is>
        size_type seq_max_size(std::index_sequence<Is...>) const {
            size_t _size = std::get<0>(containers).max_size();
            ((std::get<Is>(containers).max_size() < _size ? (_size = std::get<Is>(containers).max_size(), false) : false), ...);
            return _size;
        }
        template<size_t ...Is>
        iterator seq_begin(std::index_sequence<Is...>) { return { { std::get<Is>(containers).begin()... } }; }
        template<size_t ...Is>
        iterator seq_end(std::index_sequence<Is...>) { return { { std::get<Is>(containers).end()... } }; }
        template<size_t ...Is>
        const_iterator seq_begin(std::index_sequence<Is...>) const { return { { std::get<Is>(containers).begin()... } }; }
        template<size_t ...Is>
        const_iterator seq_end(std::index_sequence<Is...>) const { return { { std::get<Is>(containers).end()... } }; }
        template<size_t ...Is>
        bool equal(const tuple_of_containers& other, std::index_sequence<Is...>) const { return ((std::get<Is>(containers) == std::get<Is>(other.containers)) || ...); }
        template<size_t ...Is>
        bool empty(std::index_sequence<Is...>) const { return ((std::get<Is>(containers).empty()) || ...); }
    };

    /**
     * Infinity value for usage in range.
     */
    struct infinity {
        template<class T> requires (std::floating_point<T> || std::integral<T>)
            operator T() { return std::numeric_limits<T>::max(); }
    } inf;

    /**
     * Simple range class with a start and end, plus an iterator because
     * list comprehension uses iterators to create the cartesian product of all sets.
     * requires start <= end
     */
    template<class Type>
    concept can_range = requires(Type t) {
        { ++t } -> std::convertible_to<Type>;
        { t + 1 } -> std::convertible_to<Type>;
    };
    template<can_range Type>
    struct range {
        struct iterator {
            using iterator_category = std::forward_iterator_tag;
            using difference_type = std::ptrdiff_t;
            using value_type = Type;
            using reference = Type&;
            using const_reference = const Type&;

            iterator& operator++() { ++cur; return *this; }
            iterator operator++(int) { return { cur + 1 }; }
            Type& operator*() { return cur; }
            bool operator==(const iterator& o) const { return o.cur == cur; }
            Type cur;
        };

        using const_iterator = iterator;
        using value_type = Type;
        using reference = Type&;
        using const_reference = const Type&;
        using difference_type = std::iterator_traits<iterator>::difference_type;
        using size_type = std::size_t;

        range(Type&& x, Type&& y) { init(a, std::move(x)); init(b, std::move(y)); }
        range(Type& x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(Type&& x, Type& y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, Type& y) { init(a, x); init(b, y); }
        range(Type&& x, expr<Type> y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, expr<Type> y) { init(a, x); init(b, y); }
        range(Type&& x, var<Type> y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, var<Type> y) { init(a, x); init(b, y); }
        range(var<Type> x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(expr<Type> x, Type&& y) { init(a, x); init(b, std::move(y)); }
        range(var<Type> x, Type& y) { init(a, x); init(b, y); }
        range(expr<Type> x, Type& y) { init(a, x); init(b, y); }
        range(var<Type> x, var<Type> y) { init(a, x); init(b, y); }
        range(expr<Type> x, var<Type> y) { init(a, x); init(b, y); }
        range(var<Type> x, expr<Type> y) { init(a, x); init(b, y); }
        range(expr<Type> x, expr<Type> y) { init(a, x); init(b, y); }
        range(Type&& x, infinity& y) { init(a, std::move(x)); init(b, y); }
        range(Type& x, infinity& y) { init(a, x); init(b, y); }
        range(var<Type> x, infinity& y) { init(a, x); init(b, y); }
        range(expr<Type> x, infinity& y) { init(a, x); init(b, y); }
        
        range(const range& o) : a(o.a), b(o.b) {};
        range(range&& o) : a(o.a), b(o.b) {};
        range& operator=(const range& o) { a.storage = o.a.storage, b.storage = o.b.storage; return *this; };
        range& operator=(range&& o) { a.storage = o.a.storage, b.storage = o.b.storage; return *this; };

        iterator begin() { return { a.run_expression() }; }
        iterator end() { return { b.run_expression() }; }
        const_iterator begin() const { return { a.run_expression() }; }
        const_iterator end() const { return { b.run_expression() }; }
        const_iterator cbegin() const { return begin(); }
        const_iterator cend() const { return end(); }
        size_type size() const { auto res = b.run_expression() - a.run_expression(); return res < 0 ? 0 : res; }
        bool empty() const { return b.run_expression() == a.run_expression(); }

        bool operator==(const range& o) const { return o.a == a && o.b == b; }

    private:
        expr<Type> a, b;

        void init(expr<Type>& a, infinity& t) { a = expr{ [&]() { return (Type)t; } }; }
        void init(expr<Type>& a, var<Type> t) { a = expr{ [t]() { return t.run_expression(); } }; }
        void init(expr<Type>& a, expr<Type> t) { a = t; }
        void init(expr<Type>& a, Type&& t) { a = expr{ [t = std::move(t)] () { return t; } }; }
        void init(expr<Type>& a, const Type& t) { a = expr{ [t = t]() { return t; } }; }
    };

    template<class Type, std::convertible_to<Type> T2>
    range(Type, T2)->range<Type>;

    /**
     * All the operators that make the magic happen.
     */
    namespace lc_operators {

        /**
         * Normal operators between vars, exprs, and type
         */
#define var_op(x)\
    template<class A, class B> auto operator x(var<A> a, var<B> b) { return expr{ [a, b]() mutable { return a.run_expression() x b.run_expression(); } }; } \
    template<class A, class B> auto operator x(expr<A> a, var<B> b) { return expr{ [a, b]() mutable { return a.run_expression() x b.run_expression(); } }; } \
    template<class A, class B> auto operator x(var<A> a, expr<B> b) { return expr{ [a, b]() mutable { return a.run_expression() x b.run_expression(); } }; } \
    template<class A, class B> auto operator x(expr<A> a, expr<B> b) { return expr{ [a, b]() mutable { return a.run_expression() x b.run_expression(); } }; } \
    template<class A, class B> auto operator x(expr<A> a, const B& b) { return expr{ [a, b]() mutable { return a.run_expression() x b; } }; } \
    template<class A, class B> auto operator x(var<A> a, const B& b) { return expr{ [a, b]() mutable { return a.run_expression() x b; } }; } \
    template<class A, class B> auto operator x(const A& a, expr<B> b) { return expr{ [a, b]() mutable { return a x b.run_expression(); } }; } \
    template<class A, class B> auto operator x(const A& a, var<B> b) { return expr{ [a, b]() mutable { return a x b.run_expression(); } }; }

        var_op(+);
        var_op(-);
        var_op(/ );
        var_op(*);
        var_op(%);
        var_op(== );
        var_op(!= );
        var_op(<= );
        var_op(>= );
        var_op(> );
        var_op(< );
        var_op(<=> );
        var_op(&&);
        var_op(&);
        var_op(|| );
        var_op(| );
        var_op(<< );
        var_op(>> );

#define u_var_op(x)\
    template<class A> auto operator x(var<A> a) { return expr{ [a]() mutable { return x a.run_expression(); } }; } \
    template<class A> auto operator x(expr<A> a) { return expr{ [a]() mutable { return x a.run_expression(); } }; } \

        u_var_op(-);
        u_var_op(+);
        u_var_op(~);
        u_var_op(!);
        u_var_op(*);
        u_var_op(&);

        /**
         * This user-defined literal is necessary because you cannot overload the unary '-'
         * operator on a string literal.
         */
        literalview operator""_lv(const char* d, size_t) {
            return literalview{ d };
        }

        /**
         * Operators for constructing tuple of vars.
         */
        template<class A, class B>
        tuple_of_vars<A, B> operator,(var<A> a, var<B> b) { return { { a, b } }; }

        template<class A, class ...Args>
        tuple_of_vars<Args..., A> operator,(tuple_of_vars<Args...> a, var<A> b) { return { std::tuple_cat(a.vars, std::tuple{ b }) }; }

        template<class A, class ...Args>
        tuple_of_vars<A, Args...> operator,(var<A> b, tuple_of_vars<Args...> a) { return { std::tuple_cat(std::tuple{ b }, a.vars) }; }

        template<class ...As, class ...Args>
        tuple_of_vars<As..., Args...> operator,(tuple_of_vars<As...> b, tuple_of_vars<Args...> a) { return { std::tuple_cat(b.vars, a.vars) }; }

        /**
         * the '-' and '<' operators are used to create a '<-' operator, the unary '-'
         * creates a container from some class that defines a begin() and end(), and the '<'
         * links that container to a variable.
         */
        template<has_begin_end Container>
        container<typename Container::value_type, Container&> operator-(Container& r) { return { r }; }

        template<has_begin_end Container>
        container<typename Container::value_type, Container> operator-(Container&& r) { return { std::move(r) }; }

        template<has_begin_end Type>
        container<typename Type::value_type, var<Type>> operator-(var<Type> r) { return { r }; }

        template<class Type, class CType, class Container>
        linked_container<var<Type>, container<CType, Container>> 
            operator<(var<Type> v, container<CType, Container>&& r) { 
            return { v, std::move(r) }; 
        }

        template<class CType, class Container, class ...Args>
        linked_container<tuple_of_vars<Args...>, container<CType, Container>> 
            operator<(tuple_of_vars<Args...>&& v, container<CType, Container>&& r) { 
            return { v, std::move(r) }; 
        }

        /**
         * Operators and functions for constructing a container syntax.
         * Eiter a comma operator to use standard vector, or a function to choose
         * the container.
         */
        template<class A, class B>
        container_syntax<std::vector<std::tuple<A, B>>, A, B&> operator,(expr<A> a, var<B> b) { return { { a, b } }; }

        template<class A, class B>
        container_syntax<std::vector<std::tuple<A, B>>, A&, B> operator,(var<A> a, expr<B> b) { return { { a, b } }; }

        template<class A, class B>
        container_syntax<std::vector<std::tuple<A, B>>, A, B> operator,(expr<A> a, expr<B> b) { return { { a, b } }; }

        template<class A, class ...Rest>
        container_syntax<std::vector<std::tuple<Rest..., A>>, Rest&..., A> 
            operator,(tuple_of_vars<Rest...>&& a, expr<A> b) {
            return { std::tuple_cat(a.vars, std::tuple{ b }) };
        }

        template<class A, class ...Rest>
        container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> 
            operator,(container_syntax<std::vector<std::tuple<Rest...>>, Rest...>&& a, var<A> b) {
            return { std::tuple_cat(a.expressions, std::tuple{ b }) };
        }

        template<class A, class ...Rest>
        container_syntax<std::vector<std::tuple<Rest..., A>>, Rest..., A> 
            operator,(container_syntax<std::vector<std::tuple<Rest...>>, Rest...>&& a, expr<A> b) {
            return { std::tuple_cat(a.expressions, std::tuple{ b }) };
        }

        template<template<class...> class Container, class ...Types>
        using container_syntax_type = container_syntax<
            Container<std::conditional_t<sizeof...(Types) == 1, 
                std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::value_type...>>,
                std::tuple<typename std::decay_t<Types>::value_type...>>>, 
            typename std::decay_t<Types>::type...>;

        template<template<class...> class Container, class Ty, class ...Types>
        using container_syntax_type2 = container_syntax<
            Container<typename std::decay_t<Ty>::value_type, 
                std::conditional_t<sizeof...(Types) == 1,
                    std::tuple_element_t<0, std::tuple<typename std::decay_t<Types>::value_type...>>, 
                    std::tuple<typename std::decay_t<Types>::value_type...>>>,
            typename std::decay_t<Ty>::type, typename std::decay_t<Types>::type...>;

        template<class ...Types>
        container_syntax_type<std::vector, Types...> vector(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::list, Types...> list(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::deque, Types...> deque(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::stack, Types...> stack(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::queue, Types...> queue(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::priority_queue, Types...> priority_queue(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::set, Types...> set(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::multiset, Types...> multiset(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::unordered_set, Types...> unordered_set(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class ...Types>
        container_syntax_type<std::unordered_multiset, Types...> unordered_multiset(Types ...tys) {
            return { std::tuple{ (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class Ty, class ...Types>
        container_syntax_type2<std::map, Ty, Types...> map(Ty ty, Types ...tys) {
            return { { (expr<typename std::decay_t<Ty>::type>)ty, (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class Ty, class ...Types>
        container_syntax_type2<std::multimap, Ty, Types...> multimap(Ty ty, Types ...tys) {
            return { { (expr<typename std::decay_t<Ty>::type>)ty, (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class Ty, class ...Types>
        container_syntax_type2<std::unordered_map, Ty, Types...> unordered_map(Ty ty, Types ...tys) {
            return { {(expr<typename std::decay_t<Ty>::type>)ty, (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        template<class Ty, class ...Types>
        container_syntax_type2<std::unordered_multimap, Ty, Types...> unordered_multimap(Ty ty, Types ...tys) {
            return { { (expr<typename std::decay_t<Ty>::type>)ty, (expr<typename std::decay_t<Types>::type>)tys... } };
        }

        /**
         * Operators for initializing a list comprehension object with a container syntax and the first linked container.
         */
        template<class ContainerSyntax, class Container, class CType>
        list_comprehension<ContainerSyntax, std::tuple<>, linked_container<CType, Container>>
            operator|(const ContainerSyntax& v, linked_container<CType, Container>&& c) {
            return { v, linked_container<CType, Container>{ std::move(c) }, {} };
        }

        template<class Type, class Container, class CType>
        list_comprehension<container_syntax<container_for<std::decay_t<Type>>, Type&>, std::tuple<>, linked_container<CType, Container>>
            operator|(var<Type> v, linked_container<CType, Container>&& c) {
            return { container_syntax<container_for<std::decay_t<Type>>, Type&>{ v }, std::move(c), {} };
        }

        template<class Type, class Container, class CType>
        list_comprehension<container_syntax<container_for<std::decay_t<Type>>, Type>, std::tuple<>, linked_container<CType, Container>>
            operator|(expr<Type> v, linked_container<CType, Container>&& c) {
            return { container_syntax<container_for<std::decay_t<Type>>, Type>{ v }, std::move(c), {} };
        }

        template<class Container, class CType, class...Args>
        list_comprehension<container_syntax<std::vector<std::tuple<Args...>>, Args&...>, std::tuple<>, linked_container<CType, Container>>
            operator|(tuple_of_vars<Args...>&& v, linked_container<CType, Container>&& c) {
            return { container_syntax<std::vector<std::tuple<Args...>>, Args&...>{ v.vars }, std::move(c), {} };
        }

        /**
         * Operators for expanding the initial list comprehension with
         * more linked containers or constraints.
         */
        template<class ContainerSyntax, class VarAliasses, class Container, class CType, class ...LinkedContainers>
        list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers..., linked_container<CType, Container>>
            operator,(list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>&& v, linked_container<CType, Container>&& c) {
            return { std::move(v.syntax), std::tuple_cat(std::move(v.containers), std::tuple{ c }), 
                std::move(v.constraints), std::move(v.breakpoints), std::move(v.aliases), std::move(v.name_alias) };
        }

        template<class ContainerSyntax, class VarAliasses, class ...LinkedContainers>
        list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>
            operator,(list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>&& v, expr<bool> c) {
            v.constraints.push_back(c);
            return std::move(v);
        }

        template<std::convertible_to<bool> Type, class ContainerSyntax, class VarAliasses, class ...LinkedContainers>
        list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>
            operator,(list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>&& v, expr<Type> c) {
            v.constraints.push_back({ [c = std::move(c)] () { return static_cast<bool>(c.run_expression()); } });
            return std::move(v);
        }

        /**
         * Used in the syntax for a breaking condition
         */
        struct breakpoint {
            expr<bool> condition;
            breakpoint& operator<<=(expr<bool> e) {
                condition = e;
                return *this;
            }
        } brk;

        template<class ContainerSyntax, class VarAliasses, class ...LinkedContainers>
        list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>
            operator,(list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>&& v, breakpoint& b) {
            v.breakpoints.push_back(b.condition);
            return std::move(v);
        }

        /**
         * Operators for var aliasses
         */

        template<class Ty, class ContainerSyntax, class VarAliasses, class ...LinkedContainers>
        list_comprehension<ContainerSyntax, decltype(std::tuple_cat(std::declval<VarAliasses>(), std::declval<std::tuple<var_alias<Ty>>>())), LinkedContainers...>
            operator,(list_comprehension<ContainerSyntax, VarAliasses, LinkedContainers...>&& v, var_alias<Ty> b) {
            return { std::move(v.syntax), std::move(v.containers), std::move(v.constraints), 
                std::move(v.breakpoints), std::tuple_cat(v.aliases, std::tuple{ b }), v.name_alias };
        }

        /**
         * Operators to construct tuple of containers.
         */
        template<has_begin_end A, has_begin_end B>
        tuple_of_containers<A, B> operator,(A&& a, B&& b) {
            return { { { std::forward<A>(a) }, { std::forward<B>(b) } } };
        }

        template<has_begin_end B>
        tuple_of_containers<literalview, B> operator,(const char* a, B&& b) {
            return { { literalview{ a }, { std::forward<B>(b) } } };
        }

        template<has_begin_end A>
        tuple_of_containers<A, literalview> operator,(A&& a, const char* b) {
            return { { { std::forward<A>(a) }, literalview{ b } } };
        }

        template<has_begin_end A, has_begin_end ...Rest>
        tuple_of_containers<Rest..., A> operator,(tuple_of_containers<Rest...>&& a, A&& b) {
            return { std::tuple_cat(a.containers, std::tuple<A>{ std::forward<A>(b) }) };
        }

        template<has_begin_end ...Rest>
        tuple_of_containers<Rest..., literalview> operator,(tuple_of_containers<Rest...>&& a, const char* b) {
            return { std::tuple_cat(a.containers, std::tuple{ literalview{ b } }) };
        }

        template<has_begin_end A, has_begin_end ...Rest>
        tuple_of_containers<A, Rest...> operator,(A&& b, tuple_of_containers<Rest...>&& a) {
            return { std::tuple_cat(std::tuple<A>{ std::forward<A>(b) }, a.containers) };
        }

        template<has_begin_end ...Rest>
        tuple_of_containers<literalview, Rest...> operator,(const char* b, tuple_of_containers<Rest...>&& a) {
            return { std::tuple_cat(std::tuple{ literalview{ b } }, a.containers) };
        }

        template<has_begin_end ...As, has_begin_end ...Rest>
        tuple_of_containers<Rest..., As...> operator,(tuple_of_containers<Rest...>&& a, tuple_of_containers<As...>&& b) {
            return { std::tuple_cat(a.containers,  b.containers) };
        }
    }

    /**
     * Expression wrapper is used to simplify templated functions by generalizing how
     * to get the value, for an expression that requires run_expression().
     */
    template<class Type>
    struct expr_wrapper;
    template<class Type> requires std::derived_from<std::decay_t<Type>, is_an_expr>
    struct expr_wrapper<Type> {
        std::decay_t<Type> value;
        inline typename std::decay_t<Type>::type get() {
            return value.run_expression();
        }
    };

    template<class Type> requires (!std::derived_from<std::decay_t<Type>, is_an_expr>)
        struct expr_wrapper<Type> {
        Type value;
        inline Type get() {
            return value;
        }
    };

    /**
     * Specializations of the expression class for standard classes
     * and containers that contain all their member functions.
     */
#define lc_mem_fun(y, x) \
    template<class ...Args> auto x(Args&& ...exprs) const { return kaixo::expr{ [s = *this, ...args = expr_wrapper<Args>{ \
        std::forward<Args>(exprs) }]() mutable -> decltype(std::declval<y>().x(std::declval<expr_wrapper<Args>>().get()...)) { \
            return s.run_expression().y::x(args.get()...); } }; } \

    template<class String, class Storage = expr_storage<String>>
    struct string_expression : expr_base<String, Storage> {
        using type = std::decay_t<String>;
        using expr_base<String, Storage>::expr_base;
        using expr_base<String, Storage>::operator=;

        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, data);
        lc_mem_fun(type, c_str);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, length);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, reserve);
        lc_mem_fun(type, capacity);
        lc_mem_fun(type, shrink_to_fit);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, push_back);
        lc_mem_fun(type, pop_back);
        lc_mem_fun(type, append);
        lc_mem_fun(type, operator+=);
        lc_mem_fun(type, compare);
        lc_mem_fun(type, starts_with);
        lc_mem_fun(type, ends_with);
        lc_mem_fun(type, replace);
        lc_mem_fun(type, substr);
        lc_mem_fun(type, copy);
        lc_mem_fun(type, resize);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, find);
        lc_mem_fun(type, rfind);
        lc_mem_fun(type, find_first_of);
        lc_mem_fun(type, find_first_not_of);
        lc_mem_fun(type, find_last_of);
        lc_mem_fun(type, find_last_not_of);
    };

    template<class Tuple, class Storage = expr_storage<Tuple>>
    struct tuple_expression : expr_base<Tuple, Storage> {
        using type = std::decay_t<Tuple>;
        using expr_base<Tuple, Storage>::expr_base;
        using expr_base<Tuple, Storage>::operator=;

        lc_mem_fun(type, swap);

        template<size_t N>
        expr<std::decay_t<decltype(std::get<N>(std::declval<type>()))>>get() {
            return { [c = *this] () { return std::get<N>(c()); } };
        }
    };

    template<class Vector, class Storage = expr_storage<Vector>>
    struct vector_expression : expr_base<Vector, Storage> {
        using type = std::decay_t<Vector>;
        using expr_base<Vector, Storage>::expr_base;
        using expr_base<Vector, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, assign);
        lc_mem_fun(type, get_allocator);
        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, data);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, reserve);
        lc_mem_fun(type, capacity);
        lc_mem_fun(type, shrink_to_fit);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, push_back);
        lc_mem_fun(type, emplace_back);
        lc_mem_fun(type, pop_back);
        lc_mem_fun(type, resize);
        lc_mem_fun(type, swap);
    };

    template<class List, class Storage = expr_storage<List>>
    struct list_expression : expr_base<List, Storage> {
        using type = std::decay_t<List>;
        using expr_base<List, Storage>::expr_base;
        using expr_base<List, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, assign);
        lc_mem_fun(type, get_allocator);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, push_back);
        lc_mem_fun(type, push_front);
        lc_mem_fun(type, emplace_back);
        lc_mem_fun(type, emplace_front);
        lc_mem_fun(type, pop_back);
        lc_mem_fun(type, pop_front);
        lc_mem_fun(type, resize);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, merge);
        lc_mem_fun(type, splice);
        lc_mem_fun(type, remove);
        lc_mem_fun(type, remove_if);
        lc_mem_fun(type, reverse);
        lc_mem_fun(type, unique);
        lc_mem_fun(type, sort);
    };

    template<class Array, class Storage = expr_storage<Array>>
    struct array_expression : expr_base<Array, Storage> {
        using type = std::decay_t<Array>;
        using expr_base<Array, Storage>::expr_base;
        using expr_base<Array, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, front);
        lc_mem_fun(type, back);
        lc_mem_fun(type, data);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, fill);
        lc_mem_fun(type, swap);
    };

    template<class Map, class Storage = expr_storage<Map>>
    struct map_expression : expr_base<Map, Storage> {
        using type = std::decay_t<Map>;
        using expr_base<Map, Storage>::expr_base;
        using expr_base<Map, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, get_allocator);
        lc_mem_fun(type, at);
        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, insert_or_assign);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, emplace_hint);
        lc_mem_fun(type, try_emplace);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, extract);
        lc_mem_fun(type, merge);
        lc_mem_fun(type, count);
        lc_mem_fun(type, find);
        lc_mem_fun(type, contains);
        lc_mem_fun(type, equal_range);
        lc_mem_fun(type, lower_bound);
        lc_mem_fun(type, upper_bound);
        lc_mem_fun(type, key_comp);
        lc_mem_fun(type, value_comp);
    };

    template<class Set, class Storage = expr_storage<Set>>
    struct set_expression : expr_base<Set, Storage> {
        using type = std::decay_t<Set>;
        using expr_base<Set, Storage>::expr_base;
        using expr_base<Set, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, get_allocator);
        lc_mem_fun(type, begin);
        lc_mem_fun(type, cbegin);
        lc_mem_fun(type, end);
        lc_mem_fun(type, cend);
        lc_mem_fun(type, rbegin);
        lc_mem_fun(type, crbegin);
        lc_mem_fun(type, rend);
        lc_mem_fun(type, crend);
        lc_mem_fun(type, empty);
        lc_mem_fun(type, size);
        lc_mem_fun(type, max_size);
        lc_mem_fun(type, clear);
        lc_mem_fun(type, insert);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, emplace_hint);
        lc_mem_fun(type, erase);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, extract);
        lc_mem_fun(type, merge);
        lc_mem_fun(type, count);
        lc_mem_fun(type, find);
        lc_mem_fun(type, contains);
        lc_mem_fun(type, equal_range);
        lc_mem_fun(type, lower_bound);
        lc_mem_fun(type, upper_bound);
        lc_mem_fun(type, key_comp);
        lc_mem_fun(type, value_comp);
    };

    template<class Any, class Storage = expr_storage<Any>>
    struct any_expression : expr_base<Any, Storage> {
        using type = std::decay_t<Any>;
        using expr_base<Any, Storage>::expr_base;
        using expr_base<Any, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, emplace);
        lc_mem_fun(type, reset);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, has_value);
    };
        
    template<class Bitset, class Storage = expr_storage<Bitset>>
    struct bitset_expression : expr_base<Bitset, Storage> {
        using type = std::decay_t<Bitset>;
        using expr_base<Bitset, Storage>::expr_base;
        using expr_base<Bitset, Storage>::operator=;

        lc_mem_fun(type, operator[]);
        lc_mem_fun(type, test);
        lc_mem_fun(type, all);
        lc_mem_fun(type, any);
        lc_mem_fun(type, none);
        lc_mem_fun(type, count);
        lc_mem_fun(type, size);
        lc_mem_fun(type, operator&=);
        lc_mem_fun(type, operator|=);
        lc_mem_fun(type, operator^=);
        lc_mem_fun(type, operator<<=);
        lc_mem_fun(type, operator>>=);
        lc_mem_fun(type, operator<<);
        lc_mem_fun(type, operator>>);
        lc_mem_fun(type, set);
        lc_mem_fun(type, reset);
        lc_mem_fun(type, flip);
        lc_mem_fun(type, to_string);
        lc_mem_fun(type, to_ulong);
        lc_mem_fun(type, to_ullong);
    };
                
    template<class Function, class Storage = expr_storage<Function>>
    struct function_expression : expr_base<Function, Storage> {
        using type = std::decay_t<Function>;
        using expr_base<Function, Storage>::expr_base;
        using expr_base<Function, Storage>::operator=;

        lc_mem_fun(type, operator=);
        lc_mem_fun(type, swap);
        lc_mem_fun(type, assign);
        lc_mem_fun(type, operator());
        lc_mem_fun(type, target_type);
        lc_mem_fun(type, target);
    };

    template<class Range, class Storage = expr_storage<Range>>
    struct range_expression : expr_base<Range, Storage> {
        using type = std::decay_t<Range>;
        using expr_base<Range, Storage>::expr_base;
        using expr_base<Range, Storage>::operator=;

        lc_mem_fun(type, begin);
        lc_mem_fun(type, end);
    };

#define COMMA ,
#define make_expr(cls, e, ...) \
    template<__VA_ARGS__ class Storage> struct expr<cls, Storage> : e<cls, Storage> { \
        using type = cls; using value_type = cls; using e<cls, Storage>::e; }; \
    template<__VA_ARGS__ class Storage> struct expr<cls&, Storage> : e<cls&, Storage> { \
        using type = cls&; using value_type = cls; using e<cls&, Storage>::e; }; \
    template<__VA_ARGS__ class Storage> struct expr<const cls&, Storage> : e<const cls&, Storage> { \
        using type = const cls&; using value_type = const cls; using e<const cls&, Storage>::e; }; \

    make_expr(std::string, string_expression);
    make_expr(std::tuple<Args...>, tuple_expression, class ...Args, );
    make_expr(std::vector<Args...>, vector_expression, class ...Args, );
    make_expr(std::list<Args...>, list_expression, class ...Args, );
    make_expr(std::set<Args...>, set_expression, class ...Args, );
    make_expr(std::array<Arg COMMA N>, array_expression, class Arg, size_t N, );
    make_expr(std::map<Args...>, map_expression, class ...Args, );
    make_expr(std::any, any_expression, );
    make_expr(std::bitset<N>, bitset_expression, size_t N, );
    make_expr(std::function<Args...>, function_expression, class ...Args, );
    make_expr(kaixo::range<Arg>, range_expression, class Arg, );
#undef COMMA
#undef make_expr
}

/**
 * Most std function definitions in 'expression' form.
 * to disable a portion of this, just define KAIXO_LC_header as something other than 1.
 */

#define lc_std_fun(y, x) \
    template<class ...Args> \
    kaixo::expr<typename to_val_or_ref<decltype(y x(std::declval<expr_wrapper<Args>>().get()...))>::type> \
        x(Args&& ...exprs) { return kaixo::expr{ [...args = expr_wrapper<Args>{ std::forward<Args>(exprs) }]() mutable -> \
        typename to_val_or_ref<decltype(y x(std::declval<expr_wrapper<Args>>().get()...))>::type { return y x(args.get()...); } }; } \

#ifndef KAIXO_LC_FUNCTIONAL
#define KAIXO_LC_FUNCTIONAL 1
#endif
#if KAIXO_LC_FUNCTIONAL == 1
#include <functional>
namespace kaixo {
    lc_std_fun(std::, bind_front);
    lc_std_fun(std::, bind);
    lc_std_fun(std::, ref);
    lc_std_fun(std::, cref);
    lc_std_fun(std::, invoke);
}
#endif

#ifndef KAIXO_LC_ANY
#define KAIXO_LC_ANY 1
#endif
#if KAIXO_LC_ANY == 1
#include <any>
namespace kaixo {
    lc_std_fun(std::, any_cast);
    lc_std_fun(std::, make_any);
}
#endif

#ifndef KAIXO_LC_ALGORITHMS
#define KAIXO_LC_ALGORITHMS 1
#endif
#if KAIXO_LC_ALGORITHMS == 1
#include <algorithm>
namespace kaixo {
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
#endif

#ifndef KAIXO_LC_ITERATOR
#define KAIXO_LC_ITERATOR 1
#endif
#if KAIXO_LC_ITERATOR == 1
#include <iterator>
namespace kaixo {
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
#endif

#ifndef KAIXO_LC_MEMORY
#define KAIXO_LC_MEMORY 1
#endif
#if KAIXO_LC_MEMORY == 1
#include <memory>
#include <memory_resource>
namespace kaixo {
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
#endif

#ifndef KAIXO_LC_NUMERIC
#define KAIXO_LC_NUMERIC 1
#endif
#if KAIXO_LC_NUMERIC == 1
#include <numeric>
namespace kaixo {
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
#endif

#undef lc_std_fun
#undef lc_mem_fun