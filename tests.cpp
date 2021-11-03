#include "list_comprehension.hpp"
#include <cassert>
#include <iostream>

using namespace kaixo;
using namespace kaixo::lc_operators;

namespace detail {
	template<class I>
	concept __Referenceable = !std::same_as<I, void>;

	template<class I>
	concept __LegacyIterator =
		requires(I i) {
			{   *i } -> __Referenceable;
			{  ++i } -> std::same_as<I&>;
			{ *i++ } -> __Referenceable;
	}&& std::copyable<I>;

	template<class I>
	concept __LegacyInputIterator =
		__LegacyIterator<I> && std::equality_comparable<I> && requires(I i) {
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
	concept __LegacyForwardIterator =
		__LegacyInputIterator<I> && std::constructible_from<I> &&
		std::is_lvalue_reference_v<std::iter_reference_t<I>> &&
		std::same_as<
		std::remove_cvref_t<std::iter_reference_t<I>>,
		typename std::indirectly_readable_traits<I>::value_type> &&
		requires(I i) {
			{  i++ } -> std::convertible_to<const I&>;
			{ *i++ } -> std::same_as<std::iter_reference_t<I>>;
	};

	template<class I>
	concept __LegacyBidirectionalIterator =
		__LegacyForwardIterator<I> && requires(I i) {
			{  --i } -> std::same_as<I&>;
			{  i-- } -> std::convertible_to<const I&>;
			{ *i-- } -> std::same_as<std::iter_reference_t<I>>;
	};
}

void containers() {
	static_assert(detail::__LegacyForwardIterator<range<int>::iterator>);

	tuple_of_containers<std::vector<int>, std::map<int, int>> a{ { std::vector{ 1, 3, 4 }, std::map<int, int>{ { 1, 2 }, { 2, 3 } } } };
	tuple_of_containers<std::vector<int>, std::map<int, int>> b;

	tuple_of_containers<std::vector<int>, std::map<int, int>>::iterator::value_type;

	auto r = std::distance(a.begin(), a.end());
	assert(r == 2);
	std::swap(a, b);
	auto r2 = std::distance(b.cbegin(), b.cend());
	assert(r2 == 2);

	range q(0, 10);
	range p = q;
	q = p;

	expr<int> e = []() -> int { return 1; };
	expr<int> o = []() -> int { return 1; };

	e = o;
}

void setvarval(var<int> a, int val) { 
	// Make sure to move val in, otherwise it will store a reference!
	a = std::move(val); 
}

void lifetime() {

	// Going out of scope
	var<int> a = 1;
	{
		var<int> b = a;
		b = 19;
	}
	assert(a.run_expression() == 19);

	// Through function call
	setvarval(a, 10);
	assert(a.run_expression() == 10);
}

void constructibility() {
	var<int> a = 1; // directly
	a = 1; // assigning
	int i = 10;
	a = i; // reference
	a.run_expression() += 10;
	assert(i == 20); // i has been changed because stored as reference

	struct C {
		C() = delete;
		C(int v) : v(v) {}
		int v = 19;
	};

	var<C> t; // No default constructor

	// Should throw, because it will try derefencing a nullptr because it has no value
	C& q = t.run_expression(); assert(&q == nullptr);

	var<C> b = C{ 10 }; // By temporary
	b = 10; // Constructor argument of type
	C& c = b.run_expression();
	assert(c.v == 10);
}

void some_examples() {
	// Simple list comprehension with ranges and a constraint.
    var<int> a, b, c;
    auto r1 = lc[(a, b, c) | c < -range(1, 11), b < -range(1, c + 1), a < -range(1, b + 1), a* a + b * b == c * c];

    // Parallel iteration
    var<std::tuple<int, int>> d;
    auto r2 = lc[d | d < -(range(0, 10), range(0, 10))];

    // Iteration on some container
    std::vector<int> data{ 1, 2, 3, 4, 5 };
    auto r3 = lc[a + b | a < -data, b < -range(0, 10)];

    // Determine resulting container
    std::vector<std::string> strings{ "hello", "carrot", "pizza" };
    var<std::string> e;
    auto r4 = lc[map(e, a) | (e, a) < -(strings, range(0, 100))];

    // Call std functions
    std::vector<int> ints1{ 5, 2, 7, 3, 1, 9 };
    std::vector<int> ints2{ 4, 1, 8, 9, 3, 2 };
    auto r5 = lc[max(a, b) | (a, b) < -(ints1, ints2)];

    // Automatically uses std::string when working with characters (to<char>() because tolower() returns int)
    std::string mystr = "HelloWorld";
    var<char> g;
    std::string r6 = lc[tolower(g).to<char>() | g < -mystr];

    // Nested list comprehension for lists of lists
    var<int> x;
    var<std::vector<int>> xs;
    std::vector<std::vector<int>> xxs{ { 1,3,5,2,3,1,2,4,5 }, { 1,2,3,4,5,6,7,8,9 }, { 1,2,4,2,1,6,3,1,3,2,3,6 } };
    auto r8 = lc[lcv[x | x < -xs, x % 2 == 0] | xs < -xxs];

    // Using a used variable as a container
    auto r9 = lc[x | xs < -xxs, x < -xs];

    // Make a utility function
    auto indices = [x = var<int>{}, i = var<int>{}](auto& data, var<int> a) mutable {
        // Parallel iteration of value and index, constraint on value == argument, store index.
        return lc[i | (x, i) < -(data, range(0ull, data.size())), a == x];
    };

    std::vector<int> datas{ 0, 1, 1, 0, 0, 1, 0 };
    auto r10 = indices(datas, 1);

    // Parallel iteration with names for each instead of tuple
    auto r11 = lc[(a + b + c) | (a, b, c) < -(range(0, 10), range(0, 10), range(0, 10))];

    // Split a tuple from a container
    std::vector<std::tuple<int, int, int>> q{ { 0, 1, 1 }, { 2, 3, 3 } };
    auto r12 = lc[a + b + c | (a, b, c) <- q];

    // Get the key and value from a map
    std::map<std::string, int> mymap{ { "apple", 1 }, { "carrot", 3 } };
    var<std::string> key;
    var<int> value;
    auto r13 = lc[value + a + b | ((key, value), (a, b)) <- (mymap, (ints1, ints2))];

	// Generate infinite list, and take first 10
	auto lazylist = lcl[a | a <- range(0, inf)];
	auto r14 = lazylist.take(10);


	std::vector<std::function<int()>> funs{ { []() { return 0; } }, { []() { return 1; }} };
	var<std::function<int()>> fun;

	auto r15 = lc[fun() | fun <- funs];

	std::cout << "";
}


int main() {
	std::incrementable_traits<range<int>::iterator>::difference_type;


	var<int> a, b;

	lifetime();
	constructibility();
	containers();
	some_examples();
}
