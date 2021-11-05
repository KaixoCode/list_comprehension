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

void container_types() {
	var<int> a, b;
	lc[(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[vector(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[list(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[deque(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[stack(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[queue(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[priority_queue(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[set(a + b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[multiset(a + b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[unordered_set(a + b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[unordered_multiset(a + b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[map(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[multimap(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[unordered_map(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
	lc[unordered_multimap(a, b + 1) | (a, b) < -(range(0, 10), range(0, 10))];
}

void all_operators() {

	// Normal ops
	var<int> a;
	expr<int> b;
	a + a; a - a; a / a; a * a; a % a; a == a; a != a; a <= a; a >= a; a > a; a < a; a <=> a; a && a; a & a; a || a; a | a; a << a; a >> a; 
	1 + a; 1 - a; 1 / a; 1 * a; 1 % a; 1 == a; 1 != a; 1 <= a; 1 >= a; 1 > a; 1 < a; 1 <=> a; 1 && a; 1 & a; 1 || a; 1 | a; 1 << a; 1 >> a; 
	a + 1; a - 1; a / 1; a * 1; a % 1; a == 1; a != 1; a <= 1; a >= 1; a > 1; a < 1; a <=> 1; a && 1; a & 1; a || 1; a | 1; a << 1; a >> 1; 
	b + b; b - b; b / b; b * b; b % b; b == b; b != b; b <= b; b >= b; b > b; b < b; b <=> b; b && b; b & b; b || b; b | b; b << b; b >> b; 
	1 + b; 1 - b; 1 / b; 1 * b; 1 % b; 1 == b; 1 != b; 1 <= b; 1 >= b; 1 > b; 1 < b; 1 <=> b; 1 && b; 1 & b; 1 || b; 1 | b; 1 << b; 1 >> b; 
	b + 1; b - 1; b / 1; b * 1; b % 1; b == 1; b != 1; b <= 1; b >= 1; b > 1; b < 1; b <=> 1; b && 1; b & 1; b || 1; b | 1; b << 1; b >> 1; 
	a + b; a - b; a / b; a * b; a % b; a == b; a != b; a <= b; a >= b; a > b; a < b; a <=> b; a && b; a & b; a || b; a | b; a << b; a >> b; 
	b + a; b - a; b / a; b * a;	b % a; b == a; b != a; b <= a; b >= a; b > a; b < a; b <=> a; b && a; b & a; b || a; b | a;	b << a; b >> a;
	-a; -b; +a; +b; ~a; ~b; !a; !b; 
	
	var<int*> c;
	expr<int*> d;
	expr<int&> e;
	*c; *d; &a; &e;

	// Tuple of vars
	var<int> x, y;
	tuple_of_vars<int, int> _1 = (x, y);
	tuple_of_vars<int, int, int> _2 = (x, y, x);
	tuple_of_vars<int, int, int> _3 = (x, (y, x));
	tuple_of_vars<int, int, int, int> _4 = ((x, y), (y, x));

	// Container creation
	std::vector<int> c1;
	std::list<int> c2;
	std::deque<int> c3;
	range<int> c4(0, 10);
	std::set<int> c7;
	std::multiset<int> c8;
	std::unordered_set<int> c9;
	std::unordered_multiset<int> c10;
	std::map<int, int> c11;
	std::multimap<int, int> c12;
	std::unordered_map<int, int> c13;
	std::unordered_multimap<int, int> c14;

	lc[a | a <- c1];
	lc[a | a <- c2];
	lc[a | a <- c3];
	lc[a | a <- c4];
	lc[a | a <- c7];
	lc[a | a <- c8];
	lc[a | a <- c9];
	lc[a | a <- c10];
	lc[a | (a, y) <- c11];
	lc[a | (a, y) <- c12];
	lc[a | (a, y) <- c13];
	lc[a | (a, y) <- c14];

	lc[a | a <- std::vector<int>{}];
	lc[a | a <- std::list<int>{}];
	lc[a | a <- std::deque<int>{}];
	lc[a | a <- range<int>(0, 10)];
	lc[a | a <- std::set<int>{}];
	lc[a | a <- std::multiset<int>{}];
	lc[a | a <- std::unordered_set<int>{}];
	lc[a | a <- std::unordered_multiset<int>{}];
	lc[a | (a, y) <- std::map<int, int>{}];
	lc[a | (a, y) <- std::multimap<int, int>{}];
	lc[a | (a, y) <- std::unordered_map<int, int>{}];
	lc[a | (a, y) < -std::unordered_multimap<int, int>{}];

	lc[a | (x, a) <- (c1, std::vector<int>{})];
	lc[a | (x, a) <- (c1, std::list<int>{})];
	lc[a | (x, a) <- (c1, std::deque<int>{})];
	lc[a | (x, a) <- (c1, range<int>(0, 10))];
	lc[a | (x, a) <- (c1, std::set<int>{})];
	lc[a | (x, a) <- (c1, std::multiset<int>{})];
	lc[a | (x, a) <- (c1, std::unordered_set<int>{})];
	lc[a | (x, a) <- (c1, std::unordered_multiset<int>{})];
	lc[a | (x, (a, y)) <- (c1, std::map<int, int>{})];
	lc[a | (x, (a, y)) <- (c1, std::multimap<int, int>{})];
	lc[a | (x, (a, y)) <- (c1, std::unordered_map<int, int>{})];
	lc[a | (x, (a, y)) <- (c1, std::unordered_multimap<int, int>{})];

	c1, c2, c3;
	c1, (c2, c3);
	(c1, c1), (c2, c3);	
	"aa", c1, c3;
	c1, (c2, "aefae");
	"aefa", (c2, c3);
	std::vector<int>{}, std::vector<int>{}, std::vector<int>{};
	std::vector<int>{}, (std::vector<int>{}, std::vector<int>{});
	(std::vector<int>{}, std::vector<int>{}), (std::vector<int>{}, std::vector<int>{});
	c1, std::vector<int>{}, std::vector<int>{};
	std::vector<int>{}, (std::vector<int>{}, c1);
	c1, (std::vector<int>{}, c1);

	var<std::string> str;
	expr<std::string&> aef = str.append("135135");
	std::string& val = aef.run_expression();
	expr<int> caefa = max(x, y);
}

void test() {
	std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();

	var<size_t> n, i, x, j;

	auto collatz = [](auto x) { 
		return (x % 2 == 0) * (x / 2) + (x % 2 == 1) * (3 * x + 1); 
	};

	auto res = 
		lcl[x | 
			i <- range(1ull, inf), 
			n <<= i, 
			x <<= lcv[n | 
				j <- range(0ull, inf), 
				n <<= collatz(n), 
				brk <<= n == 1
			].size() + 2];
	
	auto t = res.take(1'000'000);

	auto max = std::max_element(t.begin(), t.end());
	auto index = std::distance(t.begin(), max);


    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << "[µs]" << std::endl;

	std::cout << *max << std::endl;
	std::cout << index << std::endl;
}




int main() {



	var<int> x;
	var<range<int>> y;
	auto r = lcl[(y + ___) | x <- range(1, inf), y <<= range(0, x)];
	auto res = r.take(1000);

	test();
	container_types();
	all_operators();
	lifetime();
	constructibility();
	containers();
	some_examples();
}
