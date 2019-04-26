/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
/**
 * @author Christian <c@ethdev.com>
 * @author Gav Wood <g@ethdev.com>
 * @date 2014
 * Unit tests for the solidity expression compiler, testing the behaviour of the code.
 */

#include <test/libsolidity/SolidityExecutionFramework.h>

#include <test/Options.h>

#include <liblangutil/Exceptions.h>
#include <liblangutil/EVMVersion.h>

#include <libevmasm/Assembly.h>

#include <libdevcore/Keccak256.h>

#include <boost/range/adaptor/transformed.hpp>
#include <boost/test/unit_test.hpp>

#include <functional>
#include <string>
#include <tuple>

using namespace std;
using namespace std::placeholders;
using namespace dev::test;

namespace dev
{
namespace solidity
{
namespace test
{

BOOST_FIXTURE_TEST_SUITE(SolidityEndToEndTest, SolidityExecutionFramework)

BOOST_AUTO_TEST_CASE(transaction_status)
{
	char const* sourceCode = R"(
		contract test {
			function f() public { }
			function g() public { revert(); }
			function h() public { assert(false); }
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("f()");
	BOOST_CHECK(m_transactionSuccessful);
	callContractFunction("g()");
	BOOST_CHECK(!m_transactionSuccessful);
	callContractFunction("h()");
	BOOST_CHECK(!m_transactionSuccessful);
}


BOOST_AUTO_TEST_CASE(smoke_test)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint a) public returns(uint d) { return a * 7; }
		}
	)";
	compileAndRun(sourceCode);
	testContractAgainstCppOnRange("f(uint256)", [](u256 const& a) -> u256 { return a * 7; }, 0, 100);
}

BOOST_AUTO_TEST_CASE(empty_contract)
{
	char const* sourceCode = R"(
		contract test { }
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(callContractFunction("i_am_not_there()", bytes()).empty());
}

BOOST_AUTO_TEST_CASE(exp_operator)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint a) public returns(uint d) { return 2 ** a; }
		}
	)";
	compileAndRun(sourceCode);
	testContractAgainstCppOnRange("f(uint256)", [](u256 const& a) -> u256 { return u256(1 << a.convert_to<int>()); }, 0, 16);
}

BOOST_AUTO_TEST_CASE(exp_operator_const)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns(uint d) { return 2 ** 3; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", bytes()), toBigEndian(u256(8)));
}

BOOST_AUTO_TEST_CASE(exp_operator_const_signed)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns(int d) { return (-2) ** 3; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", bytes()), toBigEndian(u256(-8)));
}

BOOST_AUTO_TEST_CASE(exp_zero)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint a) public returns(uint d) { return a ** 0; }
		}
	)";
	compileAndRun(sourceCode);
	testContractAgainstCppOnRange("f(uint256)", [](u256 const&) -> u256 { return u256(1); }, 0, 16);
}

BOOST_AUTO_TEST_CASE(exp_zero_literal)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns(uint d) { return 0 ** 0; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", bytes()), toBigEndian(u256(1)));
}


BOOST_AUTO_TEST_CASE(conditional_expression_true_literal)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns(uint d) {
				return true ? 5 : 10;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", bytes()), toBigEndian(u256(5)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_false_literal)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns(uint d) {
				return false ? 5 : 10;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", bytes()), toBigEndian(u256(10)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_multiple)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint x) public returns(uint d) {
				return x > 100 ?
							x > 1000 ? 1000 : 100
							:
							x > 50 ? 50 : 10;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256)", u256(1001)), toBigEndian(u256(1000)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(500)), toBigEndian(u256(100)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(80)), toBigEndian(u256(50)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(40)), toBigEndian(u256(10)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_with_return_values)
{
	char const* sourceCode = R"(
		contract test {
			function f(bool cond, uint v) public returns (uint a, uint b) {
				cond ? a = v : b = v;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool,uint256)", true, u256(20)), encodeArgs(u256(20), u256(0)));
	ABI_CHECK(callContractFunction("f(bool,uint256)", false, u256(20)), encodeArgs(u256(0), u256(20)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_storage_memory_1)
{
	char const* sourceCode = R"(
		contract test {
			bytes2[2] data1;
			function f(bool cond) public returns (uint) {
				bytes2[2] memory x;
				x[0] = "aa";
				bytes2[2] memory y;
				y[0] = "bb";

				data1 = cond ? x : y;

				uint ret = 0;
				if (data1[0] == "aa")
				{
					ret = 1;
				}

				if (data1[0] == "bb")
				{
					ret = 2;
				}

				return ret;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_storage_memory_2)
{
	char const* sourceCode = R"(
		contract test {
			bytes2[2] data1;
			function f(bool cond) public returns (uint) {
				data1[0] = "cc";

				bytes2[2] memory x;
				bytes2[2] memory y;
				y[0] = "bb";

				x = cond ? y : data1;

				uint ret = 0;
				if (x[0] == "bb")
				{
					ret = 1;
				}

				if (x[0] == "cc")
				{
					ret = 2;
				}

				return ret;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_different_types)
{
	char const* sourceCode = R"(
		contract test {
			function f(bool cond) public returns (uint) {
				uint8 x = 0xcd;
				uint16 y = 0xabab;
				return cond ? x : y;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(u256(0xcd)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(u256(0xabab)));
}

/* let's add this back when I figure out the correct type conversion.
BOOST_AUTO_TEST_CASE(conditional_expression_string_literal)
{
	char const* sourceCode = R"(
		contract test {
			function f(bool cond) public returns (bytes32) {
				return cond ? "true" : "false";
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(string("true", 4)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(string("false", 5)));
}
*/

BOOST_AUTO_TEST_CASE(conditional_expression_tuples)
{
	char const* sourceCode = R"(
		contract test {
			function f(bool cond) public returns (uint, uint) {
				return cond ? (1, 2) : (3, 4);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(u256(1), u256(2)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(u256(3), u256(4)));
}

BOOST_AUTO_TEST_CASE(conditional_expression_functions)
{
	char const* sourceCode = R"(
		contract test {
			function x() public returns (uint) { return 1; }
			function y() public returns (uint) { return 2; }

			function f(bool cond) public returns (uint) {
				function () returns (uint) z = cond ? x : y;
				return z();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(c99_scoping_activation)
{
	char const* sourceCode = R"(
		contract test {
			function f() pure public returns (uint) {
				uint x = 7;
				{
					x = 3; // This should still assign to the outer variable
					uint x;
					x = 4; // This should assign to the new one
				}
				return x;
			}
			function g() pure public returns (uint x) {
				x = 7;
				{
					x = 3;
					uint x;
					return x; // This returns the new variable, i.e. 0
				}
			}
			function h() pure public returns (uint x, uint a, uint b) {
				x = 7;
				{
					x = 3;
					a = x; // This should read from the outer
					uint x = 4;
					b = x;
				}
			}
			function i() pure public returns (uint x, uint a) {
				x = 7;
				{
					x = 3;
					uint x = x; // This should read from the outer and assign to the inner
					a = x;
				}
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(3));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(3, 3, 4));
	ABI_CHECK(callContractFunction("i()"), encodeArgs(3, 3));
}

BOOST_AUTO_TEST_CASE(recursive_calls)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint n) public returns(uint nfac) {
				if (n <= 1) return 1;
				else return n * f(n - 1);
			}
		}
	)";
	compileAndRun(sourceCode);
	function<u256(u256)> recursive_calls_cpp = [&recursive_calls_cpp](u256 const& n) -> u256
	{
		if (n <= 1)
			return 1;
		else
			return n * recursive_calls_cpp(n - 1);
	};

	testContractAgainstCppOnRange("f(uint256)", recursive_calls_cpp, 0, 5);
}

BOOST_AUTO_TEST_CASE(multiple_functions)
{
	char const* sourceCode = R"(
		contract test {
			function a() public returns(uint n) { return 0; }
			function b() public returns(uint n) { return 1; }
			function c() public returns(uint n) { return 2; }
			function f() public returns(uint n) { return 3; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("a()", bytes()), toBigEndian(u256(0)));
	ABI_CHECK(callContractFunction("b()", bytes()), toBigEndian(u256(1)));
	ABI_CHECK(callContractFunction("c()", bytes()), toBigEndian(u256(2)));
	ABI_CHECK(callContractFunction("f()", bytes()), toBigEndian(u256(3)));
	ABI_CHECK(callContractFunction("i_am_not_there()", bytes()), bytes());
}

BOOST_AUTO_TEST_CASE(named_args)
{
	char const* sourceCode = R"(
		contract test {
			function a(uint a, uint b, uint c) public returns (uint r) { r = a * 100 + b * 10 + c * 1; }
			function b() public returns (uint r) { r = a({a: 1, b: 2, c: 3}); }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("b()", bytes()), toBigEndian(u256(123)));
}

BOOST_AUTO_TEST_CASE(disorder_named_args)
{
	char const* sourceCode = R"(
		contract test {
			function a(uint a, uint b, uint c) public returns (uint r) { r = a * 100 + b * 10 + c * 1; }
			function b() public returns (uint r) { r = a({c: 3, a: 1, b: 2}); }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("b()", bytes()), toBigEndian(u256(123)));
}

BOOST_AUTO_TEST_CASE(while_loop)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint n) public returns(uint nfac) {
				nfac = 1;
				uint i = 2;
				while (i <= n) nfac *= i++;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto while_loop_cpp = [](u256 const& n) -> u256
	{
		u256 nfac = 1;
		u256 i = 2;
		while (i <= n)
			nfac *= i++;

		return nfac;
	};

	testContractAgainstCppOnRange("f(uint256)", while_loop_cpp, 0, 5);
}


BOOST_AUTO_TEST_CASE(do_while_loop)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint n) public returns(uint nfac) {
				nfac = 1;
				uint i = 2;
				do { nfac *= i++; } while (i <= n);
			}
		}
	)";
	compileAndRun(sourceCode);

	auto do_while_loop_cpp = [](u256 const& n) -> u256
	{
		u256 nfac = 1;
		u256 i = 2;
		do
		{
			nfac *= i++;
		}
		while (i <= n);

		return nfac;
	};

	testContractAgainstCppOnRange("f(uint256)", do_while_loop_cpp, 0, 5);
}

BOOST_AUTO_TEST_CASE(do_while_loop_continue)
{
	char const* sourceCode = R"(
		contract test {
			function f() public pure returns(uint r) {
				uint i = 0;
				do
				{
					if (i > 0) return 0;
					i++;
					continue;
				} while (false);
				return 42;
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("f()"), encodeArgs(42));
}

BOOST_AUTO_TEST_CASE(array_multiple_local_vars)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint256[] calldata seq) external pure returns (uint256) {
				uint i = 0;
				uint sum = 0;
				while (i < seq.length)
				{
					uint idx = i;
					if (idx >= 10) break;
					uint x = seq[idx];
					if (x >= 1000) {
						uint n = i + 1;
						i = n;
						continue;
					}
					else {
						uint y = sum + x;
						sum = y;
					}
					if (sum >= 500) return sum;
					i++;
				}
				return sum;
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("f(uint256[])", 32, 3, u256(1000), u256(1), u256(2)), encodeArgs(3));
	ABI_CHECK(callContractFunction("f(uint256[])", 32, 3, u256(100), u256(500), u256(300)), encodeArgs(600));
	ABI_CHECK(callContractFunction(
		"f(uint256[])", 32, 11,
		u256(1), u256(2), u256(3), u256(4), u256(5), u256(6), u256(7), u256(8), u256(9), u256(10), u256(111)
		), encodeArgs(55));
}


BOOST_AUTO_TEST_CASE(do_while_loop_multiple_local_vars)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint x) public pure returns(uint r) {
				uint i = 0;
				do
				{
					uint z = x * 2;
					if (z < 4) break;
					else {
						uint k = z + 1;
						if (k < 8) {
							x++;
							continue;
						}
					}
					if (z > 12) return 0;
					x++;
					i++;
				} while (true);
				return 42;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto do_while = [](u256 n) -> u256
	{
		u256 i = 0;
		do
		{
			u256 z = n * 2;
			if (z < 4) break;
			else {
				u256 k = z + 1;
				if (k < 8) {
					n++;
					continue;
				}
			}
			if (z > 12) return 0;
			n++;
			i++;
		} while (true);
		return 42;
	};

	testContractAgainstCppOnRange("f(uint256)", do_while, 0, 12);
}

BOOST_AUTO_TEST_CASE(nested_loops)
{
	// tests that break and continue statements in nested loops jump to the correct place
	char const* sourceCode = R"(
		contract test {
			function f(uint x) public returns(uint y) {
				while (x > 1) {
					if (x == 10) break;
					while (x > 5) {
						if (x == 8) break;
						x--;
						if (x == 6) continue;
						return x;
					}
					x--;
					if (x == 3) continue;
					break;
				}
				return x;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto nested_loops_cpp = [](u256 n) -> u256
	{
		while (n > 1)
		{
			if (n == 10)
				break;
			while (n > 5)
			{
				if (n == 8)
					break;
				n--;
				if (n == 6)
					continue;
				return n;
			}
			n--;
			if (n == 3)
				continue;
			break;
		}

		return n;
	};

	testContractAgainstCppOnRange("f(uint256)", nested_loops_cpp, 0, 12);
}

BOOST_AUTO_TEST_CASE(nested_loops_multiple_local_vars)
{
	// tests that break and continue statements in nested loops jump to the correct place
	// and free local variables properly
	char const* sourceCode = R"(
		contract test {
			function f(uint x) public returns(uint y) {
				while (x > 0) {
					uint z = x + 10;
					uint k = z + 1;
					if (k > 20) {
						break;
						uint p = 100;
						k += p;
					}
					if (k > 15) {
						x--;
						continue;
						uint t = 1000;
						x += t;
					}
					while (k > 10) {
						uint m = k - 1;
						if (m == 10) return x;
						return k;
						uint h = 10000;
						z += h;
					}
					x--;
					break;
				}
				return x;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto nested_loops_cpp = [](u256 n) -> u256
	{
		while (n > 0)
		{
			u256 z = n + 10;
			u256 k = z + 1;
			if (k > 20) break;
			if (k > 15) {
				n--;
				continue;
			}
			while (k > 10)
			{
				u256 m = k - 1;
				if (m == 10) return n;
				return k;
			}
			n--;
			break;
		}

		return n;
	};

	testContractAgainstCppOnRange("f(uint256)", nested_loops_cpp, 0, 12);
}

BOOST_AUTO_TEST_CASE(for_loop_multiple_local_vars)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint x) public pure returns(uint r) {
				for (uint i = 0; i < 12; i++)
				{
					uint z = x + 1;
					if (z < 4) break;
					else {
						uint k = z * 2;
						if (i + k < 10) {
							x++;
							continue;
						}
					}
					if (z > 8) return 0;
					x++;
				}
				return 42;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto for_loop = [](u256 n) -> u256
	{
		for (u256 i = 0; i < 12; i++)
		{
			u256 z = n + 1;
			if (z < 4) break;
			else {
				u256 k = z * 2;
				if (i + k < 10) {
					n++;
					continue;
				}
			}
			if (z > 8) return 0;
			n++;
		}
		return 42;
	};

	testContractAgainstCppOnRange("f(uint256)", for_loop, 0, 12);
}

BOOST_AUTO_TEST_CASE(nested_for_loop_multiple_local_vars)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint x) public pure returns(uint r) {
				for (uint i = 0; i < 5; i++)
				{
					uint z = x + 1;
					if (z < 3) {
						break;
						uint p = z + 2;
					}
					for (uint j = 0; j < 5; j++)
					{
						uint k = z * 2;
						if (j + k < 8) {
							x++;
							continue;
							uint t = z * 3;
						}
						x++;
						if (x > 20) {
							return 84;
							uint h = x + 42;
						}
					}
					if (x > 30) {
						return 42;
						uint b = 0xcafe;
					}
				}
				return 42;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto for_loop = [](u256 n) -> u256
	{
		for (u256 i = 0; i < 5; i++)
		{
			u256 z = n + 1;
			if (z < 3) break;
			for (u256 j = 0; j < 5; j++)
			{
				u256 k = z * 2;
				if (j + k < 8) {
					n++;
					continue;
				}
				n++;
				if (n > 20) return 84;
			}
			if (n > 30) return 42;
		}
		return 42;
	};

	testContractAgainstCppOnRange("f(uint256)", for_loop, 0, 12);
}

BOOST_AUTO_TEST_CASE(for_loop)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint n) public returns(uint nfac) {
				nfac = 1;
				uint i;
				for (i = 2; i <= n; i++)
					nfac *= i;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto for_loop_cpp = [](u256 const& n) -> u256
	{
		u256 nfac = 1;
		for (auto i = 2; i <= n; i++)
			nfac *= i;
		return nfac;
	};

	testContractAgainstCppOnRange("f(uint256)", for_loop_cpp, 0, 5);
}

BOOST_AUTO_TEST_CASE(for_loop_empty)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns(uint ret) {
				ret = 1;
				for (;;) {
					ret += 1;
					if (ret >= 10) break;
				}
			}
		}
	)";
	compileAndRun(sourceCode);

	auto for_loop_empty_cpp = []() -> u256
	{
		u256 ret = 1;
		for (;;)
		{
			ret += 1;
			if (ret >= 10) break;
		}
		return ret;
	};

	testContractAgainstCpp("f()", for_loop_empty_cpp);
}

BOOST_AUTO_TEST_CASE(for_loop_simple_init_expr)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint n) public returns(uint nfac) {
				nfac = 1;
				uint256 i;
				for (i = 2; i <= n; i++)
					nfac *= i;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto for_loop_simple_init_expr_cpp = [](u256 const& n) -> u256
	{
		u256 nfac = 1;
		u256 i;
		for (i = 2; i <= n; i++)
			nfac *= i;
		return nfac;
	};

	testContractAgainstCppOnRange("f(uint256)", for_loop_simple_init_expr_cpp, 0, 5);
}

BOOST_AUTO_TEST_CASE(for_loop_break_continue)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint n) public returns (uint r)
			{
				uint i = 1;
				uint k = 0;
				for (i *= 5; k < n; i *= 7)
				{
					k++;
					i += 4;
					if (n % 3 == 0)
						break;
					i += 9;
					if (n % 2 == 0)
						continue;
					i += 19;
				}
				return i;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto breakContinue = [](u256 const& n) -> u256
	{
		u256 i = 1;
		u256 k = 0;
		for (i *= 5; k < n; i *= 7)
		{
			k++;
			i += 4;
			if (n % 3 == 0)
				break;
			i += 9;
			if (n % 2 == 0)
				continue;
			i += 19;
		}
		return i;
	};

	testContractAgainstCppOnRange("f(uint256)", breakContinue, 0, 10);
}

BOOST_AUTO_TEST_CASE(calling_other_functions)
{
	char const* sourceCode = R"(
		contract collatz {
			function run(uint x) public returns(uint y) {
				while ((y = x) > 1) {
					if (x % 2 == 0) x = evenStep(x);
					else x = oddStep(x);
				}
			}
			function evenStep(uint x) public returns(uint y) {
				return x / 2;
			}
			function oddStep(uint x) public returns(uint y) {
				return 3 * x + 1;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto evenStep_cpp = [](u256 const& n) -> u256
	{
		return n / 2;
	};

	auto oddStep_cpp = [](u256 const& n) -> u256
	{
		return 3 * n + 1;
	};

	auto collatz_cpp = [&evenStep_cpp, &oddStep_cpp](u256 n) -> u256
	{
		u256 y;
		while ((y = n) > 1)
		{
			if (n % 2 == 0)
				n = evenStep_cpp(n);
			else
				n = oddStep_cpp(n);
		}
		return y;
	};

	testContractAgainstCpp("run(uint256)", collatz_cpp, u256(0));
	testContractAgainstCpp("run(uint256)", collatz_cpp, u256(1));
	testContractAgainstCpp("run(uint256)", collatz_cpp, u256(2));
	testContractAgainstCpp("run(uint256)", collatz_cpp, u256(8));
	testContractAgainstCpp("run(uint256)", collatz_cpp, u256(127));
}

BOOST_AUTO_TEST_CASE(many_local_variables)
{
	char const* sourceCode = R"(
		contract test {
			function run(uint x1, uint x2, uint x3) public returns(uint y) {
				uint8 a = 0x1; uint8 b = 0x10; uint16 c = 0x100;
				y = a + b + c + x1 + x2 + x3;
				y += b + x2;
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [](u256 const& x1, u256 const& x2, u256 const& x3) -> u256
	{
		u256 a = 0x1;
		u256 b = 0x10;
		u256 c = 0x100;
		u256 y = a + b + c + x1 + x2 + x3;
		return y + b + x2;
	};
	testContractAgainstCpp("run(uint256,uint256,uint256)", f, u256(0x1000), u256(0x10000), u256(0x100000));
}

BOOST_AUTO_TEST_CASE(packing_unpacking_types)
{
	char const* sourceCode = R"(
		contract test {
			function run(bool a, uint32 b, uint64 c) public returns(uint256 y) {
				if (a) y = 1;
				y = y * 0x100000000 | ~b;
				y = y * 0x10000000000000000 | ~c;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("run(bool,uint32,uint64)", true, fromHex("0f0f0f0f"), fromHex("f0f0f0f0f0f0f0f0")),
		fromHex("00000000000000000000000000000000000000""01""f0f0f0f0""0f0f0f0f0f0f0f0f")
	);
}

BOOST_AUTO_TEST_CASE(packing_signed_types)
{
	char const* sourceCode = R"(
		contract test {
			function run() public returns(int8 y) {
				uint8 x = 0xfa;
				return int8(x);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("run()"),
		fromHex("fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa")
	);
}

BOOST_AUTO_TEST_CASE(multiple_return_values)
{
	char const* sourceCode = R"(
		contract test {
			function run(bool x1, uint x2) public returns(uint y1, bool y2, uint y3) {
				y1 = x2; y2 = x1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("run(bool,uint256)", true, 0xcd), encodeArgs(0xcd, true, 0));
}

BOOST_AUTO_TEST_CASE(short_circuiting)
{
	char const* sourceCode = R"(
		contract test {
			function run(uint x) public returns(uint y) {
				x == 0 || ((x = 8) > 0);
				return x;
			}
		}
	)";
	compileAndRun(sourceCode);

	auto short_circuiting_cpp = [](u256 n) -> u256
	{
		(void)(n == 0 || (n = 8) > 0);
		return n;
	};

	testContractAgainstCppOnRange("run(uint256)", short_circuiting_cpp, 0, 2);
}

BOOST_AUTO_TEST_CASE(high_bits_cleaning)
{
	char const* sourceCode = R"(
		contract test {
			function run() public returns(uint256 y) {
				uint32 t = uint32(0xffffffff);
				uint32 x = t + 10;
				if (x >= 0xffffffff) return 0;
				return x;
			}
		}
	)";
	compileAndRun(sourceCode);
	auto high_bits_cleaning_cpp = []() -> u256
	{
		uint32_t t = uint32_t(0xffffffff);
		uint32_t x = t + 10;
		if (x >= 0xffffffff)
			return 0;
		return x;
	};
	testContractAgainstCpp("run()", high_bits_cleaning_cpp);
}

BOOST_AUTO_TEST_CASE(sign_extension)
{
	char const* sourceCode = R"(
		contract test {
			function run() public returns(uint256 y) {
				int64 x = -int32(0xff);
				if (x >= 0xff) return 0;
				return -uint256(x);
			}
		}
	)";
	compileAndRun(sourceCode);
	auto sign_extension_cpp = []() -> u256
	{
		int64_t x = -int32_t(0xff);
		if (x >= 0xff)
			return 0;
		return u256(x) * -1;
	};
	testContractAgainstCpp("run()", sign_extension_cpp);
}

BOOST_AUTO_TEST_CASE(small_unsigned_types)
{
	char const* sourceCode = R"(
		contract test {
			function run() public returns(uint256 y) {
				uint32 t = uint32(0xffffff);
				uint32 x = t * 0xffffff;
				return x / 0x100;
			}
		}
	)";
	compileAndRun(sourceCode);
	auto small_unsigned_types_cpp = []() -> u256
	{
		uint32_t t = uint32_t(0xffffff);
		uint32_t x = t * 0xffffff;
		return x / 0x100;
	};
	testContractAgainstCpp("run()", small_unsigned_types_cpp);
}

BOOST_AUTO_TEST_CASE(small_signed_types)
{
	char const* sourceCode = R"(
		contract test {
			function run() public returns(int256 y) {
				return -int32(10) * -int64(20);
			}
		}
	)";
	compileAndRun(sourceCode);
	auto small_signed_types_cpp = []() -> u256
	{
		return -int32_t(10) * -int64_t(20);
	};
	testContractAgainstCpp("run()", small_signed_types_cpp);
}

BOOST_AUTO_TEST_CASE(strings)
{
	char const* sourceCode = R"(
		contract test {
			function fixedBytes() public returns(bytes32 ret) {
				return "abc\x00\xff__";
			}
			function pipeThrough(bytes2 small, bool one) public returns(bytes16 large, bool oneRet) {
				oneRet = one;
				large = small;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("fixedBytes()"), encodeArgs(string("abc\0\xff__", 7)));
	ABI_CHECK(callContractFunction("pipeThrough(bytes2,bool)", string("\0\x02", 2), true), encodeArgs(string("\0\x2", 2), true));
}

BOOST_AUTO_TEST_CASE(inc_dec_operators)
{
	char const* sourceCode = R"(
		contract test {
			uint8 x;
			uint v;
			function f() public returns (uint r) {
				uint a = 6;
				r = a;
				r += (a++) * 0x10;
				r += (++a) * 0x100;
				v = 3;
				r += (v++) * 0x1000;
				r += (++v) * 0x10000;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0x53866));
}

BOOST_AUTO_TEST_CASE(bytes_comparison)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns (bool) {
				bytes2 a = "a";
				bytes2 x = "aa";
				bytes2 b = "b";
				return a < x && x < b;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(state_smoke_test)
{
	char const* sourceCode = R"(
		contract test {
			uint256 value1;
			uint256 value2;
			function get(uint8 which) public returns (uint256 value) {
				if (which == 0) return value1;
				else return value2;
			}
			function set(uint8 which, uint256 value) public {
				if (which == 0) value1 = value;
				else value2 = value;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x00)), encodeArgs(0));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x01)), encodeArgs(0));
	ABI_CHECK(callContractFunction("set(uint8,uint256)", uint8_t(0x00), 0x1234), encodeArgs());
	ABI_CHECK(callContractFunction("set(uint8,uint256)", uint8_t(0x01), 0x8765), encodeArgs());
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t( 0x00)), encodeArgs(0x1234));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x01)), encodeArgs(0x8765));
	ABI_CHECK(callContractFunction("set(uint8,uint256)", uint8_t(0x00), 0x3), encodeArgs());
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x00)), encodeArgs(0x3));
}

BOOST_AUTO_TEST_CASE(compound_assign)
{
	char const* sourceCode = R"(
		contract test {
			uint value1;
			uint value2;
			function f(uint x, uint y) public returns (uint w) {
				uint value3 = y;
				value1 += x;
				value3 *= x;
				value2 *= value3 + value1;
				return value2 += 7;
			}
		}
	)";
	compileAndRun(sourceCode);

	u256 value1;
	u256 value2;
	auto f = [&](u256 const& _x, u256 const& _y) -> u256
	{
		u256 value3 = _y;
		value1 += _x;
		value3 *= _x;
		value2 *= value3 + value1;
		return value2 += 7;
	};
	testContractAgainstCpp("f(uint256,uint256)", f, u256(0), u256(6));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(1), u256(3));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(2), u256(25));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(3), u256(69));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(4), u256(84));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(5), u256(2));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(6), u256(51));
	testContractAgainstCpp("f(uint256,uint256)", f, u256(7), u256(48));
}

BOOST_AUTO_TEST_CASE(simple_mapping)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8) table;
			function get(uint8 k) public returns (uint8 v) {
				return table[k];
			}
			function set(uint8 k, uint8 v) public {
				table[k] = v;
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0)), encodeArgs(uint8_t(0x00)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x01)), encodeArgs(uint8_t(0x00)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0xa7)), encodeArgs(uint8_t(0x00)));
	callContractFunction("set(uint8,uint8)", uint8_t(0x01), uint8_t(0xa1));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x00)), encodeArgs(uint8_t(0x00)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x01)), encodeArgs(uint8_t(0xa1)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0xa7)), encodeArgs(uint8_t(0x00)));
	callContractFunction("set(uint8,uint8)", uint8_t(0x00), uint8_t(0xef));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x00)), encodeArgs(uint8_t(0xef)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x01)), encodeArgs(uint8_t(0xa1)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0xa7)), encodeArgs(uint8_t(0x00)));
	callContractFunction("set(uint8,uint8)", uint8_t(0x01), uint8_t(0x05));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x00)), encodeArgs(uint8_t(0xef)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0x01)), encodeArgs(uint8_t(0x05)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(0xa7)), encodeArgs(uint8_t(0x00)));
}

BOOST_AUTO_TEST_CASE(mapping_state)
{
	char const* sourceCode = R"(
		contract Ballot {
			mapping(address => bool) canVote;
			mapping(address => uint) voteCount;
			mapping(address => bool) voted;
			function getVoteCount(address addr) public returns (uint retVoteCount) {
				return voteCount[addr];
			}
			function grantVoteRight(address addr) public {
				canVote[addr] = true;
			}
			function vote(address voter, address vote) public returns (bool success) {
				if (!canVote[voter] || voted[voter]) return false;
				voted[voter] = true;
				voteCount[vote] = voteCount[vote] + 1;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	class Ballot
	{
	public:
		u256 getVoteCount(u160 _address) { return m_voteCount[_address]; }
		void grantVoteRight(u160 _address) { m_canVote[_address] = true; }
		bool vote(u160 _voter, u160 _vote)
		{
			if (!m_canVote[_voter] || m_voted[_voter]) return false;
			m_voted[_voter] = true;
			m_voteCount[_vote]++;
			return true;
		}
	private:
		map<u160, bool> m_canVote;
		map<u160, u256> m_voteCount;
		map<u160, bool> m_voted;
	} ballot;

	auto getVoteCount = bind(&Ballot::getVoteCount, &ballot, _1);
	auto grantVoteRight = bind(&Ballot::grantVoteRight, &ballot, _1);
	auto vote = bind(&Ballot::vote, &ballot, _1, _2);
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(0));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(2));
	// voting without vote right should be rejected
	testContractAgainstCpp("vote(address,address)", vote, u160(0), u160(2));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(0));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(2));
	// grant vote rights
	testContractAgainstCpp("grantVoteRight(address)", grantVoteRight, u160(0));
	testContractAgainstCpp("grantVoteRight(address)", grantVoteRight, u160(1));
	// vote, should increase 2's vote count
	testContractAgainstCpp("vote(address,address)", vote, u160(0), u160(2));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(0));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(2));
	// vote again, should be rejected
	testContractAgainstCpp("vote(address,address)", vote, u160(0), u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(0));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(2));
	// vote without right to vote
	testContractAgainstCpp("vote(address,address)", vote, u160(2), u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(0));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(2));
	// grant vote right and now vote again
	testContractAgainstCpp("grantVoteRight(address)", grantVoteRight, u160(2));
	testContractAgainstCpp("vote(address,address)", vote, u160(2), u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(0));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(1));
	testContractAgainstCpp("getVoteCount(address)", getVoteCount, u160(2));
}

BOOST_AUTO_TEST_CASE(mapping_state_inc_dec)
{
	char const* sourceCode = R"(
		contract test {
			uint value;
			mapping(uint => uint) table;
			function f(uint x) public returns (uint y) {
				value = x;
				if (x > 0) table[++value] = 8;
				if (x > 1) value--;
				if (x > 2) table[value]++;
				return --table[value++];
			}
		}
	)";
	compileAndRun(sourceCode);

	u256 value = 0;
	map<u256, u256> table;
	auto f = [&](u256 const& _x) -> u256
	{
		value = _x;
		if (_x > 0)
			table[++value] = 8;
		if (_x > 1)
			value --;
		if (_x > 2)
			table[value]++;
		return --table[value++];
	};
	testContractAgainstCppOnRange("f(uint256)", f, 0, 5);
}

BOOST_AUTO_TEST_CASE(multi_level_mapping)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint => mapping(uint => uint)) table;
			function f(uint x, uint y, uint z) public returns (uint w) {
				if (z == 0) return table[x][y];
				else return table[x][y] = z;
			}
		}
	)";
	compileAndRun(sourceCode);

	map<u256, map<u256, u256>> table;
	auto f = [&](u256 const& _x, u256 const& _y, u256 const& _z) -> u256
	{
		if (_z == 0) return table[_x][_y];
		else return table[_x][_y] = _z;
	};
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(4), u256(5), u256(0));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(5), u256(4), u256(0));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(4), u256(5), u256(9));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(4), u256(5), u256(0));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(5), u256(4), u256(0));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(5), u256(4), u256(7));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(4), u256(5), u256(0));
	testContractAgainstCpp("f(uint256,uint256,uint256)", f, u256(5), u256(4), u256(0));
}

BOOST_AUTO_TEST_CASE(mapping_local_assignment)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8) m1;
			mapping(uint8 => uint8) m2;
			function f() public returns (uint8, uint8, uint8, uint8) {
				mapping(uint8 => uint8) storage m = m1;
				m[1] = 42;

				m = m2;
				m[2] = 21;

				return (m1[1], m1[2], m2[1], m2[2]);
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("f()"), encodeArgs(uint8_t(42), uint8_t(0), uint8_t(0), uint8_t(21)));
}

BOOST_AUTO_TEST_CASE(mapping_local_tuple_assignment)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8) m1;
			mapping(uint8 => uint8) m2;
			function f() public returns (uint8, uint8, uint8, uint8) {
				mapping(uint8 => uint8) storage m = m1;
				m[1] = 42;

				uint8 v;
				(m, v) = (m2, 21);
				m[2] = v;

				return (m1[1], m1[2], m2[1], m2[2]);
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("f()"), encodeArgs(uint8_t(42), uint8_t(0), uint8_t(0), uint8_t(21)));
}

BOOST_AUTO_TEST_CASE(mapping_local_compound_assignment)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8) m1;
			mapping(uint8 => uint8) m2;
			function f() public returns (uint8, uint8, uint8, uint8) {
				mapping(uint8 => uint8) storage m = m1;
				m[1] = 42;

				(m = m2)[2] = 21;

				return (m1[1], m1[2], m2[1], m2[2]);
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("f()"), encodeArgs(uint8_t(42), uint8_t(0), uint8_t(0), uint8_t(21)));
}

BOOST_AUTO_TEST_CASE(mapping_internal_argument)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8) a;
			mapping(uint8 => uint8) b;
			function set_internal(mapping(uint8 => uint8) storage m, uint8 key, uint8 value) internal returns (uint8) {
				uint8 oldValue = m[key];
				m[key] = value;
				return oldValue;
			}
			function set(uint8 key, uint8 value_a, uint8 value_b) public returns (uint8 old_a, uint8 old_b) {
				old_a = set_internal(a, key, value_a);
				old_b = set_internal(b, key, value_b);
			}
			function get(uint8 key) public returns (uint8, uint8) {
				return (a[key], b[key]);
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("set(uint8,uint8,uint8)", uint8_t(1), uint8_t(21), uint8_t(42)), encodeArgs(uint8_t(0), uint8_t(0)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(1)), encodeArgs(uint8_t(21), uint8_t(42)));
	ABI_CHECK(callContractFunction("set(uint8,uint8,uint8)", uint8_t(1), uint8_t(10), uint8_t(11)), encodeArgs(uint8_t(21), uint8_t(42)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(1)), encodeArgs(uint8_t(10), uint8_t(11)));
}

BOOST_AUTO_TEST_CASE(mapping_array_internal_argument)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8)[2] a;
			mapping(uint8 => uint8)[2] b;
			function set_internal(mapping(uint8 => uint8)[2] storage m, uint8 key, uint8 value1, uint8 value2) internal returns (uint8, uint8) {
				uint8 oldValue1 = m[0][key];
				uint8 oldValue2 = m[1][key];
				m[0][key] = value1;
				m[1][key] = value2;
				return (oldValue1, oldValue2);
			}
			function set(uint8 key, uint8 value_a1, uint8 value_a2, uint8 value_b1, uint8 value_b2) public returns (uint8 old_a1, uint8 old_a2, uint8 old_b1, uint8 old_b2) {
				(old_a1, old_a2) = set_internal(a, key, value_a1, value_a2);
				(old_b1, old_b2) = set_internal(b, key, value_b1, value_b2);
			}
			function get(uint8 key) public returns (uint8, uint8, uint8, uint8) {
				return (a[0][key], a[1][key], b[0][key], b[1][key]);
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("set(uint8,uint8,uint8,uint8,uint8)", uint8_t(1), uint8_t(21), uint8_t(22), uint8_t(42), uint8_t(43)), encodeArgs(uint8_t(0), uint8_t(0), uint8_t(0), uint8_t(0)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(1)), encodeArgs(uint8_t(21), uint8_t(22), uint8_t(42), uint8_t(43)));
	ABI_CHECK(callContractFunction("set(uint8,uint8,uint8,uint8,uint8)", uint8_t(1), uint8_t(10), uint8_t(30), uint8_t(11), uint8_t(31)), encodeArgs(uint8_t(21), uint8_t(22), uint8_t(42), uint8_t(43)));
	ABI_CHECK(callContractFunction("get(uint8)", uint8_t(1)), encodeArgs(uint8_t(10), uint8_t(30), uint8_t(11), uint8_t(31)));
}

BOOST_AUTO_TEST_CASE(mapping_internal_return)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint8 => uint8) a;
			mapping(uint8 => uint8) b;
			function f() internal returns (mapping(uint8 => uint8) storage r) {
				r = a;
				r[1] = 42;
				r = b;
				r[1] = 84;
			}
			function g() public returns (uint8, uint8, uint8, uint8, uint8, uint8) {
				f()[2] = 21;
				return (a[0], a[1], a[2], b[0], b[1], b[2]);
			}
			function h() public returns (uint8, uint8, uint8, uint8, uint8, uint8) {
				mapping(uint8 => uint8) storage m = f();
				m[2] = 17;
				return (a[0], a[1], a[2], b[0], b[1], b[2]);
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("g()"), encodeArgs(uint8_t(0), uint8_t(42), uint8_t(0), uint8_t(0), uint8_t(84), uint8_t (21)));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(uint8_t(0), uint8_t(42), uint8_t(0), uint8_t(0), uint8_t(84), uint8_t (17)));
}

BOOST_AUTO_TEST_CASE(structs)
{
	char const* sourceCode = R"(
		contract test {
			struct s1 {
				uint8 x;
				bool y;
			}
			struct s2 {
				uint32 z;
				s1 s1data;
				mapping(uint8 => s2) recursive;
			}
			s2 data;
			function check() public returns (bool ok) {
				return data.z == 1 && data.s1data.x == 2 &&
					data.s1data.y == true &&
					data.recursive[3].recursive[4].z == 5 &&
					data.recursive[4].recursive[3].z == 6 &&
					data.recursive[0].s1data.y == false &&
					data.recursive[4].z == 9;
			}
			function set() public {
				data.z = 1;
				data.s1data.x = 2;
				data.s1data.y = true;
				data.recursive[3].recursive[4].z = 5;
				data.recursive[4].recursive[3].z = 6;
				data.recursive[0].s1data.y = false;
				data.recursive[4].z = 9;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("check()"), encodeArgs(false));
	ABI_CHECK(callContractFunction("set()"), bytes());
	ABI_CHECK(callContractFunction("check()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(struct_reference)
{
	char const* sourceCode = R"(
		contract test {
			struct s2 {
				uint32 z;
				mapping(uint8 => s2) recursive;
			}
			s2 data;
			function check() public returns (bool ok) {
				return data.z == 2 &&
					data.recursive[0].z == 3 &&
					data.recursive[0].recursive[1].z == 0 &&
					data.recursive[0].recursive[0].z == 1;
			}
			function set() public {
				data.z = 2;
				mapping(uint8 => s2) storage map = data.recursive;
				s2 storage inner = map[0];
				inner.z = 3;
				inner.recursive[0].z = inner.recursive[1].z + 1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("check()"), encodeArgs(false));
	ABI_CHECK(callContractFunction("set()"), bytes());
	ABI_CHECK(callContractFunction("check()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(deleteStruct)
{
	char const* sourceCode = R"(
		contract test {
			struct topStruct {
				nestedStruct nstr;
				uint topValue;
				mapping (uint => uint) topMapping;
			}
			uint toDelete;
			topStruct str;
			struct nestedStruct {
				uint nestedValue;
				mapping (uint => bool) nestedMapping;
			}
			constructor() public {
				toDelete = 5;
				str.topValue = 1;
				str.topMapping[0] = 1;
				str.topMapping[1] = 2;

				str.nstr.nestedValue = 2;
				str.nstr.nestedMapping[0] = true;
				str.nstr.nestedMapping[1] = false;
				delete str;
				delete toDelete;
			}
			function getToDelete() public returns (uint res){
				res = toDelete;
			}
			function getTopValue() public returns(uint topValue){
				topValue = str.topValue;
			}
			function getNestedValue() public returns(uint nestedValue){
				nestedValue = str.nstr.nestedValue;
			}
			function getTopMapping(uint index) public returns(uint ret) {
				ret = str.topMapping[index];
			}
			function getNestedMapping(uint index) public returns(bool ret) {
				return str.nstr.nestedMapping[index];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getToDelete()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("getTopValue()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("getNestedValue()"), encodeArgs(0));
	// mapping values should be the same
	ABI_CHECK(callContractFunction("getTopMapping(uint256)", 0), encodeArgs(1));
	ABI_CHECK(callContractFunction("getTopMapping(uint256)", 1), encodeArgs(2));
	ABI_CHECK(callContractFunction("getNestedMapping(uint256)", 0), encodeArgs(true));
	ABI_CHECK(callContractFunction("getNestedMapping(uint256)", 1), encodeArgs(false));
}

BOOST_AUTO_TEST_CASE(deleteLocal)
{
	char const* sourceCode = R"(
		contract test {
			function delLocal() public returns (uint res){
				uint v = 5;
				delete v;
				res = v;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("delLocal()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(deleteLocals)
{
	char const* sourceCode = R"(
		contract test {
			function delLocal() public returns (uint res1, uint res2){
				uint v = 5;
				uint w = 6;
				uint x = 7;
				delete v;
				res1 = w;
				res2 = x;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("delLocal()"), encodeArgs(6, 7));
}

BOOST_AUTO_TEST_CASE(deleteLength)
{
	char const* sourceCode = R"(
		contract test {
			uint[] x;
			function f() public returns (uint){
				x.length = 1;
				x[0] = 1;
				delete x.length;
				return x.length;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(constructor)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint => uint) data;
			constructor() public {
				data[7] = 8;
			}
			function get(uint key) public returns (uint value) {
				return data[key];
			}
		}
	)";
	compileAndRun(sourceCode);
	map<u256, uint8_t> data;
	data[7] = 8;
	auto get = [&](u256 const& _x) -> u256
	{
		return data[_x];
	};
	testContractAgainstCpp("get(uint256)", get, u256(6));
	testContractAgainstCpp("get(uint256)", get, u256(7));
}

BOOST_AUTO_TEST_CASE(simple_accessor)
{
	char const* sourceCode = R"(
		contract test {
			uint256 public data;
			constructor() public {
				data = 8;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("data()"), encodeArgs(8));
}

BOOST_AUTO_TEST_CASE(array_accessor)
{
	char const* sourceCode = R"(
		contract test {
			uint[8] public data;
			uint[] public dynamicData;
			uint24[] public smallTypeData;
			struct st { uint a; uint[] finalArray; }
			mapping(uint256 => mapping(uint256 => st[5])) public multiple_map;

			constructor() public {
				data[0] = 8;
				dynamicData.length = 3;
				dynamicData[2] = 8;
				smallTypeData.length = 128;
				smallTypeData[1] = 22;
				smallTypeData[127] = 2;
				multiple_map[2][1][2].a = 3;
				multiple_map[2][1][2].finalArray.length = 4;
				multiple_map[2][1][2].finalArray[3] = 5;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("data(uint256)", 0), encodeArgs(8));
	ABI_CHECK(callContractFunction("data(uint256)", 8), encodeArgs());
	ABI_CHECK(callContractFunction("dynamicData(uint256)", 2), encodeArgs(8));
	ABI_CHECK(callContractFunction("dynamicData(uint256)", 8), encodeArgs());
	ABI_CHECK(callContractFunction("smallTypeData(uint256)", 1), encodeArgs(22));
	ABI_CHECK(callContractFunction("smallTypeData(uint256)", 127), encodeArgs(2));
	ABI_CHECK(callContractFunction("smallTypeData(uint256)", 128), encodeArgs());
	ABI_CHECK(callContractFunction("multiple_map(uint256,uint256,uint256)", 2, 1, 2), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(accessors_mapping_for_array)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint => uint[8]) public data;
			mapping(uint => uint[]) public dynamicData;
			constructor() public {
				data[2][2] = 8;
				dynamicData[2].length = 3;
				dynamicData[2][2] = 8;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("data(uint256,uint256)", 2, 2), encodeArgs(8));
	ABI_CHECK(callContractFunction("data(uint256, 256)", 2, 8), encodeArgs());
	ABI_CHECK(callContractFunction("dynamicData(uint256,uint256)", 2, 2), encodeArgs(8));
	ABI_CHECK(callContractFunction("dynamicData(uint256,uint256)", 2, 8), encodeArgs());
}

BOOST_AUTO_TEST_CASE(multiple_elementary_accessors)
{
	char const* sourceCode = R"(
		contract test {
			uint256 public data;
			bytes6 public name;
			bytes32 public a_hash;
			address public an_address;
			constructor() public {
				data = 8;
				name = "Celina";
				a_hash = keccak256("\x7b");
				an_address = address(0x1337);
				super_secret_data = 42;
			}
			uint256 super_secret_data;
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("data()"), encodeArgs(8));
	ABI_CHECK(callContractFunction("name()"), encodeArgs("Celina"));
	ABI_CHECK(callContractFunction("a_hash()"), encodeArgs(dev::keccak256(bytes(1, 0x7b))));
	ABI_CHECK(callContractFunction("an_address()"), encodeArgs(toBigEndian(u160(0x1337))));
	ABI_CHECK(callContractFunction("super_secret_data()"), bytes());
}

BOOST_AUTO_TEST_CASE(complex_accessors)
{
	char const* sourceCode = R"(
		contract test {
			mapping(uint256 => bytes4) public to_string_map;
			mapping(uint256 => bool) public to_bool_map;
			mapping(uint256 => uint256) public to_uint_map;
			mapping(uint256 => mapping(uint256 => uint256)) public to_multiple_map;
			constructor() public {
				to_string_map[42] = "24";
				to_bool_map[42] = false;
				to_uint_map[42] = 12;
				to_multiple_map[42][23] = 31;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("to_string_map(uint256)", 42), encodeArgs("24"));
	ABI_CHECK(callContractFunction("to_bool_map(uint256)", 42), encodeArgs(false));
	ABI_CHECK(callContractFunction("to_uint_map(uint256)", 42), encodeArgs(12));
	ABI_CHECK(callContractFunction("to_multiple_map(uint256,uint256)", 42, 23), encodeArgs(31));
}

BOOST_AUTO_TEST_CASE(struct_accessor)
{
	char const* sourceCode = R"(
		contract test {
			struct Data { uint a; uint8 b; mapping(uint => uint) c; bool d; }
			mapping(uint => Data) public data;
			constructor() public {
				data[7].a = 1;
				data[7].b = 2;
				data[7].c[0] = 3;
				data[7].d = true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("data(uint256)", 7), encodeArgs(1, 2, true));
}

BOOST_AUTO_TEST_CASE(balance)
{
	char const* sourceCode = R"(
		contract test {
			constructor() public payable {}
			function getBalance() public returns (uint256 balance) {
				return address(this).balance;
			}
		}
	)";
	compileAndRun(sourceCode, 23);
	ABI_CHECK(callContractFunction("getBalance()"), encodeArgs(23));
}

BOOST_AUTO_TEST_CASE(blockchain)
{
	char const* sourceCode = R"(
		contract test {
			constructor() public payable {}
			function someInfo() public payable returns (uint256 value, address coinbase, uint256 blockNumber) {
				value = msg.value;
				coinbase = block.coinbase;
				blockNumber = block.number;
			}
		}
	)";
	BOOST_CHECK(m_rpc.rpcCall("miner_setEtherbase", {"\"0x1212121212121212121212121212121212121212\""}).asBool() == true);
	m_rpc.test_mineBlocks(5);
	compileAndRun(sourceCode, 27);
	ABI_CHECK(callContractFunctionWithValue("someInfo()", 28), encodeArgs(28, u256("0x1212121212121212121212121212121212121212"), 7));
}

BOOST_AUTO_TEST_CASE(msg_sig)
{
	char const* sourceCode = R"(
		contract test {
			function foo(uint256 a) public returns (bytes4 value) {
				return msg.sig;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("foo(uint256)", 0), encodeArgs(asString(FixedHash<4>(dev::keccak256("foo(uint256)")).asBytes())));
}

BOOST_AUTO_TEST_CASE(msg_sig_after_internal_call_is_same)
{
	char const* sourceCode = R"(
		contract test {
			function boo() public returns (bytes4 value) {
				return msg.sig;
			}
			function foo(uint256 a) public returns (bytes4 value) {
				return boo();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("foo(uint256)", 0), encodeArgs(asString(FixedHash<4>(dev::keccak256("foo(uint256)")).asBytes())));
}

BOOST_AUTO_TEST_CASE(now)
{
	char const* sourceCode = R"(
		contract test {
			function someInfo() public returns (bool equal, uint val) {
				equal = block.timestamp == now;
				val = now;
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 startBlock = m_blockNumber;
	size_t startTime = blockTimestamp(startBlock);
	auto ret = callContractFunction("someInfo()");
	u256 endBlock = m_blockNumber;
	size_t endTime = blockTimestamp(endBlock);
	BOOST_CHECK(startBlock != endBlock);
	BOOST_CHECK(startTime != endTime);
	ABI_CHECK(ret, encodeArgs(true, endTime));
}

BOOST_AUTO_TEST_CASE(type_conversions_cleanup)
{
	// 22-byte integer converted to a contract (i.e. address, 20 bytes), converted to a 32 byte
	// integer should drop the first two bytes
	char const* sourceCode = R"(
		contract Test {
			function test() public returns (uint ret) { return uint(address(Test(address(0x11223344556677889900112233445566778899001122)))); }
		}
	)";
	compileAndRun(sourceCode);
	BOOST_REQUIRE(callContractFunction("test()") == bytes({0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
														   0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0x00, 0x11, 0x22,
														   0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0x00, 0x11, 0x22}));
}
// fixed bytes to fixed bytes conversion tests
BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_fixed_bytes_smaller_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToBytes(bytes4 input) public returns (bytes2 ret) {
				return bytes2(input);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("bytesToBytes(bytes4)", "abcd"), encodeArgs("ab"));
}

BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_fixed_bytes_greater_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToBytes(bytes2 input) public returns (bytes4 ret) {
				return bytes4(input);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("bytesToBytes(bytes2)", "ab"), encodeArgs("ab"));
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
