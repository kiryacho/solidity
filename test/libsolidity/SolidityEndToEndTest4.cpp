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

BOOST_AUTO_TEST_CASE(byte_array_pop_isolated)
{
	char const* sourceCode = R"(
		// This tests that the compiler knows the correct size of the function on the stack.
		contract c {
			bytes data;
			function test() public returns (uint x) {
				x = 2;
				data.pop;
				x = 3;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(external_array_args)
{
	char const* sourceCode = R"(
		contract c {
			function test(uint[8] calldata a, uint[] calldata b, uint[5] calldata c, uint a_index, uint b_index, uint c_index)
					external returns (uint av, uint bv, uint cv) {
				av = a[a_index];
				bv = b[b_index];
				cv = c[c_index];
			}
		}
	)";
	compileAndRun(sourceCode);
	bytes params = encodeArgs(
		1, 2, 3, 4, 5, 6, 7, 8, // a
		32 * (8 + 1 + 5 + 1 + 1 + 1), // offset to b
		21, 22, 23, 24, 25, // c
		0, 1, 2, // (a,b,c)_index
		3, // b.length
		11, 12, 13 // b
		);
	ABI_CHECK(callContractFunction("test(uint256[8],uint256[],uint256[5],uint256,uint256,uint256)", params), encodeArgs(1, 12, 23));
}

BOOST_AUTO_TEST_CASE(bytes_index_access)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function direct(bytes calldata arg, uint index) external returns (uint) {
				return uint(uint8(arg[index]));
			}
			function storageCopyRead(bytes calldata arg, uint index) external returns (uint) {
				data = arg;
				return uint(uint8(data[index]));
			}
			function storageWrite() external returns (uint) {
				data.length = 35;
				data[31] = 0x77;
				data[32] = 0x14;

				data[31] = 0x01;
				data[31] |= 0x08;
				data[30] = 0x01;
				data[32] = 0x03;
				return uint(uint8(data[30])) * 0x100 | uint(uint8(data[31])) * 0x10 | uint(uint8(data[32]));
			}
		}
	)";
	compileAndRun(sourceCode);
	string array{
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
		10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
		20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
		30, 31, 32, 33};
	ABI_CHECK(callContractFunction("direct(bytes,uint256)", 64, 33, u256(array.length()), array), encodeArgs(33));
	ABI_CHECK(callContractFunction("storageCopyRead(bytes,uint256)", 64, 33, u256(array.length()), array), encodeArgs(33));
	ABI_CHECK(callContractFunction("storageWrite()"), encodeArgs(0x193));
}

BOOST_AUTO_TEST_CASE(bytes_delete_element)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test1() external returns (bool) {
				data.length = 100;
				for (uint i = 0; i < data.length; i++)
					data[i] = byte(uint8(i));
				delete data[94];
				delete data[96];
				delete data[98];
				return data[94] == 0 && uint8(data[95]) == 95 && data[96] == 0 && uint8(data[97]) == 97;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test1()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(array_copy_calldata_storage)
{
	char const* sourceCode = R"(
		contract c {
			uint[9] m_data;
			uint[] m_data_dyn;
			uint8[][] m_byte_data;
			function store(uint[9] calldata a, uint8[3][] calldata b) external returns (uint8) {
				m_data = a;
				m_data_dyn = a;
				m_byte_data = b;
				return b[3][1]; // note that access and declaration are reversed to each other
			}
			function retrieve() public returns (uint a, uint b, uint c, uint d, uint e, uint f, uint g) {
				a = m_data.length;
				b = m_data[7];
				c = m_data_dyn.length;
				d = m_data_dyn[7];
				e = m_byte_data.length;
				f = m_byte_data[3].length;
				g = m_byte_data[3][1];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("store(uint256[9],uint8[3][])", encodeArgs(
		21, 22, 23, 24, 25, 26, 27, 28, 29, // a
		u256(32 * (9 + 1)),
		4, // size of b
		1, 2, 3, // b[0]
		11, 12, 13, // b[1]
		21, 22, 23, // b[2]
		31, 32, 33 // b[3]
	)), encodeArgs(32));
	ABI_CHECK(callContractFunction("retrieve()"), encodeArgs(
		9, 28, 9, 28,
		4, 3, 32));
}

BOOST_AUTO_TEST_CASE(array_copy_nested_array)
{
	char const* sourceCode = R"(
		contract c {
			uint[4][] a;
			uint[10][] b;
			uint[][] c;
			function test(uint[2][] calldata d) external returns (uint) {
				a = d;
				b = a;
				c = b;
				return c[1][1] | c[1][2] | c[1][3] | c[1][4];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test(uint256[2][])", encodeArgs(
		32, 3,
		7, 8,
		9, 10,
		11, 12
	)), encodeArgs(10));
}

BOOST_AUTO_TEST_CASE(array_copy_including_mapping)
{
	char const* sourceCode = R"(
		contract c {
			mapping(uint=>uint)[90][] large;
			mapping(uint=>uint)[3][] small;
			function test() public returns (uint r) {
				large.length = small.length = 7;
				large[3][2][0] = 2;
				large[1] = large[3];
				small[3][2][0] = 2;
				small[1] = small[2];
				r = ((
					small[3][2][0] * 0x100 |
					small[1][2][0]) * 0x100 |
					large[3][2][0]) * 0x100 |
					large[1][2][0];
				delete small;
				delete large;
			}
			function clear() public returns (uint r) {
				large.length = small.length = 7;
				small[3][2][0] = 0;
				large[3][2][0] = 0;
				small.length = large.length = 0;
				return 7;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(0x02000200));
	// storage is not empty because we cannot delete the mappings
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("clear()"), encodeArgs(7));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(swap_in_storage_overwrite)
{
	// This tests a swap in storage which does not work as one
	// might expect because we do not have temporary storage.
	// (x, y) = (y, x) is the same as
	// y = x;
	// x = y;
	char const* sourceCode = R"(
		contract c {
			struct S { uint a; uint b; }
			S public x;
			S public y;
			function set() public {
				x.a = 1; x.b = 2;
				y.a = 3; y.b = 4;
			}
			function swap() public {
				(x, y) = (y, x);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0), u256(0)));
	ABI_CHECK(callContractFunction("y()"), encodeArgs(u256(0), u256(0)));
	ABI_CHECK(callContractFunction("set()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1), u256(2)));
	ABI_CHECK(callContractFunction("y()"), encodeArgs(u256(3), u256(4)));
	ABI_CHECK(callContractFunction("swap()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1), u256(2)));
	ABI_CHECK(callContractFunction("y()"), encodeArgs(u256(1), u256(2)));
}

BOOST_AUTO_TEST_CASE(pass_dynamic_arguments_to_the_base)
{
	char const* sourceCode = R"(
		contract Base {
			constructor(uint i) public
			{
				m_i = i;
			}
			uint public m_i;
		}
		contract Derived is Base {
			constructor(uint i) Base(i) public
			{}
		}
		contract Final is Derived(4) {
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("m_i()"), encodeArgs(4));
}

BOOST_AUTO_TEST_CASE(pass_dynamic_arguments_to_the_base_base)
{
	char const* sourceCode = R"(
		contract Base {
			constructor(uint j) public
			{
				m_i = j;
			}
			uint public m_i;
		}
		contract Base1 is Base {
			constructor(uint k) Base(k) public {}
		}
		contract Derived is Base, Base1 {
			constructor(uint i) Base1(i) public
			{}
		}
		contract Final is Derived(4) {
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("m_i()"), encodeArgs(4));
}

BOOST_AUTO_TEST_CASE(pass_dynamic_arguments_to_the_base_base_with_gap)
{
	char const* sourceCode = R"(
		contract Base {
			constructor(uint i) public
			{
				m_i = i;
			}
			uint public m_i;
		}
		contract Base1 is Base {
			constructor(uint k) public {}
		}
		contract Derived is Base, Base1 {
			constructor(uint i) Base(i) Base1(7) public {}
		}
		contract Final is Derived(4) {
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("m_i()"), encodeArgs(4));
}

BOOST_AUTO_TEST_CASE(simple_constant_variables_test)
{
	char const* sourceCode = R"(
		contract Foo {
			function getX() public returns (uint r) { return x; }
			uint constant x = 56;
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getX()"), encodeArgs(56));
}

BOOST_AUTO_TEST_CASE(constant_variables)
{
	char const* sourceCode = R"(
		contract Foo {
			uint constant x = 56;
			enum ActionChoices { GoLeft, GoRight, GoStraight, Sit }
			ActionChoices constant choices = ActionChoices.GoLeft;
			bytes32 constant st = "abc\x00\xff__";
		}
	)";
	compileAndRun(sourceCode);
}

BOOST_AUTO_TEST_CASE(assignment_to_const_var_involving_expression)
{
	char const* sourceCode = R"(
		contract C {
			uint constant x = 0x123 + 0x456;
			function f() public returns (uint) { return x + 1; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0x123 + 0x456 + 1));
}

BOOST_AUTO_TEST_CASE(assignment_to_const_var_involving_keccak)
{
	char const* sourceCode = R"(
		contract C {
			bytes32 constant x = keccak256("abc");
			function f() public returns (bytes32) { return x; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(dev::keccak256("abc")));
}

// Disabled until https://github.com/ethereum/solidity/issues/715 is implemented
//BOOST_AUTO_TEST_CASE(assignment_to_const_array_vars)
//{
//	char const* sourceCode = R"(
//		contract C {
//			uint[3] constant x = [uint(1), 2, 3];
//			uint constant y = x[0] + x[1] + x[2];
//			function f() public returns (uint) { return y; }
//		}
//	)";
//	compileAndRun(sourceCode);
//	ABI_CHECK(callContractFunction("f()"), encodeArgs(1 + 2 + 3));
//}

// Disabled until https://github.com/ethereum/solidity/issues/715 is implemented
//BOOST_AUTO_TEST_CASE(constant_struct)
//{
//	char const* sourceCode = R"(
//		contract C {
//			struct S { uint x; uint[] y; }
//			S constant x = S(5, new uint[](4));
//			function f() public returns (uint) { return x.x; }
//		}
//	)";
//	compileAndRun(sourceCode);
//	ABI_CHECK(callContractFunction("f()"), encodeArgs(5));
//}

BOOST_AUTO_TEST_CASE(packed_storage_structs_uint)
{
	char const* sourceCode = R"(
		contract C {
			struct str { uint8 a; uint16 b; uint248 c; }
			str data;
			function test() public returns (uint) {
				data.a = 2;
				if (data.a != 2) return 2;
				data.b = 0xabcd;
				if (data.b != 0xabcd) return 3;
				data.c = 0x1234567890;
				if (data.c != 0x1234567890) return 4;
				if (data.a != 2) return 5;
				data.a = 8;
				if (data.a != 8) return 6;
				if (data.b != 0xabcd) return 7;
				data.b = 0xdcab;
				if (data.b != 0xdcab) return 8;
				if (data.c != 0x1234567890) return 9;
				data.c = 0x9876543210;
				if (data.c != 0x9876543210) return 10;
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(packed_storage_structs_enum)
{
	char const* sourceCode = R"(
		contract C {
			enum small { A, B, C, D }
			enum larger { A, B, C, D, E}
			struct str { small a; small b; larger c; larger d; }
			str data;
			function test() public returns (uint) {
				data.a = small.B;
				if (data.a != small.B) return 2;
				data.b = small.C;
				if (data.b != small.C) return 3;
				data.c = larger.D;
				if (data.c != larger.D) return 4;
				if (data.a != small.B) return 5;
				data.a = small.C;
				if (data.a != small.C) return 6;
				if (data.b != small.C) return 7;
				data.b = small.D;
				if (data.b != small.D) return 8;
				if (data.c != larger.D) return 9;
				data.c = larger.B;
				if (data.c != larger.B) return 10;
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(packed_storage_structs_bytes)
{
	char const* sourceCode = R"(
		contract C {
			struct s1 { byte a; byte b; bytes10 c; bytes9 d; bytes10 e; }
			struct s2 { byte a; s1 inner; byte b; byte c; }
			byte x;
			s2 data;
			byte y;
			function test() public returns (bool) {
				x = 0x01;
				data.a = 0x02;
				data.inner.a = 0x03;
				data.inner.b = 0x04;
				data.inner.c = "1234567890";
				data.inner.d = "123456789";
				data.inner.e = "abcdefghij";
				data.b = 0x05;
				data.c = byte(0x06);
				y = 0x07;
				return x == 0x01 && data.a == 0x02 && data.inner.a == 0x03 && data.inner.b == 0x04 &&
					data.inner.c == "1234567890" && data.inner.d == "123456789" &&
					data.inner.e == "abcdefghij" && data.b == 0x05 && data.c == byte(0x06) && y == 0x07;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(packed_storage_structs_delete)
{
	char const* sourceCode = R"(
		contract C {
			struct str { uint8 a; uint16 b; uint8 c; }
			uint8 x;
			uint16 y;
			str data;
			function test() public returns (uint) {
				x = 1;
				y = 2;
				data.a = 2;
				data.b = 0xabcd;
				data.c = 0xfa;
				if (x != 1 || y != 2 || data.a != 2 || data.b != 0xabcd || data.c != 0xfa)
					return 2;
				delete y;
				delete data.b;
				if (x != 1 || y != 0 || data.a != 2 || data.b != 0 || data.c != 0xfa)
					return 3;
				delete x;
				delete data;
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(overloaded_function_call_resolve_to_first)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint k) public returns(uint d) { return k; }
			function f(uint a, uint b) public returns(uint d) { return a + b; }
			function g() public returns(uint d) { return f(3); }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("g()"), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(overloaded_function_call_resolve_to_second)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint a, uint b) public returns(uint d) { return a + b; }
			function f(uint k) public returns(uint d) { return k; }
			function g() public returns(uint d) { return f(3, 7); }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("g()"), encodeArgs(10));
}

BOOST_AUTO_TEST_CASE(overloaded_function_call_with_if_else)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint a, uint b) public returns(uint d) { return a + b; }
			function f(uint k) public returns(uint d) { return k; }
			function g(bool flag) public returns(uint d) {
				if (flag)
					return f(3);
				else
					return f(3, 7);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("g(bool)", true), encodeArgs(3));
	ABI_CHECK(callContractFunction("g(bool)", false), encodeArgs(10));
}

BOOST_AUTO_TEST_CASE(derived_overload_base_function_direct)
{
	char const* sourceCode = R"(
		contract B { function f() public returns(uint) { return 10; } }
		contract C is B {
			function f(uint i) public returns(uint) { return 2 * i; }
			function g() public returns(uint) { return f(1); }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(derived_overload_base_function_indirect)
{
	char const* sourceCode = R"(
		contract A { function f(uint a) public returns(uint) { return 2 * a; } }
		contract B { function f() public returns(uint) { return 10; } }
		contract C is A, B {
			function g() public returns(uint) { return f(); }
			function h() public returns(uint) { return f(1); }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(10));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(super_overload)
{
	char const* sourceCode = R"(
		contract A { function f(uint a) public returns(uint) { return 2 * a; } }
		contract B { function f(bool b) public returns(uint) { return 10; } }
		contract C is A, B {
			function g() public returns(uint) { return super.f(true); }
			function h() public returns(uint) { return super.f(1); }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(10));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(gasleft_shadow_resolution)
{
	char const* sourceCode = R"(
		contract C {
			function gasleft() public returns(uint256) { return 0; }
			function f() public returns(uint256) { return gasleft(); }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(bool_conversion)
{
	char const* sourceCode = R"(
		contract C {
			function f(bool _b) public returns(uint) {
				if (_b)
					return 1;
				else
					return 0;
			}
			function g(bool _in) public returns (bool _out) {
				_out = _in;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(bool)", 0), encodeArgs(0));
	ABI_CHECK(callContractFunction("f(bool)", 1), encodeArgs(1));
	ABI_CHECK(callContractFunction("f(bool)", 2), v2 ? encodeArgs() : encodeArgs(1));
	ABI_CHECK(callContractFunction("f(bool)", 3), v2 ? encodeArgs() : encodeArgs(1));
	ABI_CHECK(callContractFunction("f(bool)", 255), v2 ? encodeArgs() : encodeArgs(1));
	ABI_CHECK(callContractFunction("g(bool)", 0), encodeArgs(0));
	ABI_CHECK(callContractFunction("g(bool)", 1), encodeArgs(1));
	ABI_CHECK(callContractFunction("g(bool)", 2), v2 ? encodeArgs() : encodeArgs(1));
	ABI_CHECK(callContractFunction("g(bool)", 3), v2 ? encodeArgs() : encodeArgs(1));
	ABI_CHECK(callContractFunction("g(bool)", 255), v2 ? encodeArgs() : encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(packed_storage_signed)
{
	char const* sourceCode = R"(
		contract C {
			int8 a;
			uint8 b;
			int8 c;
			uint8 d;
			function test() public returns (uint x1, uint x2, uint x3, uint x4) {
				a = -2;
				b = -uint8(a) * 2;
				c = a * int8(120) * int8(121);
				x1 = uint(a);
				x2 = b;
				x3 = uint(c);
				x4 = d;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(-2), u256(4), u256(-112), u256(0)));
}

BOOST_AUTO_TEST_CASE(external_types_in_calls)
{
	char const* sourceCode = R"(
		contract C1 { C1 public bla; constructor(C1 x) public { bla = x; } }
		contract C {
			function test() public returns (C1 x, C1 y) {
				C1 c = new C1(C1(9));
				x = c.bla();
				y = this.t1(C1(7));
			}
			function t1(C1 a) public returns (C1) { return a; }
			function t2() public returns (C1) { return C1(9); }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(9), u256(7)));
	ABI_CHECK(callContractFunction("t2()"), encodeArgs(u256(9)));
}

BOOST_AUTO_TEST_CASE(invalid_enum_compared)
{
	char const* sourceCode = R"(
		contract C {
			enum X { A, B }

			function test_eq() public returns (bool) {
				X garbled;
				assembly {
					garbled := 5
				}
				return garbled == garbled;
			}
			function test_eq_ok() public returns (bool) {
				X garbled = X.A;
				return garbled == garbled;
			}
			function test_neq() public returns (bool) {
				X garbled;
				assembly {
					garbled := 5
				}
				return garbled != garbled;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test_eq_ok()"), encodeArgs(u256(1)));
	// both should throw
	ABI_CHECK(callContractFunction("test_eq()"), encodeArgs());
	ABI_CHECK(callContractFunction("test_neq()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(invalid_enum_logged)
{
	char const* sourceCode = R"(
		contract C {
			enum X { A, B }
			event Log(X);

			function test_log() public returns (uint) {
				X garbled = X.A;
				assembly {
					garbled := 5
				}
				emit Log(garbled);
				return 1;
			}
			function test_log_ok() public returns (uint) {
				X x = X.A;
				emit Log(x);
				return 1;
			}
		}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test_log_ok()"), encodeArgs(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_REQUIRE_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Log(uint8)")));
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(0)));

	// should throw
	ABI_CHECK(callContractFunction("test_log()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(invalid_enum_stored)
{
	char const* sourceCode = R"(
		contract C {
			enum X { A, B }
			X public x;

			function test_store() public returns (uint) {
				X garbled = X.A;
				assembly {
					garbled := 5
				}
				x = garbled;
				return 1;
			}
			function test_store_ok() public returns (uint) {
				x = X.A;
				return 1;
			}
		}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test_store_ok()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));

	// should throw
	ABI_CHECK(callContractFunction("test_store()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(invalid_enum_as_external_ret)
{
	char const* sourceCode = R"(
		contract C {
			enum X { A, B }

			function test_return() public returns (X) {
				X garbled;
				assembly {
					garbled := 5
				}
				return garbled;
			}
			function test_inline_assignment() public returns (X _ret) {
				assembly {
					_ret := 5
				}
			}
			function test_assignment() public returns (X _ret) {
				X tmp;
				assembly {
					tmp := 5
				}
				_ret = tmp;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	// both should throw
	ABI_CHECK(callContractFunction("test_return()"), encodeArgs());
	ABI_CHECK(callContractFunction("test_inline_assignment()"), encodeArgs());
	ABI_CHECK(callContractFunction("test_assignment()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(invalid_enum_as_external_arg)
{
	char const* sourceCode = R"(
		contract C {
			enum X { A, B }

			function tested (X x) public returns (uint) {
				return 1;
			}

			function test() public returns (uint) {
				X garbled;

				assembly {
					garbled := 5
				}

				return this.tested(garbled);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	// should throw
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
}


BOOST_AUTO_TEST_CASE(proper_order_of_overwriting_of_attributes)
{
	// bug #1798
	char const* sourceCode = R"(
		contract init {
			function isOk() public returns (bool) { return false; }
			bool public ok = false;
		}
		contract fix {
			function isOk() public returns (bool) { return true; }
			bool public ok = true;
		}

		contract init_fix is init, fix {
			function checkOk() public returns (bool) { return ok; }
		}
		contract fix_init is fix, init {
			function checkOk() public returns (bool) { return ok; }
		}
	)";
	compileAndRun(sourceCode, 0, "init_fix");
	ABI_CHECK(callContractFunction("isOk()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("ok()"), encodeArgs(true));

	compileAndRun(sourceCode, 0, "fix_init");
	ABI_CHECK(callContractFunction("isOk()"), encodeArgs(false));
	ABI_CHECK(callContractFunction("ok()"), encodeArgs(false));
}

BOOST_AUTO_TEST_CASE(struct_assign_reference_to_struct)
{
	char const* sourceCode = R"(
		contract test {
			struct testStruct
			{
				uint m_value;
			}
			testStruct data1;
			testStruct data2;
			testStruct data3;
			constructor() public
			{
				data1.m_value = 2;
			}
			function assign() public returns (uint ret_local, uint ret_global, uint ret_global3, uint ret_global1)
			{
				testStruct storage x = data1; //x is a reference data1.m_value == 2 as well as x.m_value = 2
				data2 = data1; // should copy data. data2.m_value == 2

				ret_local = x.m_value; // = 2
				ret_global = data2.m_value; // = 2

				x.m_value = 3;
				data3 = x; //should copy the data. data3.m_value == 3
				ret_global3 = data3.m_value; // = 3
				ret_global1 = data1.m_value; // = 3. Changed due to the assignment to x.m_value
			}
		}
	)";
	compileAndRun(sourceCode, 0, "test");
	ABI_CHECK(callContractFunction("assign()"), encodeArgs(2, 2, 3, 3));
}

BOOST_AUTO_TEST_CASE(struct_delete_member)
{
	char const* sourceCode = R"(
		contract test {
			struct testStruct
			{
				uint m_value;
			}
			testStruct data1;
			constructor() public
			{
				data1.m_value = 2;
			}
			function deleteMember() public returns (uint ret_value)
			{
				testStruct storage x = data1; //should not copy the data. data1.m_value == 2 but x.m_value = 0
				x.m_value = 4;
				delete x.m_value;
				ret_value = data1.m_value;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "test");
	ABI_CHECK(callContractFunction("deleteMember()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(struct_delete_struct_in_mapping)
{
	char const* sourceCode = R"(
		contract test {
			struct testStruct
			{
				uint m_value;
			}
			mapping (uint => testStruct) campaigns;

			constructor() public
			{
				campaigns[0].m_value = 2;
			}
			function deleteIt() public returns (uint)
			{
				delete campaigns[0];
				return campaigns[0].m_value;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "test");
	ABI_CHECK(callContractFunction("deleteIt()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(evm_exceptions_out_of_band_access)
{
	char const* sourceCode = R"(
		contract A {
			uint[3] arr;
			bool public test = false;
			function getElement(uint i) public returns (uint)
			{
				return arr[i];
			}
			function testIt() public returns (bool)
			{
				uint i = this.getElement(5);
				test = true;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "A");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(false));
	ABI_CHECK(callContractFunction("testIt()"), encodeArgs());
	ABI_CHECK(callContractFunction("test()"), encodeArgs(false));
}

BOOST_AUTO_TEST_CASE(evm_exceptions_in_constructor_call_fail)
{
	char const* sourceCode = R"(
		contract A {
			constructor() public
			{
				address(this).call("123");
			}
		}
		contract B {
			uint public test = 1;
			function testIt() public
			{
				A a = new A();
				++test;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "B");

	ABI_CHECK(callContractFunction("testIt()"), encodeArgs());
	ABI_CHECK(callContractFunction("test()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(evm_exceptions_in_constructor_out_of_baund)
{
	char const* sourceCode = R"(
		contract A {
			uint public test = 1;
			uint[3] arr;
			constructor() public
			{
				uint index = 5;
				test = arr[index];
				++test;
			}
		}
	)";
	ABI_CHECK(compileAndRunWithoutCheck(sourceCode, 0, "A"), encodeArgs());
	BOOST_CHECK(!m_transactionSuccessful);
}

BOOST_AUTO_TEST_CASE(positive_integers_to_signed)
{
	char const* sourceCode = R"(
		contract test {
			int8 public x = 2;
			int8 public y = 127;
			int16 public q = 250;
		}
	)";
	compileAndRun(sourceCode, 0, "test");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(2));
	ABI_CHECK(callContractFunction("y()"), encodeArgs(127));
	ABI_CHECK(callContractFunction("q()"), encodeArgs(250));
}

BOOST_AUTO_TEST_CASE(failing_send)
{
	char const* sourceCode = R"(
		contract Helper {
			uint[] data;
			function () external {
				data[9]; // trigger exception
			}
		}
		contract Main {
			constructor() public payable {}
			function callHelper(address payable _a) public returns (bool r, uint bal) {
				r = !_a.send(5);
				bal = address(this).balance;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 20, "Main");
	BOOST_REQUIRE(callContractFunction("callHelper(address)", c_helperAddress) == encodeArgs(true, 20));
}

BOOST_AUTO_TEST_CASE(send_zero_ether)
{
	// Sending zero ether to a contract should still invoke the fallback function
	// (it previously did not because the gas stipend was not provided by the EVM)
	char const* sourceCode = R"(
		contract Receiver {
			function () external payable {
			}
		}
		contract Main {
			constructor() public payable {}
			function s() public returns (bool) {
				Receiver r = new Receiver();
				return address(r).send(0);
			}
		}
	)";
	compileAndRun(sourceCode, 20, "Main");
	BOOST_REQUIRE(callContractFunction("s()") == encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(reusing_memory)
{
	// Invoke some features that use memory and test that they do not interfere with each other.
	char const* sourceCode = R"(
		contract Helper {
			uint public flag;
			constructor(uint x) public {
				flag = x;
			}
		}
		contract Main {
			mapping(uint => uint) map;
			function f(uint x) public returns (uint) {
				map[x] = x;
				return (new Helper(uint(keccak256(abi.encodePacked(this.g(map[x])))))).flag();
			}
			function g(uint a) public returns (uint)
			{
				return map[a];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("f(uint256)", 0x34) == encodeArgs(dev::keccak256(dev::toBigEndian(u256(0x34)))));
}

BOOST_AUTO_TEST_CASE(return_string)
{
	char const* sourceCode = R"(
		contract Main {
			string public s;
			function set(string calldata _s) external {
				s = _s;
			}
			function get1() public returns (string memory r) {
				return s;
			}
			function get2() public returns (string memory r) {
				r = s;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	string s("Julia");
	bytes args = encodeArgs(u256(0x20), u256(s.length()), s);
	BOOST_REQUIRE(callContractFunction("set(string)", asString(args)) == encodeArgs());
	ABI_CHECK(callContractFunction("get1()"), args);
	ABI_CHECK(callContractFunction("get2()"), args);
	ABI_CHECK(callContractFunction("s()"), args);
}

BOOST_AUTO_TEST_CASE(return_multiple_strings_of_various_sizes)
{
	char const* sourceCode = R"(
		contract Main {
			string public s1;
			string public s2;
			function set(string calldata _s1, uint x, string calldata _s2) external returns (uint) {
				s1 = _s1;
				s2 = _s2;
				return x;
			}
			function get() public returns (string memory r1, string memory r2) {
				r1 = s1;
				r2 = s2;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	string s1(
		"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
		"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
		"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
		"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
	);
	string s2(
		"ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ"
		"ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ"
		"ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ"
		"ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ"
		"ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ"
	);
	vector<size_t> lengths{0, 30, 32, 63, 64, 65, 210, 300};
	for (auto l1: lengths)
		for (auto l2: lengths)
		{
			bytes dyn1 = encodeArgs(u256(l1), s1.substr(0, l1));
			bytes dyn2 = encodeArgs(u256(l2), s2.substr(0, l2));
			bytes args = encodeArgs(u256(0x60), u256(l1), u256(0x60 + dyn1.size())) + dyn1 + dyn2;
			BOOST_REQUIRE(
				callContractFunction("set(string,uint256,string)", asString(args)) ==
				encodeArgs(u256(l1))
			);
			bytes result = encodeArgs(u256(0x40), u256(0x40 + dyn1.size())) + dyn1 + dyn2;
			ABI_CHECK(callContractFunction("get()"), result);
			ABI_CHECK(callContractFunction("s1()"), encodeArgs(0x20) + dyn1);
			ABI_CHECK(callContractFunction("s2()"), encodeArgs(0x20) + dyn2);
		}
}

BOOST_AUTO_TEST_CASE(accessor_involving_strings)
{
	char const* sourceCode = R"(
		contract Main {
			struct stringData { string a; uint b; string c; }
			mapping(uint => stringData[]) public data;
			function set(uint x, uint y, string calldata a, uint b, string calldata c) external returns (bool) {
				data[x].length = y + 1;
				data[x][y].a = a;
				data[x][y].b = b;
				data[x][y].c = c;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	string s1("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	string s2("ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ");
	bytes s1Data = encodeArgs(u256(s1.length()), s1);
	bytes s2Data = encodeArgs(u256(s2.length()), s2);
	u256 b = 765;
	u256 x = 7;
	u256 y = 123;
	bytes args = encodeArgs(x, y, u256(0xa0), b, u256(0xa0 + s1Data.size()), s1Data, s2Data);
	bytes result = encodeArgs(u256(0x60), b, u256(0x60 + s1Data.size()), s1Data, s2Data);
	BOOST_REQUIRE(callContractFunction("set(uint256,uint256,string,uint256,string)", asString(args)) == encodeArgs(true));
	BOOST_REQUIRE(callContractFunction("data(uint256,uint256)", x, y) == result);
}

BOOST_AUTO_TEST_CASE(bytes_in_function_calls)
{
	char const* sourceCode = R"(
		contract Main {
			string public s1;
			string public s2;
			function set(string memory _s1, uint x, string memory _s2) public returns (uint) {
				s1 = _s1;
				s2 = _s2;
				return x;
			}
			function setIndirectFromMemory(string memory _s1, uint x, string memory _s2) public returns (uint) {
				return this.set(_s1, x, _s2);
			}
			function setIndirectFromCalldata(string calldata _s1, uint x, string calldata _s2) external returns (uint) {
				return this.set(_s1, x, _s2);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	string s1("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	string s2("ABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZABCDEFGHIJKLMNOPQRSTUVXYZ");
	vector<size_t> lengths{0, 31, 64, 65};
	for (auto l1: lengths)
		for (auto l2: lengths)
		{
			bytes dyn1 = encodeArgs(u256(l1), s1.substr(0, l1));
			bytes dyn2 = encodeArgs(u256(l2), s2.substr(0, l2));
			bytes args1 = encodeArgs(u256(0x60), u256(l1), u256(0x60 + dyn1.size())) + dyn1 + dyn2;
			BOOST_REQUIRE(
				callContractFunction("setIndirectFromMemory(string,uint256,string)", asString(args1)) ==
				encodeArgs(u256(l1))
			);
			ABI_CHECK(callContractFunction("s1()"), encodeArgs(0x20) + dyn1);
			ABI_CHECK(callContractFunction("s2()"), encodeArgs(0x20) + dyn2);
			// swapped
			bytes args2 = encodeArgs(u256(0x60), u256(l1), u256(0x60 + dyn2.size())) + dyn2 + dyn1;
			BOOST_REQUIRE(
				callContractFunction("setIndirectFromCalldata(string,uint256,string)", asString(args2)) ==
				encodeArgs(u256(l1))
			);
			ABI_CHECK(callContractFunction("s1()"), encodeArgs(0x20) + dyn2);
			ABI_CHECK(callContractFunction("s2()"), encodeArgs(0x20) + dyn1);
		}
}

BOOST_AUTO_TEST_CASE(return_bytes_internal)
{
	char const* sourceCode = R"(
		contract Main {
			bytes s1;
			function doSet(bytes memory _s1) public returns (bytes memory _r1) {
				s1 = _s1;
				_r1 = s1;
			}
			function set(bytes calldata _s1) external returns (uint _r, bytes memory _r1) {
				_r1 = doSet(_s1);
				_r = _r1.length;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	string s1("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	vector<size_t> lengths{0, 31, 64, 65};
	for (auto l1: lengths)
	{
		bytes dyn1 = encodeArgs(u256(l1), s1.substr(0, l1));
		bytes args1 = encodeArgs(u256(0x20)) + dyn1;
		BOOST_REQUIRE(
			callContractFunction("set(bytes)", asString(args1)) ==
			encodeArgs(u256(l1), u256(0x40)) + dyn1
		);
	}
}

BOOST_AUTO_TEST_CASE(bytes_index_access_memory)
{
	char const* sourceCode = R"(
		contract Main {
			function f(bytes memory _s1, uint i1, uint i2, uint i3) public returns (byte c1, byte c2, byte c3) {
				c1 = _s1[i1];
				c2 = intern(_s1, i2);
				c3 = internIndirect(_s1)[i3];
			}
			function intern(bytes memory _s1, uint i) public returns (byte c) {
				return _s1[i];
			}
			function internIndirect(bytes memory _s1) public returns (bytes memory) {
				return _s1;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	string s1("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	bytes dyn1 = encodeArgs(u256(s1.length()), s1);
	bytes args1 = encodeArgs(u256(0x80), u256(3), u256(4), u256(5)) + dyn1;
	BOOST_REQUIRE(
		callContractFunction("f(bytes,uint256,uint256,uint256)", asString(args1)) ==
		encodeArgs(string{s1[3]}, string{s1[4]}, string{s1[5]})
	);
}

BOOST_AUTO_TEST_CASE(bytes_in_constructors_unpacker)
{
	char const* sourceCode = R"(
		contract Test {
			uint public m_x;
			bytes public m_s;
			constructor(uint x, bytes memory s) public {
				m_x = x;
				m_s = s;
			}
		}
	)";
	string s1("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	bytes dyn1 = encodeArgs(u256(s1.length()), s1);
	u256 x = 7;
	bytes args1 = encodeArgs(x, u256(0x40)) + dyn1;
	compileAndRun(sourceCode, 0, "Test", args1);
	BOOST_REQUIRE(callContractFunction("m_x()") == encodeArgs(x));
	BOOST_REQUIRE(callContractFunction("m_s()") == encodeArgs(u256(0x20)) + dyn1);
}

BOOST_AUTO_TEST_CASE(bytes_in_constructors_packer)
{
	char const* sourceCode = R"(
		contract Base {
			uint public m_x;
			bytes m_s;
			constructor(uint x, bytes memory s) public {
				m_x = x;
				m_s = s;
			}
			function part(uint i) public returns (byte) {
				return m_s[i];
			}
		}
		contract Main is Base {
			constructor(bytes memory s, uint x) Base(x, f(s)) public {}
			function f(bytes memory s) public returns (bytes memory) {
				return s;
			}
		}
		contract Creator {
			function f(uint x, bytes memory s) public returns (uint r, byte ch) {
				Main c = new Main(s, x);
				r = c.m_x();
				ch = c.part(x);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Creator");
	string s1("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
	bytes dyn1 = encodeArgs(u256(s1.length()), s1);
	u256 x = 7;
	bytes args1 = encodeArgs(x, u256(0x40)) + dyn1;
	BOOST_REQUIRE(
		callContractFunction("f(uint256,bytes)", asString(args1)) ==
		encodeArgs(x, string{s1[unsigned(x)]})
	);
}

BOOST_AUTO_TEST_CASE(arrays_in_constructors)
{
	char const* sourceCode = R"(
		contract Base {
			uint public m_x;
			address[] m_s;
			constructor(uint x, address[] memory s) public {
				m_x = x;
				m_s = s;
			}
			function part(uint i) public returns (address) {
				return m_s[i];
			}
		}
		contract Main is Base {
			constructor(address[] memory s, uint x) Base(x, f(s)) public {}
			function f(address[] memory s) public returns (address[] memory) {
				return s;
			}
		}
		contract Creator {
			function f(uint x, address[] memory s) public returns (uint r, address ch) {
				Main c = new Main(s, x);
				r = c.m_x();
				ch = c.part(x);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Creator");
	vector<u256> s1{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	bytes dyn1 = encodeArgs(u256(s1.size()), s1);
	u256 x = 7;
	bytes args1 = encodeArgs(x, u256(0x40)) + dyn1;
	BOOST_REQUIRE(
		callContractFunction("f(uint256,address[])", asString(args1)) ==
		encodeArgs(x, s1[unsigned(x)])
	);
}

BOOST_AUTO_TEST_CASE(fixed_arrays_in_constructors)
{
	char const* sourceCode = R"(
		contract Creator {
			uint public r;
			address public ch;
			constructor(address[3] memory s, uint x) public {
				r = x;
				ch = s[2];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Creator", encodeArgs(u256(1), u256(2), u256(3), u256(4)));
	BOOST_REQUIRE(callContractFunction("r()") == encodeArgs(u256(4)));
	BOOST_REQUIRE(callContractFunction("ch()") == encodeArgs(u256(3)));
}

BOOST_AUTO_TEST_CASE(arrays_from_and_to_storage)
{
	char const* sourceCode = R"(
		contract Test {
			uint24[] public data;
			function set(uint24[] memory _data) public returns (uint) {
				data = _data;
				return data.length;
			}
			function get() public returns (uint24[] memory) {
				return data;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	vector<u256> data{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
	BOOST_REQUIRE(
		callContractFunction("set(uint24[])", u256(0x20), u256(data.size()), data) ==
		encodeArgs(u256(data.size()))
	);
	ABI_CHECK(callContractFunction("data(uint256)", u256(7)), encodeArgs(u256(8)));
	ABI_CHECK(callContractFunction("data(uint256)", u256(15)), encodeArgs(u256(16)));
	ABI_CHECK(callContractFunction("data(uint256)", u256(18)), encodeArgs());
	ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(0x20), u256(data.size()), data));
}

BOOST_AUTO_TEST_CASE(arrays_complex_from_and_to_storage)
{
	char const* sourceCode = R"(
		contract Test {
			uint24[3][] public data;
			function set(uint24[3][] memory _data) public returns (uint) {
				data = _data;
				return data.length;
			}
			function get() public returns (uint24[3][] memory) {
				return data;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	vector<u256> data{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
	BOOST_REQUIRE(
		callContractFunction("set(uint24[3][])", u256(0x20), u256(data.size() / 3), data) ==
		encodeArgs(u256(data.size() / 3))
	);
	ABI_CHECK(callContractFunction("data(uint256,uint256)", u256(2), u256(2)), encodeArgs(u256(9)));
	ABI_CHECK(callContractFunction("data(uint256,uint256)", u256(5), u256(1)), encodeArgs(u256(17)));
	ABI_CHECK(callContractFunction("data(uint256,uint256)", u256(6), u256(0)), encodeArgs());
	ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(0x20), u256(data.size() / 3), data));
}

BOOST_AUTO_TEST_CASE(arrays_complex_memory_index_access)
{
	char const* sourceCode = R"(
		contract Test {
			function set(uint24[3][] memory _data, uint a, uint b) public returns (uint l, uint e) {
				l = _data.length;
				e = _data[a][b];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	vector<u256> data{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
	BOOST_REQUIRE(callContractFunction(
			"set(uint24[3][],uint256,uint256)",
			u256(0x60),
			u256(3),
			u256(2),
			u256(data.size() / 3),
			data
	) == encodeArgs(u256(data.size() / 3), u256(data[3 * 3 + 2])));
}

BOOST_AUTO_TEST_CASE(bytes_memory_index_access)
{
	char const* sourceCode = R"(
		contract Test {
			function set(bytes memory _data, uint i) public returns (uint l, byte c) {
				l = _data.length;
				c = _data[i];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	string data("abcdefgh");
	BOOST_REQUIRE(callContractFunction(
			"set(bytes,uint256)",
			u256(0x40),
			u256(3),
			u256(data.size()),
			data
	) == encodeArgs(u256(data.size()), string("d")));
}

BOOST_AUTO_TEST_CASE(storage_array_ref)
{
	char const* sourceCode = R"(
		contract BinarySearch {
		  /// Finds the position of _value in the sorted list _data.
		  /// Note that "internal" is important here, because storage references only work for internal or private functions
		  function find(uint[] storage _data, uint _value) internal returns (uint o_position) {
			return find(_data, 0, _data.length, _value);
		  }
		  function find(uint[] storage _data, uint _begin, uint _len, uint _value) private returns (uint o_position) {
			if (_len == 0 || (_len == 1 && _data[_begin] != _value))
			  return uint(-1); // failure
			uint halfLen = _len / 2;
			uint v = _data[_begin + halfLen];
			if (_value < v)
			  return find(_data, _begin, halfLen, _value);
			else if (_value > v)
			  return find(_data, _begin + halfLen + 1, halfLen - 1, _value);
			else
			  return _begin + halfLen;
		  }
		}

		contract Store is BinarySearch {
			uint[] data;
			function add(uint v) public {
				data.length++;
				data[data.length - 1] = v;
			}
			function find(uint v) public returns (uint) {
				return find(data, v);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Store");
	BOOST_REQUIRE(callContractFunction("find(uint256)", u256(7)) == encodeArgs(u256(-1)));
	BOOST_REQUIRE(callContractFunction("add(uint256)", u256(7)) == encodeArgs());
	BOOST_REQUIRE(callContractFunction("find(uint256)", u256(7)) == encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("add(uint256)", u256(11)), encodeArgs());
	ABI_CHECK(callContractFunction("add(uint256)", u256(17)), encodeArgs());
	ABI_CHECK(callContractFunction("add(uint256)", u256(27)), encodeArgs());
	ABI_CHECK(callContractFunction("add(uint256)", u256(31)), encodeArgs());
	ABI_CHECK(callContractFunction("add(uint256)", u256(32)), encodeArgs());
	ABI_CHECK(callContractFunction("add(uint256)", u256(66)), encodeArgs());
	ABI_CHECK(callContractFunction("add(uint256)", u256(177)), encodeArgs());
	ABI_CHECK(callContractFunction("find(uint256)", u256(7)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("find(uint256)", u256(27)), encodeArgs(u256(3)));
	ABI_CHECK(callContractFunction("find(uint256)", u256(32)), encodeArgs(u256(5)));
	ABI_CHECK(callContractFunction("find(uint256)", u256(176)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("find(uint256)", u256(0)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("find(uint256)", u256(400)), encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(memory_types_initialisation)
{
	char const* sourceCode = R"(
		contract Test {
			mapping(uint=>uint) data;
			function stat() public returns (uint[5] memory)
			{
				data[2] = 3; // make sure to use some memory
			}
			function dyn() public returns (uint[] memory) { stat(); }
			function nested() public returns (uint[3][] memory) { stat(); }
			function nestedStat() public returns (uint[3][7] memory) { stat(); }
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	ABI_CHECK(callContractFunction("stat()"), encodeArgs(vector<u256>(5)));
	ABI_CHECK(callContractFunction("dyn()"), encodeArgs(u256(0x20), u256(0)));
	ABI_CHECK(callContractFunction("nested()"), encodeArgs(u256(0x20), u256(0)));
	ABI_CHECK(callContractFunction("nestedStat()"), encodeArgs(vector<u256>(3 * 7)));
}

BOOST_AUTO_TEST_CASE(memory_arrays_delete)
{
	char const* sourceCode = R"(
		contract Test {
			function del() public returns (uint24[3][4] memory) {
				uint24[3][4] memory x;
				for (uint24 i = 0; i < x.length; i ++)
					for (uint24 j = 0; j < x[i].length; j ++)
						x[i][j] = i * 0x10 + j;
				delete x[1];
				delete x[3][2];
				return x;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	vector<u256> data(3 * 4);
	for (unsigned i = 0; i < 4; i++)
		for (unsigned j = 0; j < 3; j++)
		{
			u256 v = 0;
			if (!(i == 1 || (i == 3 && j == 2)))
				v = i * 0x10 + j;
			data[i * 3 + j] = v;
		}
	ABI_CHECK(callContractFunction("del()"), encodeArgs(data));
}

BOOST_AUTO_TEST_CASE(memory_arrays_index_access_write)
{
	char const* sourceCode = R"(
		contract Test {
			function set(uint24[3][4] memory x) public {
				x[2][2] = 1;
				x[3][2] = 7;
			}
			function f() public returns (uint24[3][4] memory){
				uint24[3][4] memory data;
				set(data);
				return data;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	vector<u256> data(3 * 4);
	data[3 * 2 + 2] = 1;
	data[3 * 3 + 2] = 7;
	ABI_CHECK(callContractFunction("f()"), encodeArgs(data));
}

BOOST_AUTO_TEST_CASE(memory_arrays_dynamic_index_access_write)
{
	char const* sourceCode = R"(
		contract Test {
			uint24[3][][4] data;
			function set(uint24[3][][4] memory x) internal returns (uint24[3][][4] memory) {
				x[1][2][2] = 1;
				x[1][3][2] = 7;
				return x;
			}
			function f() public returns (uint24[3][] memory) {
				data[1].length = 4;
				return set(data)[1];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	vector<u256> data(3 * 4);
	data[3 * 2 + 2] = 1;
	data[3 * 3 + 2] = 7;
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x20), u256(4), data));
}

BOOST_AUTO_TEST_CASE(memory_structs_read_write)
{
	char const* sourceCode = R"(
		contract Test {
			struct S { uint8 x; uint16 y; uint z; uint8[2] a; }
			S[5] data;
			function testInit() public returns (uint8 x, uint16 y, uint z, uint8 a, bool flag) {
				S[2] memory d;
				x = d[0].x;
				y = d[0].y;
				z = d[0].z;
				a = d[0].a[1];
				flag = true;
			}
			function testCopyRead() public returns (uint8 x, uint16 y, uint z, uint8 a) {
				data[2].x = 1;
				data[2].y = 2;
				data[2].z = 3;
				data[2].a[1] = 4;
				S memory s = data[2];
				x = s.x;
				y = s.y;
				z = s.z;
				a = s.a[1];
			}
			function testAssign() public returns (uint8 x, uint16 y, uint z, uint8 a) {
				S memory s;
				s.x = 1;
				s.y = 2;
				s.z = 3;
				s.a[1] = 4;
				x = s.x;
				y = s.y;
				z = s.z;
				a = s.a[1];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	ABI_CHECK(callContractFunction("testInit()"), encodeArgs(u256(0), u256(0), u256(0), u256(0), true));
	ABI_CHECK(callContractFunction("testCopyRead()"), encodeArgs(u256(1), u256(2), u256(3), u256(4)));
	ABI_CHECK(callContractFunction("testAssign()"), encodeArgs(u256(1), u256(2), u256(3), u256(4)));
}

BOOST_AUTO_TEST_CASE(memory_structs_as_function_args)
{
	char const* sourceCode = R"(
		contract Test {
			struct S { uint8 x; uint16 y; uint z; }
			function test() public returns (uint x, uint y, uint z) {
				S memory data = combine(1, 2, 3);
				x = extract(data, 0);
				y = extract(data, 1);
				z = extract(data, 2);
			}
			function extract(S memory s, uint which) internal returns (uint x) {
				if (which == 0) return s.x;
				else if (which == 1) return s.y;
				else return s.z;
			}
			function combine(uint8 x, uint16 y, uint z) internal returns (S memory s) {
				s.x = x;
				s.y = y;
				s.z = z;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(1), u256(2), u256(3)));
}

BOOST_AUTO_TEST_CASE(memory_structs_nested)
{
	char const* sourceCode = R"(
		contract Test {
			struct S { uint8 x; uint16 y; uint z; }
			struct X { uint8 x; S s; }
			function test() public returns (uint a, uint x, uint y, uint z) {
				X memory d = combine(1, 2, 3, 4);
				a = extract(d, 0);
				x = extract(d, 1);
				y = extract(d, 2);
				z = extract(d, 3);
			}
			function extract(X memory s, uint which) internal returns (uint x) {
				if (which == 0) return s.x;
				else if (which == 1) return s.s.x;
				else if (which == 2) return s.s.y;
				else return s.s.z;
			}
			function combine(uint8 a, uint8 x, uint16 y, uint z) internal returns (X memory s) {
				s.x = a;
				s.s.x = x;
				s.s.y = y;
				s.s.z = z;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(1), u256(2), u256(3), u256(4)));
}

BOOST_AUTO_TEST_CASE(memory_structs_nested_load)
{
	char const* sourceCode = R"(
		contract Test {
			struct S { uint8 x; uint16 y; uint z; }
			struct X { uint8 x; S s; uint8[2] a; }
			X m_x;
			function load() public returns (uint a, uint x, uint y, uint z, uint a1, uint a2) {
				m_x.x = 1;
				m_x.s.x = 2;
				m_x.s.y = 3;
				m_x.s.z = 4;
				m_x.a[0] = 5;
				m_x.a[1] = 6;
				X memory d = m_x;
				a = d.x;
				x = d.s.x;
				y = d.s.y;
				z = d.s.z;
				a1 = d.a[0];
				a2 = d.a[1];
			}
			function store() public returns (uint a, uint x, uint y, uint z, uint a1, uint a2) {
				X memory d;
				d.x = 1;
				d.s.x = 2;
				d.s.y = 3;
				d.s.z = 4;
				d.a[0] = 5;
				d.a[1] = 6;
				m_x = d;
				a = m_x.x;
				x = m_x.s.x;
				y = m_x.s.y;
				z = m_x.s.z;
				a1 = m_x.a[0];
				a2 = m_x.a[1];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	auto out = encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5), u256(6));
	ABI_CHECK(callContractFunction("load()"), out);
	ABI_CHECK(callContractFunction("store()"), out);
}

BOOST_AUTO_TEST_CASE(struct_constructor_nested)
{
	char const* sourceCode = R"(
		contract C {
			struct X { uint x1; uint x2; }
			struct S { uint s1; uint[3] s2; X s3; }
			S s;
			constructor() public {
				uint[3] memory s2;
				s2[1] = 9;
				s = S(1, s2, X(4, 5));
			}
			function get() public returns (uint s1, uint[3] memory s2, uint x1, uint x2)
			{
				s1 = s.s1;
				s2 = s.s2;
				x1 = s.s3.x1;
				x2 = s.s3.x2;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	auto out = encodeArgs(u256(1), u256(0), u256(9), u256(0), u256(4), u256(5));
	ABI_CHECK(callContractFunction("get()"), out);
}

BOOST_AUTO_TEST_CASE(struct_named_constructor)
{
	char const* sourceCode = R"(
		contract C {
			struct S { uint a; bool x; }
			S public s;
			constructor() public {
				s = S({a: 1, x: true});
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("s()"), encodeArgs(u256(1), true));
}

BOOST_AUTO_TEST_CASE(calldata_array)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(uint[2] calldata s) external pure returns (uint256 a, uint256 b) {
				a = s[0];
				b = s[1];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f(uint256[2])", encodeArgs(u256(42), u256(23))), encodeArgs(u256(42), u256(23)));
}

BOOST_AUTO_TEST_CASE(calldata_struct)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256 b; }
			function f(S calldata s) external pure returns (uint256 a, uint256 b) {
				a = s.a;
				b = s.b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256))", encodeArgs(u256(42), u256(23))), encodeArgs(u256(42), u256(23)));
}

BOOST_AUTO_TEST_CASE(calldata_struct_and_ints)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256 b; }
			function f(uint256 a, S calldata s, uint256 b) external pure returns (uint256, uint256, uint256, uint256) {
				return (a, s.a, s.b, b);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f(uint256,(uint256,uint256),uint256)", encodeArgs(u256(1), u256(2), u256(3), u256(4))), encodeArgs(u256(1), u256(2), u256(3), u256(4)));
}


BOOST_AUTO_TEST_CASE(calldata_structs)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S1 { uint256 a; uint256 b; }
			struct S2 { uint256 a; }
			function f(S1 calldata s1, S2 calldata s2, S1 calldata s3)
				external pure returns (uint256 a, uint256 b, uint256 c, uint256 d, uint256 e) {
				a = s1.a;
				b = s1.b;
				c = s2.a;
				d = s3.a;
				e = s3.b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256),(uint256),(uint256,uint256))", encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5))), encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5)));
}

BOOST_AUTO_TEST_CASE(calldata_struct_array_member)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256[2] b; uint256 c; }
			function f(S calldata s) external pure returns (uint256 a, uint256 b0, uint256 b1, uint256 c) {
				a = s.a;
				b0 = s.b[0];
				b1 = s.b[1];
				c = s.c;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256[2],uint256))", encodeArgs(u256(42), u256(1), u256(2), u256(23))), encodeArgs(u256(42), u256(1), u256(2), u256(23)));
}

BOOST_AUTO_TEST_CASE(calldata_array_of_struct)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256 b; }
			function f(S[] calldata s) external pure returns (uint256 l, uint256 a, uint256 b, uint256 c, uint256 d) {
				l = s.length;
				a = s[0].a;
				b = s[0].b;
				c = s[1].a;
				d = s[1].b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256)[])", encodeArgs(u256(0x20), u256(2), u256(1), u256(2), u256(3), u256(4))), encodeArgs(u256(2), u256(1), u256(2), u256(3), u256(4)));
}

BOOST_AUTO_TEST_CASE(calldata_array_of_struct_to_memory)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256 b; }
			function f(S[] calldata s) external pure returns (uint256 l, uint256 a, uint256 b, uint256 c, uint256 d) {
				S[] memory m = s;
				l = m.length;
				a = m[0].a;
				b = m[0].b;
				c = m[1].a;
				d = m[1].b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256)[])", encodeArgs(u256(0x20), u256(2), u256(1), u256(2), u256(3), u256(4))), encodeArgs(u256(2), u256(1), u256(2), u256(3), u256(4)));
}


BOOST_AUTO_TEST_CASE(calldata_struct_to_memory)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256 b; }
			function f(S calldata s) external pure returns (uint256, uint256) {
				S memory m = s;
				return (m.a, m.b);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256))", encodeArgs(u256(42), u256(23))), encodeArgs(u256(42), u256(23)));
}

BOOST_AUTO_TEST_CASE(nested_calldata_struct)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S1 { uint256 a; uint256 b; }
			struct S2 { uint256 a; uint256 b; S1 s; uint256 c; }
			function f(S2 calldata s) external pure returns (uint256 a, uint256 b, uint256 sa, uint256 sb, uint256 c) {
				return (s.a, s.b, s.s.a, s.s.b, s.c);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256,(uint256,uint256),uint256))", encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5))), encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5)));
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
