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

int constexpr roundTo32(int _num)
{
	return (_num + 31) / 32 * 32;
}

BOOST_AUTO_TEST_CASE(nested_calldata_struct_to_memory)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S1 { uint256 a; uint256 b; }
			struct S2 { uint256 a; uint256 b; S1 s; uint256 c; }
			function f(S2 calldata s) external pure returns (uint256 a, uint256 b, uint256 sa, uint256 sb, uint256 c) {
				S2 memory m = s;
				return (m.a, m.b, m.s.a, m.s.b, m.c);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(callContractFunction("f((uint256,uint256,(uint256,uint256),uint256))", encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5))), encodeArgs(u256(1), u256(2), u256(3), u256(4), u256(5)));
}

BOOST_AUTO_TEST_CASE(calldata_struct_short)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint256 a; uint256 b; }
			function f(S calldata) external pure returns (uint256) {
				return msg.data.length;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	// double check that the valid case goes through
	ABI_CHECK(callContractFunction("f((uint256,uint256))", u256(1), u256(2)), encodeArgs(0x44));

	ABI_CHECK(callContractFunctionNoEncoding("f((uint256,uint256))", bytes(63,0)), encodeArgs());
	ABI_CHECK(callContractFunctionNoEncoding("f((uint256,uint256))", bytes(33,0)), encodeArgs());
	ABI_CHECK(callContractFunctionNoEncoding("f((uint256,uint256))", bytes(32,0)), encodeArgs());
	ABI_CHECK(callContractFunctionNoEncoding("f((uint256,uint256))", bytes(31,0)), encodeArgs());
	ABI_CHECK(callContractFunctionNoEncoding("f((uint256,uint256))", bytes()), encodeArgs());
}

BOOST_AUTO_TEST_CASE(calldata_struct_cleaning)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint8 a; bytes1 b; }
			function f(S calldata s) external pure returns (uint256 a, bytes32 b) {
				uint8 tmp1 = s.a;
				bytes1 tmp2 = s.b;
				assembly {
					a := tmp1
					b := tmp2
				}

			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	// double check that the valid case goes through
	ABI_CHECK(callContractFunction("f((uint8,bytes1))", u256(0x12), bytes{0x34} + bytes(31,0)), encodeArgs(0x12, bytes{0x34} + bytes(31,0)));
	ABI_CHECK(callContractFunction("f((uint8,bytes1))", u256(0x1234), bytes{0x56, 0x78} + bytes(30,0)), encodeArgs());
	ABI_CHECK(callContractFunction("f((uint8,bytes1))", u256(-1), u256(-1)), encodeArgs());
}

BOOST_AUTO_TEST_CASE(calldata_struct_function_type)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { function (uint) external returns (uint) fn; }
			function f(S calldata s) external returns (uint256) {
				return s.fn(42);
			}
			function g(uint256 a) external returns (uint256) {
				return a * 3;
			}
			function h(uint256 a) external returns (uint256) {
				return 23;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	bytes fn_C_g = m_contractAddress.asBytes() + FixedHash<4>(dev::keccak256("g(uint256)")).asBytes() + bytes(8,0);
	bytes fn_C_h = m_contractAddress.asBytes() + FixedHash<4>(dev::keccak256("h(uint256)")).asBytes() + bytes(8,0);
	ABI_CHECK(callContractFunctionNoEncoding("f((function))", fn_C_g), encodeArgs(42 * 3));
	ABI_CHECK(callContractFunctionNoEncoding("f((function))", fn_C_h), encodeArgs(23));
}

BOOST_AUTO_TEST_CASE(calldata_array_dynamic_bytes)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f1(bytes[1] calldata a) external returns (uint256, uint256, uint256, uint256) {
				return (a[0].length, uint8(a[0][0]), uint8(a[0][1]), uint8(a[0][2]));
			}
			function f2(bytes[1] calldata a, bytes[1] calldata b) external returns (uint256, uint256, uint256, uint256, uint256, uint256, uint256) {
				return (a[0].length, uint8(a[0][0]), uint8(a[0][1]), uint8(a[0][2]), b[0].length, uint8(b[0][0]), uint8(b[0][1]));
			}
			function g1(bytes[2] calldata a) external returns (uint256, uint256, uint256, uint256, uint256, uint256, uint256, uint256) {
				return (a[0].length, uint8(a[0][0]), uint8(a[0][1]), uint8(a[0][2]), a[1].length, uint8(a[1][0]), uint8(a[1][1]), uint8(a[1][2]));
			}
			function g2(bytes[] calldata a) external returns (uint256[8] memory) {
				return [a.length, a[0].length, uint8(a[0][0]), uint8(a[0][1]), a[1].length, uint8(a[1][0]), uint8(a[1][1]), uint8(a[1][2])];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	bytes bytes010203 = bytes{1,2,3}+bytes(29,0);
	bytes bytes040506 = bytes{4,5,6}+bytes(29,0);
	bytes bytes0102 = bytes{1,2}+bytes(30,0);
	ABI_CHECK(
		callContractFunction("f1(bytes[1])", 0x20, 0x20, 3, bytes010203),
		encodeArgs(3, 1, 2, 3)
	);
	ABI_CHECK(
		callContractFunction("f2(bytes[1],bytes[1])", 0x40, 0xA0, 0x20, 3, bytes010203, 0x20, 2, bytes0102),
		encodeArgs(3, 1, 2, 3, 2, 1, 2)
	);
	ABI_CHECK(
		callContractFunction("g1(bytes[2])", 0x20, 0x40, 0x80, 3, bytes010203, 3, bytes040506),
		encodeArgs(3, 1, 2, 3, 3, 4, 5, 6)
	);
	// same offset for both arrays
	ABI_CHECK(
		callContractFunction("g1(bytes[2])", 0x20, 0x40, 0x40, 3, bytes010203),
		encodeArgs(3, 1, 2, 3, 3, 1, 2, 3)
	);
	ABI_CHECK(
		callContractFunction("g2(bytes[])", 0x20, 2, 0x40, 0x80, 2, bytes0102, 3, bytes040506),
		encodeArgs(2, 2, 1, 2, 3, 4, 5, 6)
	);
}

BOOST_AUTO_TEST_CASE(calldata_dynamic_array_to_memory)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(uint256[][] calldata a) external returns (uint, uint256[] memory) {
				uint256[] memory m = a[0];
				return (a.length, m);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(
		callContractFunction("f(uint256[][])", 0x20, 1, 0x20, 2, 23, 42),
		encodeArgs(1, 0x40, 2, 23, 42)
	);
}

BOOST_AUTO_TEST_CASE(calldata_bytes_array_to_memory)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(bytes[] calldata a) external returns (uint, uint, bytes memory) {
				bytes memory m = a[0];
				return (a.length, m.length, m);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(
		callContractFunction("f(bytes[])", 0x20, 1, 0x20, 2, bytes{'a', 'b'} + bytes(30, 0)),
		encodeArgs(1, 2, 0x60, 2, bytes{'a','b'} + bytes(30, 0))
	);

	ABI_CHECK(
		callContractFunction("f(bytes[])", 0x20, 1, 0x20, 32, bytes(32, 'x')),
		encodeArgs(1, 32, 0x60, 32, bytes(32, 'x'))
	);
	bytes x_zero_a = bytes{'x'} + bytes(30, 0) + bytes{'a'};
	bytes a_zero_x = bytes{'a'} + bytes(30, 0) + bytes{'x'};
	bytes a_m_x = bytes{'a'} + bytes(30, 'm') + bytes{'x'};
	ABI_CHECK(
		callContractFunction("f(bytes[])", 0x20, 1, 0x20, 32, x_zero_a),
		encodeArgs(1, 32, 0x60, 32, x_zero_a)
	);
	ABI_CHECK(
		callContractFunction("f(bytes[])", 0x20, 1, 0x20, 32, a_zero_x),
		encodeArgs(1, 32, 0x60, 32, a_zero_x)
	);
	ABI_CHECK(
		callContractFunction("f(bytes[])", 0x20, 1, 0x20, 32, a_m_x),
		encodeArgs(1, 32, 0x60, 32, a_m_x)
	);
}

BOOST_AUTO_TEST_CASE(calldata_bytes_array_bounds)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(bytes[] calldata a, uint256 i) external returns (uint) {
				return uint8(a[0][i]);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(
		callContractFunction("f(bytes[],uint256)", 0x40, 0, 1, 0x20, 2, bytes{'a', 'b'} + bytes(30, 0)),
		encodeArgs('a')
	);
	ABI_CHECK(
		callContractFunction("f(bytes[],uint256)", 0x40, 1, 1, 0x20, 2, bytes{'a', 'b'} + bytes(30, 0)),
		encodeArgs('b')
	);
	ABI_CHECK(
		callContractFunction("f(bytes[],uint256)", 0x40, 2, 1, 0x20, 2, bytes{'a', 'b'} + bytes(30, 0)),
		encodeArgs()
	);
}

BOOST_AUTO_TEST_CASE(calldata_string_array)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(string[] calldata a) external returns (uint, uint, uint, string memory) {
				string memory s1 = a[0];
				bytes memory m1 = bytes(s1);
				return (a.length, m1.length, uint8(m1[0]), s1);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	ABI_CHECK(
		callContractFunction("f(string[])", 0x20, 1, 0x20, 2, bytes{'a', 'b'} + bytes(30, 0)),
		encodeArgs(1, 2, 'a', 0x80, 2, bytes{'a', 'b'} + bytes(30, 0))
	);
}

BOOST_AUTO_TEST_CASE(calldata_array_two_dimensional)
{
	vector<vector<u256>> data {
		{ 0x0A01, 0x0A02, 0x0A03 },
		{ 0x0B01, 0x0B02, 0x0B03, 0x0B04 }
	};

	for (bool outerDynamicallySized: { true, false })
	{
		string arrayType = outerDynamicallySized ? "uint256[][]" : "uint256[][2]";
		string sourceCode = R"(
			pragma experimental ABIEncoderV2;
			contract C {
				function test()" + arrayType + R"( calldata a) external returns (uint256) {
					return a.length;
				}
				function test()" + arrayType + R"( calldata a, uint256 i) external returns (uint256) {
					return a[i].length;
				}
				function test()" + arrayType + R"( calldata a, uint256 i, uint256 j) external returns (uint256) {
					return a[i][j];
				}
				function reenc()" + arrayType + R"( calldata a, uint256 i, uint256 j) external returns (uint256) {
					return this.test(a, i, j);
				}
			}
		)";
		compileAndRun(sourceCode, 0, "C");

		bytes encoding = encodeArray(
			outerDynamicallySized,
			true,
			data | boost::adaptors::transformed([&](vector<u256> const& _values) {
				return encodeArray(true, false, _values);
			})
		);

		ABI_CHECK(callContractFunction("test(" + arrayType + ")", 0x20, encoding), encodeArgs(data.size()));
		for (size_t i = 0; i < data.size(); i++)
		{
			ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256)", 0x40, i, encoding), encodeArgs(data[i].size()));
			for (size_t j = 0; j < data[i].size(); j++)
			{
				ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256,uint256)", 0x60, i, j, encoding), encodeArgs(data[i][j]));
				ABI_CHECK(callContractFunction("reenc(" + arrayType + ",uint256,uint256)", 0x60, i, j, encoding), encodeArgs(data[i][j]));
			}
			// out of bounds access
			ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256,uint256)", 0x60, i, data[i].size(), encoding), encodeArgs());
		}
		// out of bounds access
		ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256)", 0x40, data.size(), encoding), encodeArgs());
	}
}

BOOST_AUTO_TEST_CASE(calldata_array_dynamic_three_dimensional)
{
	vector<vector<vector<u256>>> data {
		{
			{ 0x010A01, 0x010A02, 0x010A03 },
			{ 0x010B01, 0x010B02, 0x010B03 }
		},
		{
			{ 0x020A01, 0x020A02, 0x020A03 },
			{ 0x020B01, 0x020B02, 0x020B03 }
		}
	};

	for (bool outerDynamicallySized: { true, false })
	for (bool middleDynamicallySized: { true, false })
	for (bool innerDynamicallySized: { true, false })
	{
		// only test dynamically encoded arrays
		if (!outerDynamicallySized && !middleDynamicallySized && !innerDynamicallySized)
			continue;

		string arrayType = "uint256";
		arrayType += innerDynamicallySized ? "[]" : "[3]";
		arrayType += middleDynamicallySized ? "[]" : "[2]";
		arrayType += outerDynamicallySized ? "[]" : "[2]";

		string sourceCode = R"(
			pragma experimental ABIEncoderV2;
			contract C {
				function test()" + arrayType + R"( calldata a) external returns (uint256) {
					return a.length;
				}
				function test()" + arrayType + R"( calldata a, uint256 i) external returns (uint256) {
					return a[i].length;
				}
				function test()" + arrayType + R"( calldata a, uint256 i, uint256 j) external returns (uint256) {
					return a[i][j].length;
				}
				function test()" + arrayType + R"( calldata a, uint256 i, uint256 j, uint256 k) external returns (uint256) {
					return a[i][j][k];
				}
				function reenc()" + arrayType + R"( calldata a, uint256 i, uint256 j, uint256 k) external returns (uint256) {
					return this.test(a, i, j, k);
				}
			}
		)";
		compileAndRun(sourceCode, 0, "C");

		bytes encoding = encodeArray(
			outerDynamicallySized,
			middleDynamicallySized || innerDynamicallySized,
			data | boost::adaptors::transformed([&](auto const& _middleData) {
				return encodeArray(
					middleDynamicallySized,
					innerDynamicallySized,
					_middleData | boost::adaptors::transformed([&](auto const& _values) {
						return encodeArray(innerDynamicallySized, false, _values);
					})
				);
			})
		);

		ABI_CHECK(callContractFunction("test(" + arrayType + ")", 0x20, encoding), encodeArgs(data.size()));
		for (size_t i = 0; i < data.size(); i++)
		{
			ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256)", 0x40, i, encoding), encodeArgs(data[i].size()));
			for (size_t j = 0; j < data[i].size(); j++)
			{
				ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256,uint256)", 0x60, i, j, encoding), encodeArgs(data[i][j].size()));
				for (size_t k = 0; k < data[i][j].size(); k++)
				{
					ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256,uint256,uint256)", 0x80, i, j, k, encoding), encodeArgs(data[i][j][k]));
					ABI_CHECK(callContractFunction("reenc(" + arrayType + ",uint256,uint256,uint256)", 0x80, i, j, k, encoding), encodeArgs(data[i][j][k]));
				}
				// out of bounds access
				ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256,uint256,uint256)", 0x80, i, j, data[i][j].size(), encoding), encodeArgs());
			}
			// out of bounds access
			ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256,uint256)", 0x60, i, data[i].size(), encoding), encodeArgs());
		}
		// out of bounds access
		ABI_CHECK(callContractFunction("test(" + arrayType + ",uint256)", 0x40, data.size(), encoding), encodeArgs());
	}
}

BOOST_AUTO_TEST_CASE(calldata_array_dynamic_invalid)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(uint256[][] calldata a) external returns (uint) {
				return 42;
			}
			function g(uint256[][] calldata a) external returns (uint) {
				a[0];
				return 42;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	// valid access stub
	ABI_CHECK(callContractFunction("f(uint256[][])", 0x20, 0), encodeArgs(42));
	// invalid on argument decoding
	ABI_CHECK(callContractFunction("f(uint256[][])", 0x20, 1), encodeArgs());
	// invalid on outer access
	ABI_CHECK(callContractFunction("f(uint256[][])", 0x20, 1, 0x20), encodeArgs(42));
	ABI_CHECK(callContractFunction("g(uint256[][])", 0x20, 1, 0x20), encodeArgs());
	// invalid on inner access
	ABI_CHECK(callContractFunction("f(uint256[][])", 0x20, 1, 0x20, 2, 0x42), encodeArgs(42));
	ABI_CHECK(callContractFunction("g(uint256[][])", 0x20, 1, 0x20, 2, 0x42), encodeArgs());
}

BOOST_AUTO_TEST_CASE(calldata_array_dynamic_invalid_static_middle)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(uint256[][1][] calldata a) external returns (uint) {
				return 42;
			}
			function g(uint256[][1][] calldata a) external returns (uint) {
				a[0];
				return 42;
			}
			function h(uint256[][1][] calldata a) external returns (uint) {
				a[0][0];
				return 42;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	// valid access stub
	ABI_CHECK(callContractFunction("f(uint256[][1][])", 0x20, 0), encodeArgs(42));
	// invalid on argument decoding
	ABI_CHECK(callContractFunction("f(uint256[][1][])", 0x20, 1), encodeArgs());
	// invalid on outer access
	ABI_CHECK(callContractFunction("f(uint256[][1][])", 0x20, 1, 0x20), encodeArgs(42));
	ABI_CHECK(callContractFunction("g(uint256[][1][])", 0x20, 1, 0x20), encodeArgs());
	// invalid on inner access
	ABI_CHECK(callContractFunction("f(uint256[][1][])", 0x20, 1, 0x20, 0x20), encodeArgs(42));
	ABI_CHECK(callContractFunction("g(uint256[][1][])", 0x20, 1, 0x20, 0x20), encodeArgs(42));
	ABI_CHECK(callContractFunction("h(uint256[][1][])", 0x20, 1, 0x20, 0x20), encodeArgs());
	ABI_CHECK(callContractFunction("f(uint256[][1][])", 0x20, 1, 0x20, 0x20, 1), encodeArgs(42));
	ABI_CHECK(callContractFunction("g(uint256[][1][])", 0x20, 1, 0x20, 0x20, 1), encodeArgs(42));
	ABI_CHECK(callContractFunction("h(uint256[][1][])", 0x20, 1, 0x20, 0x20, 1), encodeArgs());
}

BOOST_AUTO_TEST_CASE(literal_strings)
{
	char const* sourceCode = R"(
		contract Test {
			string public long;
			string public medium;
			string public short;
			string public empty;
			function f() public returns (string memory) {
				long = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678900123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
				medium = "01234567890123456789012345678901234567890123456789012345678901234567890123456789";
				short = "123";
				empty = "";
				return "Hello, World!";
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	string longStr = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678900123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
	string medium = "01234567890123456789012345678901234567890123456789012345678901234567890123456789";
	string shortStr = "123";
	string hello = "Hello, World!";

	ABI_CHECK(callContractFunction("f()"), encodeDyn(hello));
	ABI_CHECK(callContractFunction("long()"), encodeDyn(longStr));
	ABI_CHECK(callContractFunction("medium()"), encodeDyn(medium));
	ABI_CHECK(callContractFunction("short()"), encodeDyn(shortStr));
	ABI_CHECK(callContractFunction("empty()"), encodeDyn(string()));
}

BOOST_AUTO_TEST_CASE(initialise_string_constant)
{
	char const* sourceCode = R"(
		contract Test {
			string public short = "abcdef";
			string public long = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678900123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	string longStr = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678900123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
	string shortStr = "abcdef";

	ABI_CHECK(callContractFunction("long()"), encodeDyn(longStr));
	ABI_CHECK(callContractFunction("short()"), encodeDyn(shortStr));
}

BOOST_AUTO_TEST_CASE(memory_structs_with_mappings)
{
	char const* sourceCode = R"(
		contract Test {
			struct S { uint8 a; mapping(uint => uint) b; uint8 c; }
			S s;
			function f() public returns (uint) {
				S memory x;
				if (x.a != 0 || x.c != 0) return 1;
				x.a = 4; x.c = 5;
				s = x;
				if (s.a != 4 || s.c != 5) return 2;
				x = S(2, 3);
				if (x.a != 2 || x.c != 3) return 3;
				x = s;
				if (s.a != 4 || s.c != 5) return 4;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(string_bytes_conversion)
{
	char const* sourceCode = R"(
		contract Test {
			string s;
			bytes b;
			function f(string memory _s, uint n) public returns (byte) {
				b = bytes(_s);
				s = string(b);
				return bytes(s)[n];
			}
			function l() public returns (uint) { return bytes(s).length; }
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	ABI_CHECK(callContractFunction(
		"f(string,uint256)",
		u256(0x40),
		u256(2),
		u256(6),
		string("abcdef")
	), encodeArgs("c"));
	ABI_CHECK(callContractFunction("l()"), encodeArgs(u256(6)));
}

BOOST_AUTO_TEST_CASE(string_as_mapping_key)
{
	char const* sourceCode = R"(
		contract Test {
			mapping(string => uint) data;
			function set(string memory _s, uint _v) public { data[_s] = _v; }
			function get(string memory _s) public returns (uint) { return data[_s]; }
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	vector<string> strings{
		"Hello, World!",
		"Hello,                            World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111",
		"",
		"1"
	};
	for (unsigned i = 0; i < strings.size(); i++)
		ABI_CHECK(callContractFunction(
			"set(string,uint256)",
			u256(0x40),
			u256(7 + i),
			u256(strings[i].size()),
			strings[i]
		), encodeArgs());
	for (unsigned i = 0; i < strings.size(); i++)
		ABI_CHECK(callContractFunction(
			"get(string)",
			u256(0x20),
			u256(strings[i].size()),
			strings[i]
		), encodeArgs(u256(7 + i)));
}

BOOST_AUTO_TEST_CASE(string_as_public_mapping_key)
{
	char const* sourceCode = R"(
		contract Test {
			mapping(string => uint) public data;
			function set(string memory _s, uint _v) public { data[_s] = _v; }
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	vector<string> strings{
		"Hello, World!",
		"Hello,                            World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111",
		"",
		"1"
	};
	for (unsigned i = 0; i < strings.size(); i++)
		ABI_CHECK(callContractFunction(
			"set(string,uint256)",
			u256(0x40),
			u256(7 + i),
			u256(strings[i].size()),
			strings[i]
		), encodeArgs());
	for (unsigned i = 0; i < strings.size(); i++)
		ABI_CHECK(callContractFunction(
			"data(string)",
			u256(0x20),
			u256(strings[i].size()),
			strings[i]
		), encodeArgs(u256(7 + i)));
}

BOOST_AUTO_TEST_CASE(nested_string_as_public_mapping_key)
{
	char const* sourceCode = R"(
		contract Test {
			mapping(string => mapping(string => uint)) public data;
			function set(string memory _s, string memory _s2, uint _v) public {
				data[_s][_s2] = _v; }
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	vector<string> strings{
		"Hello, World!",
		"Hello,                            World!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111",
		"",
		"1",
		"last one"
	};
	for (unsigned i = 0; i + 1 < strings.size(); i++)
		ABI_CHECK(callContractFunction(
			"set(string,string,uint256)",
			u256(0x60),
			u256(roundTo32(0x80 + strings[i].size())),
			u256(7 + i),
			u256(strings[i].size()),
			strings[i],
			u256(strings[i+1].size()),
			strings[i+1]
		), encodeArgs());
	for (unsigned i = 0; i + 1 < strings.size(); i++)
		ABI_CHECK(callContractFunction(
			"data(string,string)",
			u256(0x40),
			u256(roundTo32(0x60 + strings[i].size())),
			u256(strings[i].size()),
			strings[i],
			u256(strings[i+1].size()),
			strings[i+1]
		), encodeArgs(u256(7 + i)));
}

BOOST_AUTO_TEST_CASE(nested_mixed_string_as_public_mapping_key)
{
	char const* sourceCode = R"(
		contract Test {
			mapping(string =>
				mapping(int =>
					mapping(address =>
						mapping(bytes => int)))) public data;

			function set(
				string memory _s1,
				int _s2,
				address _s3,
				bytes memory _s4,
				int _value
			) public
			{
				data[_s1][_s2][_s3][_s4] = _value;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");

	struct Index
	{
		string s1;
		int s2;
		int s3;
		string s4;
	};

	vector<Index> data{
		{ "aabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcbc", 4, 23, "efg" },
		{ "tiaron", 456, 63245, "908apzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapzapz" },
		{ "", 2345, 12934, "665i65i65i65i65i65i65i65i65i65i65i65i65i65i65i65i65i65i5iart" },
		{ "¡¿…", 9781, 8148, "" },
		{ "ρν♀♀ω₂₃♀", 929608, 303030, "" }
	};

	for (size_t i = 0; i + 1 < data.size(); i++)
		ABI_CHECK(callContractFunction(
			"set(string,int256,address,bytes,int256)",
			u256(0xA0),
			u256(data[i].s2),
			u256(data[i].s3),
			u256(roundTo32(0xC0 + data[i].s1.size())),
			u256(i - 3),
			u256(data[i].s1.size()),
			data[i].s1,
			u256(data[i].s4.size()),
			data[i].s4
		), encodeArgs());
	for (size_t i = 0; i + 1 < data.size(); i++)
		ABI_CHECK(callContractFunction(
			"data(string,int256,address,bytes)",
			u256(0x80),
			u256(data[i].s2),
			u256(data[i].s3),
			u256(roundTo32(0xA0 + data[i].s1.size())),
			u256(data[i].s1.size()),
			data[i].s1,
			u256(data[i].s4.size()),
			data[i].s4
		), encodeArgs(u256(i - 3)));
}

BOOST_AUTO_TEST_CASE(accessor_for_state_variable)
{
	char const* sourceCode = R"(
		contract Lotto {
			uint public ticketPrice = 500;
		}
	)";

	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("ticketPrice()"), encodeArgs(u256(500)));
}

BOOST_AUTO_TEST_CASE(accessor_for_const_state_variable)
{
	char const* sourceCode = R"(
		contract Lotto{
			uint constant public ticketPrice = 555;
		}
	)";

	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("ticketPrice()"), encodeArgs(u256(555)));
}

BOOST_AUTO_TEST_CASE(state_variable_under_contract_name)
{
	char const* text = R"(
		contract Scope {
			uint stateVar = 42;

			function getStateVar() public view returns (uint stateVar) {
				stateVar = Scope.stateVar;
			}
		}
	)";
	compileAndRun(text);
	ABI_CHECK(callContractFunction("getStateVar()"), encodeArgs(u256(42)));
}

BOOST_AUTO_TEST_CASE(state_variable_local_variable_mixture)
{
	char const* sourceCode = R"(
		contract A {
			uint x = 1;
			uint y = 2;
			function a() public returns (uint x) {
				x = A.y;
			}
		}
	)";

	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(inherited_function) {
	char const* sourceCode = R"(
		contract A { function f() internal returns (uint) { return 1; } }
		contract B is A {
			function f() internal returns (uint) { return 2; }
			function g() public returns (uint) {
				return A.f();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(inherited_function_calldata_memory) {
	char const* sourceCode = R"(
		contract A { function f(uint[] calldata a) external returns (uint) { return a[0]; } }
		contract B is A {
			function f(uint[] memory a) public returns (uint) { return a[1]; }
			function g() public returns (uint) {
				uint[] memory m = new uint[](2);
				m[0] = 42;
				m[1] = 23;
				return A(this).f(m);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(23)));
}

BOOST_AUTO_TEST_CASE(inherited_function_calldata_memory_interface) {
	char const* sourceCode = R"(
		interface I { function f(uint[] calldata a) external returns (uint); }
		contract A is I { function f(uint[] memory a) public returns (uint) { return 42; } }
		contract B {
			function f(uint[] memory a) public returns (uint) { return a[1]; }
			function g() public returns (uint) {
				I i = I(new A());
				return i.f(new uint[](2));
			}
		}
	)";

	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(42)));
}

BOOST_AUTO_TEST_CASE(inherited_function_calldata_calldata_interface) {
	char const* sourceCode = R"(
		interface I { function f(uint[] calldata a) external returns (uint); }
		contract A is I { function f(uint[] calldata a) external returns (uint) { return 42; } }
		contract B {
			function f(uint[] memory a) public returns (uint) { return a[1]; }
			function g() public returns (uint) {
				I i = I(new A());
				return i.f(new uint[](2));
			}
		}
	)";

	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(42)));
}

BOOST_AUTO_TEST_CASE(inherited_function_from_a_library) {
	char const* sourceCode = R"(
		library A { function f() internal returns (uint) { return 1; } }
		contract B {
			function f() internal returns (uint) { return 2; }
			function g() public returns (uint) {
				return A.f();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(inherited_constant_state_var)
{
	char const* sourceCode = R"(
		contract A {
			uint constant x = 7;
		}
		contract B is A {
			function f() public returns (uint) {
				return A.x;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(multiple_inherited_state_vars)
{
	char const* sourceCode = R"(
		contract A {
			uint x = 7;
		}
		contract B {
			uint x = 9;
		}
		contract C is A, B {
			function a() public returns (uint) {
				return A.x;
			}
			function b() public returns (uint) {
				return B.x;
			}
			function a_set(uint _x) public returns (uint) {
				A.x = _x;
				return 1;
			}
			function b_set(uint _x) public returns (uint) {
				B.x = _x;
				return 1;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("b()"), encodeArgs(u256(9)));
	ABI_CHECK(callContractFunction("a_set(uint256)", u256(1)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("b_set(uint256)", u256(3)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("b()"), encodeArgs(u256(3)));
}

BOOST_AUTO_TEST_CASE(constant_string_literal)
{
	char const* sourceCode = R"(
		contract Test {
			bytes32 constant public b = "abcdefghijklmnopq";
			string constant public x = "abefghijklmnopqabcdefghijklmnopqabcdefghijklmnopqabca";

			constructor() public {
				string memory xx = x;
				bytes32 bb = b;
			}
			function getB() public returns (bytes32) { return b; }
			function getX() public returns (string memory) { return x; }
			function getX2() public returns (string memory r) { r = x; }
			function unused() public returns (uint) {
				"unusedunusedunusedunusedunusedunusedunusedunusedunusedunusedunusedunused";
				return 2;
			}
		}
	)";

	compileAndRun(sourceCode);
	string longStr = "abefghijklmnopqabcdefghijklmnopqabcdefghijklmnopqabca";
	string shortStr = "abcdefghijklmnopq";
	ABI_CHECK(callContractFunction("b()"), encodeArgs(shortStr));
	ABI_CHECK(callContractFunction("x()"), encodeDyn(longStr));
	ABI_CHECK(callContractFunction("getB()"), encodeArgs(shortStr));
	ABI_CHECK(callContractFunction("getX()"), encodeDyn(longStr));
	ABI_CHECK(callContractFunction("getX2()"), encodeDyn(longStr));
	ABI_CHECK(callContractFunction("unused()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(storage_string_as_mapping_key_without_variable)
{
	char const* sourceCode = R"(
		contract Test {
			mapping(string => uint) data;
			function f() public returns (uint) {
				data["abc"] = 2;
				return data["abc"];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(library_call)
{
	char const* sourceCode = R"(
		library Lib { function m(uint x, uint y) public returns (uint) { return x * y; } }
		contract Test {
			function f(uint x) public returns (uint) {
				return Lib.m(x, 9);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(33)), encodeArgs(u256(33) * 9));
}

BOOST_AUTO_TEST_CASE(library_function_external)
{
	char const* sourceCode = R"(
		library Lib { function m(bytes calldata b) external pure returns (byte) { return b[2]; } }
		contract Test {
			function f(bytes memory b) public pure returns (byte) {
				return Lib.m(b);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(bytes)", u256(0x20), u256(5), "abcde"), encodeArgs("c"));
}

BOOST_AUTO_TEST_CASE(library_stray_values)
{
	char const* sourceCode = R"(
		library Lib { function m(uint x, uint y) public returns (uint) { return x * y; } }
		contract Test {
			function f(uint x) public returns (uint) {
				Lib;
				Lib.m;
				return x + 9;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(33)), encodeArgs(u256(42)));
}

BOOST_AUTO_TEST_CASE(cross_contract_types)
{
	char const* sourceCode = R"(
		contract Lib { struct S {uint a; uint b; } }
		contract Test {
			function f() public returns (uint r) {
				Lib.S memory x = Lib.S({a: 2, b: 3});
				r = x.b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Test");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(3)));
}

BOOST_AUTO_TEST_CASE(simple_throw)
{
	char const* sourceCode = R"(
		contract Test {
			function f(uint x) public returns (uint) {
				if (x > 10)
					return x + 10;
				else
					revert();
				return 2;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256)", u256(11)), encodeArgs(u256(21)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs());
}

BOOST_AUTO_TEST_CASE(strings_in_struct)
{
	char const* sourceCode = R"(
		contract buggystruct {
			Buggy public bug;

			struct Buggy {
				uint first;
				uint second;
				uint third;
				string last;
			}

			constructor() public {
				bug = Buggy(10, 20, 30, "asdfghjkl");
			}
			function getFirst() public returns (uint)
			{
				return bug.first;
			}
			function getSecond() public returns (uint)
			{
				return bug.second;
			}
			function getThird() public returns (uint)
			{
				return bug.third;
			}
			function getLast() public returns (string memory)
			{
				return bug.last;
			}
		}
		)";
	compileAndRun(sourceCode);
	string s = "asdfghjkl";
	ABI_CHECK(callContractFunction("getFirst()"), encodeArgs(u256(10)));
	ABI_CHECK(callContractFunction("getSecond()"), encodeArgs(u256(20)));
	ABI_CHECK(callContractFunction("getThird()"), encodeArgs(u256(30)));
	ABI_CHECK(callContractFunction("getLast()"), encodeDyn(s));
}

BOOST_AUTO_TEST_CASE(fixed_arrays_as_return_type)
{
	char const* sourceCode = R"(
		contract A {
			function f(uint16 input) public pure returns (uint16[5] memory arr)
			{
				arr[0] = input;
				arr[1] = ++input;
				arr[2] = ++input;
				arr[3] = ++input;
				arr[4] = ++input;
			}
		}
		contract B {
			function f() public returns (uint16[5] memory res, uint16[5] memory res2)
			{
				A a = new A();
				res = a.f(2);
				res2 = a.f(1000);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(
		u256(2), u256(3), u256(4), u256(5), u256(6), // first return argument
		u256(1000), u256(1001), u256(1002), u256(1003), u256(1004)) // second return argument
	);
}

BOOST_AUTO_TEST_CASE(internal_types_in_library)
{
	char const* sourceCode = R"(
		library Lib {
			function find(uint16[] storage _haystack, uint16 _needle) public view returns (uint)
			{
				for (uint i = 0; i < _haystack.length; ++i)
					if (_haystack[i] == _needle)
						return i;
				return uint(-1);
			}
		}
		contract Test {
			mapping(string => uint16[]) data;
			function f() public returns (uint a, uint b)
			{
				data["abc"].length = 20;
				data["abc"][4] = 9;
				data["abc"][17] = 3;
				a = Lib.find(data["abc"], 9);
				b = Lib.find(data["abc"], 3);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(4), u256(17)));
}

BOOST_AUTO_TEST_CASE(mapping_arguments_in_library)
{
	char const* sourceCode = R"(
		library Lib {
			function set(mapping(uint => uint) storage m, uint key, uint value) internal
			{
				m[key] = value;
			}
			function get(mapping(uint => uint) storage m, uint key) internal view returns (uint)
			{
				return m[key];
			}
		}
		contract Test {
			mapping(uint => uint) m;
			function set(uint256 key, uint256 value) public returns (uint)
			{
				uint oldValue = Lib.get(m, key);
				Lib.set(m, key, value);
				return oldValue;
			}
			function get(uint256 key) public view returns (uint) {
				return Lib.get(m, key);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("set(uint256,uint256)", u256(1), u256(42)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", u256(2), u256(84)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", u256(21), u256(7)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(1)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(2)), encodeArgs(u256(84)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(21)), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", u256(1), u256(21)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", u256(2), u256(42)), encodeArgs(u256(84)));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", u256(21), u256(14)), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(1)), encodeArgs(u256(21)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(2)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("get(uint256)", u256(21)), encodeArgs(u256(14)));
}

BOOST_AUTO_TEST_CASE(mapping_returns_in_library)
{
	char const* sourceCode = R"(
		library Lib {
			function choose_mapping(mapping(uint => uint) storage a, mapping(uint => uint) storage b, bool c) internal pure returns(mapping(uint=>uint) storage)
			{
				return c ? a : b;
			}
		}
		contract Test {
			mapping(uint => uint) a;
			mapping(uint => uint) b;
			function set(bool choice, uint256 key, uint256 value) public returns (uint)
			{
				mapping(uint => uint) storage m = Lib.choose_mapping(a, b, choice);
				uint oldValue = m[key];
				m[key] = value;
				return oldValue;
			}
			function get(bool choice, uint256 key) public view returns (uint) {
				return Lib.choose_mapping(a, b, choice)[key];
			}
			function get_a(uint256 key) public view returns (uint) {
				return a[key];
			}
			function get_b(uint256 key) public view returns (uint) {
				return b[key];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", true, u256(1), u256(42)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", true, u256(2), u256(84)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", true, u256(21), u256(7)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", false, u256(1), u256(10)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", false, u256(2), u256(11)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", false, u256(21), u256(12)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(1)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(2)), encodeArgs(u256(84)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(21)), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(1)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(2)), encodeArgs(u256(84)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(21)), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(1)), encodeArgs(u256(10)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(2)), encodeArgs(u256(11)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(21)), encodeArgs(u256(12)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(1)), encodeArgs(u256(10)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(2)), encodeArgs(u256(11)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(21)), encodeArgs(u256(12)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", true, u256(1), u256(21)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", true, u256(2), u256(42)), encodeArgs(u256(84)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", true, u256(21), u256(14)), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", false, u256(1), u256(30)), encodeArgs(u256(10)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", false, u256(2), u256(31)), encodeArgs(u256(11)));
	ABI_CHECK(callContractFunction("set(bool,uint256,uint256)", false, u256(21), u256(32)), encodeArgs(u256(12)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(1)), encodeArgs(u256(21)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(2)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("get_a(uint256)", u256(21)), encodeArgs(u256(14)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(1)), encodeArgs(u256(21)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(2)), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", true, u256(21)), encodeArgs(u256(14)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(1)), encodeArgs(u256(30)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(2)), encodeArgs(u256(31)));
	ABI_CHECK(callContractFunction("get_b(uint256)", u256(21)), encodeArgs(u256(32)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(1)), encodeArgs(u256(30)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(2)), encodeArgs(u256(31)));
	ABI_CHECK(callContractFunction("get(bool,uint256)", false, u256(21)), encodeArgs(u256(32)));
}

BOOST_AUTO_TEST_CASE(mapping_returns_in_library_named)
{
	char const* sourceCode = R"(
		library Lib {
			function f(mapping(uint => uint) storage a, mapping(uint => uint) storage b) internal returns(mapping(uint=>uint) storage r)
			{
				r = a;
				r[1] = 42;
				r = b;
				r[1] = 21;
			}
		}
		contract Test {
			mapping(uint => uint) a;
			mapping(uint => uint) b;
			function f() public returns (uint, uint, uint, uint, uint, uint)
			{
				Lib.f(a, b)[2] = 84;
				return (a[0], a[1], a[2], b[0], b[1], b[2]);
			}
			function g() public returns (uint, uint, uint, uint, uint, uint)
			{
				mapping(uint => uint) storage m = Lib.f(a, b);
				m[2] = 17;
				return (a[0], a[1], a[2], b[0], b[1], b[2]);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0), u256(42), u256(0), u256(0), u256(21), u256(84)));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(0), u256(42), u256(0), u256(0), u256(21), u256(17)));
}

BOOST_AUTO_TEST_CASE(using_library_mappings_public)
{
	char const* sourceCode = R"(
			library Lib {
				function set(mapping(uint => uint) storage m, uint key, uint value) public
				{
					m[key] = value;
				}
			}
			contract Test {
				mapping(uint => uint) m1;
				mapping(uint => uint) m2;
				function f() public returns (uint, uint, uint, uint, uint, uint)
				{
					Lib.set(m1, 0, 1);
					Lib.set(m1, 2, 42);
					Lib.set(m2, 0, 23);
					Lib.set(m2, 2, 99);
					return (m1[0], m1[1], m1[2], m2[0], m2[1], m2[2]);
				}
			}
		)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), u256(0), u256(42), u256(23), u256(0), u256(99)));
}

BOOST_AUTO_TEST_CASE(using_library_mappings_external)
{
	char const* libSourceCode = R"(
			library Lib {
				function set(mapping(uint => uint) storage m, uint key, uint value) external
				{
					m[key] = value * 2;
				}
			}
		)";
	char const* sourceCode = R"(
			library Lib {
				function set(mapping(uint => uint) storage m, uint key, uint value) external;
			}
			contract Test {
				mapping(uint => uint) m1;
				mapping(uint => uint) m2;
				function f() public returns (uint, uint, uint, uint, uint, uint)
				{
					Lib.set(m1, 0, 1);
					Lib.set(m1, 2, 42);
					Lib.set(m2, 0, 23);
					Lib.set(m2, 2, 99);
					return (m1[0], m1[1], m1[2], m2[0], m2[1], m2[2]);
				}
			}
		)";
	for (auto v2: {false, true})
	{
		string prefix = v2 ? "pragma experimental ABIEncoderV2;\n" : "";
		compileAndRun(prefix + libSourceCode, 0, "Lib");
		compileAndRun(prefix + sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
		ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2), u256(0), u256(84), u256(46), u256(0), u256(198)));
	}
}

BOOST_AUTO_TEST_CASE(using_library_mappings_return)
{
	char const* sourceCode = R"(
			library Lib {
				function choose(mapping(uint => mapping(uint => uint)) storage m, uint key) external returns (mapping(uint => uint) storage) {
					return m[key];
				}
			}
			contract Test {
				mapping(uint => mapping(uint => uint)) m;
				function f() public returns (uint, uint, uint, uint, uint, uint)
				{
					Lib.choose(m, 0)[0] = 1;
					Lib.choose(m, 0)[2] = 42;
					Lib.choose(m, 1)[0] = 23;
					Lib.choose(m, 1)[2] = 99;
					return (m[0][0], m[0][1], m[0][2], m[1][0], m[1][1], m[1][2]);
				}
			}
		)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), u256(0), u256(42), u256(23), u256(0), u256(99)));
}

BOOST_AUTO_TEST_CASE(using_library_structs)
{
	char const* sourceCode = R"(
		library Lib {
			struct Data { uint a; uint[] b; }
			function set(Data storage _s) public
			{
				_s.a = 7;
				_s.b.length = 20;
				_s.b[19] = 8;
			}
		}
		contract Test {
			mapping(string => Lib.Data) data;
			function f() public returns (uint a, uint b)
			{
				Lib.set(data["abc"]);
				a = data["abc"].a;
				b = data["abc"].b[19];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7), u256(8)));
}

BOOST_AUTO_TEST_CASE(library_struct_as_an_expression)
{
	char const* sourceCode = R"(
		library Arst {
			struct Foo {
				int Things;
				int Stuff;
			}
		}

		contract Tsra {
			function f() public returns(uint) {
				Arst.Foo;
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Tsra");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(library_enum_as_an_expression)
{
	char const* sourceCode = R"(
		library Arst {
			enum Foo {
				Things,
				Stuff
			}
		}

		contract Tsra {
			function f() public returns(uint) {
				Arst.Foo;
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Tsra");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(short_strings)
{
	// This test verifies that the byte array encoding that combines length and data works
	// correctly.
	char const* sourceCode = R"(
		contract A {
			bytes public data1 = "123";
			bytes data2;
			function lengthChange() public returns (uint)
			{
				// store constant in short and long string
				data1 = "123";
				if (!equal(data1, "123")) return 1;
				data2 = "12345678901234567890123456789012345678901234567890a";
				if (data2[17] != "8") return 3;
				if (data2.length != 51) return 4;
				if (data2[data2.length - 1] != "a") return 5;
				// change length: short -> short
				data1.length = 5;
				if (data1.length != 5) return 6;
				data1[4] = "4";
				if (data1[0] != "1") return 7;
				if (data1[4] != "4") return 8;
				// change length: short -> long
				data1.length = 80;
				if (data1.length != 80) return 9;
				data1.length = 70;
				if (data1.length != 70) return 9;
				if (data1[0] != "1") return 10;
				if (data1[4] != "4") return 11;
				for (uint i = 0; i < data1.length; i ++)
					data1[i] = byte(uint8(i * 3));
				if (uint8(data1[4]) != 4 * 3) return 12;
				if (uint8(data1[67]) != 67 * 3) return 13;
				// change length: long -> short
				data1.length = 22;
				if (data1.length != 22) return 14;
				if (uint8(data1[21]) != 21 * 3) return 15;
				if (uint8(data1[2]) != 2 * 3) return 16;
				// change length: short -> shorter
				data1.length = 19;
				if (data1.length != 19) return 17;
				if (uint8(data1[7]) != 7 * 3) return 18;
				// and now again to original size
				data1.length = 22;
				if (data1.length != 22) return 19;
				if (data1[21] != 0) return 20;
				data1.length = 0;
				data2.length = 0;
			}
			function copy() public returns (uint) {
				bytes memory x = "123";
				bytes memory y = "012345678901234567890123456789012345678901234567890123456789";
				bytes memory z = "1234567";
				data1 = x;
				data2 = y;
				if (!equal(data1, x)) return 1;
				if (!equal(data2, y)) return 2;
				// lengthen
				data1 = y;
				if (!equal(data1, y)) return 3;
				// shorten
				data1 = x;
				if (!equal(data1, x)) return 4;
				// change while keeping short
				data1 = z;
				if (!equal(data1, z)) return 5;
				// copy storage -> storage
				data1 = x;
				data2 = y;
				// lengthen
				data1 = data2;
				if (!equal(data1, y)) return 6;
				// shorten
				data1 = x;
				data2 = data1;
				if (!equal(data2, x)) return 7;
				bytes memory c = data2;
				data1 = c;
				if (!equal(data1, x)) return 8;
				data1 = "";
				data2 = "";
			}
			function deleteElements() public returns (uint) {
				data1 = "01234";
				delete data1[2];
				if (data1[2] != 0) return 1;
				if (data1[0] != "0") return 2;
				if (data1[3] != "3") return 3;
				delete data1;
				if (data1.length != 0) return 4;
			}

			function equal(bytes storage a, bytes memory b) internal returns (bool) {
				if (a.length != b.length) return false;
				for (uint i = 0; i < a.length; ++i) if (a[i] != b[i]) return false;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "A");
	ABI_CHECK(callContractFunction("data1()"), encodeDyn(string("123")));
	ABI_CHECK(callContractFunction("lengthChange()"), encodeArgs(u256(0)));
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("deleteElements()"), encodeArgs(u256(0)));
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("copy()"), encodeArgs(u256(0)));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(calldata_offset)
{
	// This tests a specific bug that was caused by not using the correct memory offset in the
	// calldata unpacker.
	char const* sourceCode = R"(
		contract CB
		{
			address[] _arr;
			string public last = "nd";
			constructor(address[] memory guardians) public
			{
				_arr = guardians;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "CB", encodeArgs(u256(0x20)));
	ABI_CHECK(callContractFunction("last()", encodeArgs()), encodeDyn(string("nd")));
}

BOOST_AUTO_TEST_CASE(contract_binary_dependencies)
{
	char const* sourceCode = R"(
		contract A { function f() public { new B(); } }
		contract B { function f() public { } }
		contract C { function f() public { new B(); } }
	)";
	compileAndRun(sourceCode);
}

BOOST_AUTO_TEST_CASE(reject_ether_sent_to_library)
{
	char const* sourceCode = R"(
		library lib {}
		contract c {
			constructor() public payable {}
			function f(address payable x) public returns (bool) {
				return x.send(1);
			}
			function () external payable {}
		}
	)";
	compileAndRun(sourceCode, 0, "lib");
	Address libraryAddress = m_contractAddress;
	compileAndRun(sourceCode, 10, "c");
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 10);
	BOOST_CHECK_EQUAL(balanceAt(libraryAddress), 0);
	ABI_CHECK(callContractFunction("f(address)", encodeArgs(u160(libraryAddress))), encodeArgs(false));
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 10);
	BOOST_CHECK_EQUAL(balanceAt(libraryAddress), 0);
	ABI_CHECK(callContractFunction("f(address)", encodeArgs(u160(m_contractAddress))), encodeArgs(true));
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 10);
	BOOST_CHECK_EQUAL(balanceAt(libraryAddress), 0);
}

BOOST_AUTO_TEST_CASE(multi_variable_declaration)
{
	char const* sourceCode = R"(
		contract C {
			function g() public returns (uint a, uint b, uint c) {
				a = 1; b = 2; c = 3;
			}
			function h() public returns (uint a, uint b, uint c, uint d) {
				a = 1; b = 2; c = 3; d = 4;
			}
			function f1() public returns (bool) {
				(uint x, uint y, uint z) = g();
				if (x != 1 || y != 2 || z != 3) return false;
				(, uint a,) = g();
				if (a != 2) return false;
				(uint b, , ) = g();
				if (b != 1) return false;
				(, , uint c) = g();
				if (c != 3) return false;
				return true;
			}
			function f2() public returns (bool) {
				(uint a1, , uint a3, ) = h();
				if (a1 != 1 || a3 != 3) return false;
				(uint b1, uint b2, , ) = h();
				if (b1 != 1 || b2 != 2) return false;
				(, uint c2, uint c3, ) = h();
				if (c2 != 2 || c3 != 3) return false;
				(, , uint d3, uint d4) = h();
				if (d3 != 3 || d4 != 4) return false;
				(uint e1, , uint e3, uint e4) = h();
				if (e1 != 1 || e3 != 3 || e4 != 4) return false;
				return true;
			}
			function f() public returns (bool) {
				return f1() && f2();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", encodeArgs()), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(typed_multi_variable_declaration)
{
	char const* sourceCode = R"(
		contract C {
			struct S { uint x; }
			S s;
			function g() internal returns (uint, S storage, uint) {
				s.x = 7;
				return (1, s, 2);
			}
			function f() public returns (bool) {
				(uint x1, S storage y1, uint z1) = g();
				if (x1 != 1 || y1.x != 7 || z1 != 2) return false;
				(, S storage y2,) = g();
				if (y2.x != 7) return false;
				(uint x2,,) = g();
				if (x2 != 1) return false;
				(,,uint z2) = g();
				if (z2 != 2) return false;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()", encodeArgs()), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(tuples)
{
	char const* sourceCode = R"(
		contract C {
			uint[] data;
			uint[] m_c;
			function g() internal returns (uint a, uint b, uint[] storage c) {
				return (1, 2, data);
			}
			function h() external returns (uint a, uint b) {
				return (5, 6);
			}
			function f() public returns (uint) {
				data.length = 1;
				data[0] = 3;
				uint a; uint b;
				(a, b) = this.h();
				if (a != 5 || b != 6) return 1;
				uint[] storage c = m_c;
				(a, b, c) = g();
				if (a != 1 || b != 2 || c[0] != 3) return 2;
				(a, b) = (b, a);
				if (a != 2 || b != 1) return 3;
				(a, , b, , ) = (8, 9, 10, 11, 12);
				if (a != 8 || b != 10) return 4;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(string_tuples)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (string memory, uint) {
				return ("abc", 8);
			}
			function g() public returns (string memory, string memory) {
				return (h(), "def");
			}
			function h() public returns (string memory) {
				return ("abc");
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x40), u256(8), u256(3), string("abc")));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(0x40), u256(0x80), u256(3), string("abc"), u256(3), string("def")));
}

BOOST_AUTO_TEST_CASE(decayed_tuple)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				uint x = 1;
				(x) = 2;
				return x;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(inline_tuple_with_rational_numbers)
{
	char const* sourceCode = R"(
		contract c {
			function f() public returns (int8) {
				int8[5] memory foo3 = [int8(1), -1, 0, 0, 0];
				return foo3[0];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(destructuring_assignment)
{
	char const* sourceCode = R"(
		contract C {
			uint x = 7;
			bytes data;
			uint[] y;
			uint[] arrayData;
			function returnsArray() public returns (uint[] memory) {
				arrayData.length = 9;
				arrayData[2] = 5;
				arrayData[7] = 4;
				return arrayData;
			}
			function f(bytes memory s) public returns (uint) {
				uint loc;
				uint[] memory memArray;
				(loc, x, y, data, arrayData[3]) = (8, 4, returnsArray(), s, 2);
				if (loc != 8) return 1;
				if (x != 4) return 2;
				if (y.length != 9) return 3;
				if (y[2] != 5) return 4;
				if (y[7] != 4) return 5;
				if (data.length != s.length) return 6;
				if (data[3] != s[3]) return 7;
				if (arrayData[3] != 2) return 8;
				(memArray, loc) = (arrayData, 3);
				if (loc != 3) return 9;
				if (memArray.length != arrayData.length) return 10;
				bytes memory memBytes;
				(x, memBytes, y[2], , ) = (456, s, 789, 101112, 131415);
				if (x != 456 || memBytes.length != s.length || y[2] != 789) return 11;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bytes)", u256(0x20), u256(5), string("abcde")), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(lone_struct_array_type)
{
	char const* sourceCode = R"(
		contract C {
			struct s { uint a; uint b;}
			function f() public returns (uint) {
				s[7][]; // This is only the type, should not have any effect
				return 3;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(3)));
}

BOOST_AUTO_TEST_CASE(create_memory_array)
{
	char const* sourceCode = R"(
		contract C {
			struct S { uint[2] a; bytes b; }
			function f() public returns (byte, uint, uint, byte) {
				bytes memory x = new bytes(200);
				x[199] = 'A';
				uint[2][] memory y = new uint[2][](300);
				y[203][1] = 8;
				S[] memory z = new S[](180);
				z[170].a[1] = 4;
				z[170].b = new bytes(102);
				z[170].b[99] = 'B';
				return (x[199], y[203][1], z[170].a[1], z[170].b[99]);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(string("A"), u256(8), u256(4), string("B")));
}

BOOST_AUTO_TEST_CASE(create_memory_array_allocation_size)
{
	// Check allocation size of byte array. Should be 32 plus length rounded up to next
	// multiple of 32
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (uint d1, uint d2, uint d3, uint memsize) {
				bytes memory b1 = new bytes(31);
				bytes memory b2 = new bytes(32);
				bytes memory b3 = new bytes(256);
				bytes memory b4 = new bytes(31);
				assembly {
					d1 := sub(b2, b1)
					d2 := sub(b3, b2)
					d3 := sub(b4, b3)
					memsize := msize()
				}
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0x40, 0x40, 0x20 + 256, 0x260));
}

BOOST_AUTO_TEST_CASE(memory_arrays_of_various_sizes)
{
	// Computes binomial coefficients the chinese way
	char const* sourceCode = R"(
		contract C {
			function f(uint n, uint k) public returns (uint) {
				uint[][] memory rows = new uint[][](n + 1);
				for (uint i = 1; i <= n; i++) {
					rows[i] = new uint[](i);
					rows[i][0] = rows[i][rows[i].length - 1] = 1;
					for (uint j = 1; j < i - 1; j++)
						rows[i][j] = rows[i - 1][j - 1] + rows[i - 1][j];
				}
				return rows[n][k - 1];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256,uint256)", encodeArgs(u256(3), u256(1))), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", encodeArgs(u256(9), u256(5))), encodeArgs(u256(70)));
}

BOOST_AUTO_TEST_CASE(create_multiple_dynamic_arrays)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				uint[][] memory x = new uint[][](42);
				assert(x[0].length == 0);
				x[0] = new uint[](1);
				x[0][0] = 1;
				assert(x[4].length == 0);
				x[4] = new uint[](1);
				x[4][0] = 2;
				assert(x[10].length == 0);
				x[10] = new uint[](1);
				x[10][0] = 44;
				uint[][] memory y = new uint[][](24);
				assert(y[0].length == 0);
				y[0] = new uint[](1);
				y[0][0] = 1;
				assert(y[4].length == 0);
				y[4] = new uint[](1);
				y[4][0] = 2;
				assert(y[10].length == 0);
				y[10] = new uint[](1);
				y[10][0] = 88;
				if ((x[0][0] == y[0][0]) && (x[4][0] == y[4][0]) && (x[10][0] == 44) && (y[10][0] == 88))
					return 7;
				return 0;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(memory_overwrite)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bytes memory x) {
				x = "12345";
				x[3] = 0x61;
				x[0] = 0x62;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeDyn(string("b23a5")));
}

BOOST_AUTO_TEST_CASE(addmod_mulmod)
{
	char const* sourceCode = R"(
		contract C {
			function test() public returns (uint) {
				// Note that this only works because computation on literals is done using
				// unbounded integers.
				if ((2**255 + 2**255) % 7 != addmod(2**255, 2**255, 7))
					return 1;
				if ((2**255 + 2**255) % 7 != addmod(2**255, 2**255, 7))
					return 2;
				return 0;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(addmod_mulmod_zero)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint d) public pure returns (uint) {
				addmod(1, 2, d);
				return 2;
			}
			function g(uint d) public pure returns (uint) {
				mulmod(1, 2, d);
				return 2;
			}
			function h() public pure returns (uint) {
				mulmod(0, 1, 2);
				mulmod(1, 0, 2);
				addmod(0, 1, 2);
				addmod(1, 0, 2);
				return 2;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint)", 0), encodeArgs());
	ABI_CHECK(callContractFunction("g(uint)", 0), encodeArgs());
	ABI_CHECK(callContractFunction("h()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(divisiod_by_zero)
{
	char const* sourceCode = R"(
		contract C {
			function div(uint a, uint b) public returns (uint) {
				return a / b;
			}
			function mod(uint a, uint b) public returns (uint) {
				return a % b;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("div(uint256,uint256)", 7, 2), encodeArgs(u256(3)));
	// throws
	ABI_CHECK(callContractFunction("div(uint256,uint256)", 7, 0), encodeArgs());
	ABI_CHECK(callContractFunction("mod(uint256,uint256)", 7, 2), encodeArgs(u256(1)));
	// throws
	ABI_CHECK(callContractFunction("mod(uint256,uint256)", 7, 0), encodeArgs());
}

BOOST_AUTO_TEST_CASE(string_allocation_bug)
{
	char const* sourceCode = R"(
		contract Sample
		{
			struct s { uint16 x; uint16 y; string a; string b;}
			s[2] public p;
			constructor() public {
				s memory m;
				m.x = 0xbbbb;
				m.y = 0xcccc;
				m.a = "hello";
				m.b = "world";
				p[0] = m;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("p(uint256)", 0), encodeArgs(
		u256(0xbbbb),
		u256(0xcccc),
		u256(0x80),
		u256(0xc0),
		u256(5),
		string("hello"),
		u256(5),
		string("world")
	));
}

BOOST_AUTO_TEST_CASE(using_for_function_on_int)
{
	char const* sourceCode = R"(
		library D { function double(uint self) public returns (uint) { return 2*self; } }
		contract C {
			using D for uint;
			function f(uint a) public returns (uint) {
				return a.double();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"D", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(9)), encodeArgs(u256(2 * 9)));
}

BOOST_AUTO_TEST_CASE(using_for_function_on_struct)
{
	char const* sourceCode = R"(
		library D { struct s { uint a; } function mul(s storage self, uint x) public returns (uint) { return self.a *= x; } }
		contract C {
			using D for D.s;
			D.s public x;
			function f(uint a) public returns (uint) {
				x.a = 3;
				return x.mul(a);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"D", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(7)), encodeArgs(u256(3 * 7)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(3 * 7)));
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
