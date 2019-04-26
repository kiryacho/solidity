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

BOOST_AUTO_TEST_CASE(shift_constant_left)
{
	char const* sourceCode = R"(
		contract C {
			uint public a = 0x42 << 8;
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(0x4200)));
}

BOOST_AUTO_TEST_CASE(shift_negative_constant_left)
{
	char const* sourceCode = R"(
		contract C {
			int public a = -0x42 << 8;
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(-0x4200)));
}

BOOST_AUTO_TEST_CASE(shift_constant_right)
{
	char const* sourceCode = R"(
		contract C {
			uint public a = 0x4200 >> 8;
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(0x42)));
}

BOOST_AUTO_TEST_CASE(shift_negative_constant_right)
{
	char const* sourceCode = R"(
		contract C {
			int public a = -0x4200 >> 8;
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(-0x42)));
}

BOOST_AUTO_TEST_CASE(shift_left)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a, uint b) public returns (uint) {
				return a << b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(8)), encodeArgs(u256(0x426600)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(16)), encodeArgs(u256(0x42660000)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(17)), encodeArgs(u256(0x84cc0000)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(240)), fromHex("4266000000000000000000000000000000000000000000000000000000000000"));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(256)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_left_uint32)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint32 a, uint32 b) public returns (uint) {
				return a << b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(8)), encodeArgs(u256(0x426600)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(16)), encodeArgs(u256(0x42660000)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(17)), encodeArgs(u256(0x84cc0000)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(32)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_left_uint8)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint8 a, uint8 b) public returns (uint) {
				return a << b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint8,uint8)", u256(0x66), u256(0)), encodeArgs(u256(0x66)));
	ABI_CHECK(callContractFunction("f(uint8,uint8)", u256(0x66), u256(8)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_left_larger_type)
{
	// This basically tests proper cleanup and conversion. It should not convert x to int8.
	char const* sourceCode = R"(
		contract C {
			function f() public returns (int8) {
				uint8 x = 254;
				int8 y = 1;
				return y << x;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_left_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a, uint b) public returns (uint) {
				a <<= b;
				return a;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(8)), encodeArgs(u256(0x426600)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(16)), encodeArgs(u256(0x42660000)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(17)), encodeArgs(u256(0x84cc0000)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(240)), fromHex("4266000000000000000000000000000000000000000000000000000000000000"));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(256)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_left_assignment_different_type)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a, uint8 b) public returns (uint) {
				a <<= b;
				return a;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256,uint8)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint256,uint8)", u256(0x4266), u256(8)), encodeArgs(u256(0x426600)));
	ABI_CHECK(callContractFunction("f(uint256,uint8)", u256(0x4266), u256(16)), encodeArgs(u256(0x42660000)));
	ABI_CHECK(callContractFunction("f(uint256,uint8)", u256(0x4266), u256(17)), encodeArgs(u256(0x84cc0000)));
	ABI_CHECK(callContractFunction("f(uint256,uint8)", u256(0x4266), u256(240)), fromHex("4266000000000000000000000000000000000000000000000000000000000000"));
}

BOOST_AUTO_TEST_CASE(shift_right)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a, uint b) public returns (uint) {
				return a >> b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(8)), encodeArgs(u256(0x42)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(16)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(17)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(1)<<255, u256(5)), encodeArgs(u256(1)<<250));
}

BOOST_AUTO_TEST_CASE(shift_right_garbled)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint8 a, uint8 b) public returns (uint) {
				assembly {
					a := 0xffffffff
				}
				// Higher bits should be cleared before the shift
				return a >> b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(uint8,uint8)", u256(0x0), u256(4)), encodeArgs(u256(0xf)));
	ABI_CHECK(callContractFunction("f(uint8,uint8)", u256(0x0), u256(0x1004)), v2 ? encodeArgs() : encodeArgs(u256(0xf)));
}

BOOST_AUTO_TEST_CASE(shift_right_garbled_signed)
{
	char const* sourceCode = R"(
			contract C {
				function f(int8 a, uint8 b) public returns (int) {
					assembly {
						a := 0xfffffff0
					}
					// Higher bits should be signextended before the shift
					return a >> b;
				}
				function g(int8 a, uint8 b) public returns (int) {
					assembly {
						a := 0xf0
					}
					// Higher bits should be signextended before the shift
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(int8,uint8)", u256(0x0), u256(3)), encodeArgs(u256(-2)));
	ABI_CHECK(callContractFunction("f(int8,uint8)", u256(0x0), u256(4)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,uint8)", u256(0x0), u256(0xFF)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,uint8)", u256(0x0), u256(0x1003)), v2 ? encodeArgs() : encodeArgs(u256(-2)));
	ABI_CHECK(callContractFunction("f(int8,uint8)", u256(0x0), u256(0x1004)), v2 ? encodeArgs() : encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("g(int8,uint8)", u256(0x0), u256(3)), encodeArgs(u256(-2)));
	ABI_CHECK(callContractFunction("g(int8,uint8)", u256(0x0), u256(4)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("g(int8,uint8)", u256(0x0), u256(0xFF)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("g(int8,uint8)", u256(0x0), u256(0x1003)), v2 ? encodeArgs() : encodeArgs(u256(-2)));
	ABI_CHECK(callContractFunction("g(int8,uint8)", u256(0x0), u256(0x1004)), v2 ? encodeArgs() : encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_uint32)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint32 a, uint32 b) public returns (uint) {
				return a >> b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(8)), encodeArgs(u256(0x42)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(16)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(uint32,uint32)", u256(0x4266), u256(17)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_right_uint8)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint8 a, uint8 b) public returns (uint) {
				return a >> b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint8,uint8)", u256(0x66), u256(0)), encodeArgs(u256(0x66)));
	ABI_CHECK(callContractFunction("f(uint8,uint8)", u256(0x66), u256(8)), encodeArgs(u256(0x0)));
}

BOOST_AUTO_TEST_CASE(shift_right_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a, uint b) public returns (uint) {
				a >>= b;
				return a;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(8)), encodeArgs(u256(0x42)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(16)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", u256(0x4266), u256(17)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_right_assignment_signed)
{
	char const* sourceCode = R"(
			contract C {
				function f(int a, int b) public returns (int) {
					a >>= b;
					return a;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(0x4266), u256(0)), encodeArgs(u256(0x4266)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(0x4266), u256(8)), encodeArgs(u256(0x42)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(0x4266), u256(16)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(0x4266), u256(17)), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue)
{
	char const* sourceCode = R"(
		contract C {
			function f(int a, int b) public returns (int) {
				return a >> b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(0)), encodeArgs(u256(-4266)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(1)), encodeArgs(u256(-2133)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(17)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(0)), encodeArgs(u256(-4267)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(1)), encodeArgs(u256(-2134)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(17)), encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_literal)
{
	char const* sourceCode = R"(
			contract C {
				function f1() public pure returns (bool) {
					return (-4266 >> 0) == -4266;
				}
				function f2() public pure returns (bool) {
					return (-4266 >> 1) == -2133;
				}
				function f3() public pure returns (bool) {
					return (-4266 >> 4) == -267;
				}
				function f4() public pure returns (bool) {
					return (-4266 >> 8) == -17;
				}
				function f5() public pure returns (bool) {
					return (-4266 >> 16) == -1;
				}
				function f6() public pure returns (bool) {
					return (-4266 >> 17) == -1;
				}
				function g1() public pure returns (bool) {
					return (-4267 >> 0) == -4267;
				}
				function g2() public pure returns (bool) {
					return (-4267 >> 1) == -2134;
				}
				function g3() public pure returns (bool) {
					return (-4267 >> 4) == -267;
				}
				function g4() public pure returns (bool) {
					return (-4267 >> 8) == -17;
				}
				function g5() public pure returns (bool) {
					return (-4267 >> 16) == -1;
				}
				function g6() public pure returns (bool) {
					return (-4267 >> 17) == -1;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f1()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("f2()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("f3()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("f4()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("f5()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("f6()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g1()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g2()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g3()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g4()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g5()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g6()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_int8)
{
	char const* sourceCode = R"(
			contract C {
				function f(int8 a, int8 b) public returns (int) {
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-66), u256(0)), encodeArgs(u256(-66)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-66), u256(1)), encodeArgs(u256(-33)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-66), u256(4)), encodeArgs(u256(-5)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-66), u256(8)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-66), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-66), u256(17)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-67), u256(0)), encodeArgs(u256(-67)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-67), u256(1)), encodeArgs(u256(-34)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-67), u256(4)), encodeArgs(u256(-5)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-67), u256(8)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-67), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(-67), u256(17)), encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_signextend_int8)
{
	char const* sourceCode = R"(
			contract C {
				function f(int8 a, int8 b) public returns (int8) {
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(0x99u), u256(0)), v2 ? encodeArgs() : encodeArgs(u256(-103)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(0x99u), u256(1)), v2 ? encodeArgs() : encodeArgs(u256(-52)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(0x99u), u256(2)), v2 ? encodeArgs() : encodeArgs(u256(-26)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(0x99u), u256(4)), v2 ? encodeArgs() : encodeArgs(u256(-7)));
	ABI_CHECK(callContractFunction("f(int8,int8)", u256(0x99u), u256(8)), v2 ? encodeArgs() : encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_signextend_int16)
{
	char const* sourceCode = R"(
			contract C {
				function f(int16 a, int16 b) public returns (int16) {
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(0xFF99u), u256(0)), v2 ? encodeArgs() : encodeArgs(u256(-103)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(0xFF99u), u256(1)), v2 ? encodeArgs() : encodeArgs(u256(-52)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(0xFF99u), u256(2)), v2 ? encodeArgs() : encodeArgs(u256(-26)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(0xFF99u), u256(4)), v2 ? encodeArgs() : encodeArgs(u256(-7)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(0xFF99u), u256(8)), v2 ? encodeArgs() : encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_signextend_int32)
{
	char const* sourceCode = R"(
			contract C {
				function f(int32 a, int32 b) public returns (int32) {
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(0xFFFFFF99u), u256(0)), v2 ? encodeArgs() : encodeArgs(u256(-103)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(0xFFFFFF99u), u256(1)), v2 ? encodeArgs() : encodeArgs(u256(-52)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(0xFFFFFF99u), u256(2)), v2 ? encodeArgs() : encodeArgs(u256(-26)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(0xFFFFFF99u), u256(4)), v2 ? encodeArgs() : encodeArgs(u256(-7)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(0xFFFFFF99u), u256(8)), v2 ? encodeArgs() : encodeArgs(u256(-1)));
}


BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_int16)
{
	char const* sourceCode = R"(
			contract C {
				function f(int16 a, int16 b) public returns (int) {
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4266), u256(0)), encodeArgs(u256(-4266)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4266), u256(1)), encodeArgs(u256(-2133)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4266), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4266), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4266), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4266), u256(17)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4267), u256(0)), encodeArgs(u256(-4267)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4267), u256(1)), encodeArgs(u256(-2134)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4267), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4267), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4267), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int16,int16)", u256(-4267), u256(17)), encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_int32)
{
	char const* sourceCode = R"(
			contract C {
				function f(int32 a, int32 b) public returns (int) {
					return a >> b;
				}
			}
		)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4266), u256(0)), encodeArgs(u256(-4266)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4266), u256(1)), encodeArgs(u256(-2133)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4266), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4266), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4266), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4266), u256(17)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4267), u256(0)), encodeArgs(u256(-4267)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4267), u256(1)), encodeArgs(u256(-2134)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4267), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4267), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4267), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int32,int32)", u256(-4267), u256(17)), encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_right_negative_lvalue_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f(int a, int b) public returns (int) {
				a >>= b;
				return a;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(0)), encodeArgs(u256(-4266)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(1)), encodeArgs(u256(-2133)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4266), u256(17)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(0)), encodeArgs(u256(-4267)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(1)), encodeArgs(u256(-2134)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(4)), encodeArgs(u256(-267)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(8)), encodeArgs(u256(-17)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(16)), encodeArgs(u256(-1)));
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(-4267), u256(17)), encodeArgs(u256(-1)));
}

BOOST_AUTO_TEST_CASE(shift_negative_rvalue)
{
	char const* sourceCode = R"(
		contract C {
			function f(int a, int b) public returns (int) {
				return a << b;
			}
			function g(int a, int b) public returns (int) {
				return a >> b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(1), u256(-1)), encodeArgs());
	ABI_CHECK(callContractFunction("g(int256,int256)", u256(1), u256(-1)), encodeArgs());
}

BOOST_AUTO_TEST_CASE(shift_negative_rvalue_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f(int a, int b) public returns (int) {
				a <<= b;
				return a;
			}
			function g(int a, int b) public returns (int) {
				a >>= b;
				return a;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(int256,int256)", u256(1), u256(-1)), encodeArgs());
	ABI_CHECK(callContractFunction("g(int256,int256)", u256(1), u256(-1)), encodeArgs());
}

BOOST_AUTO_TEST_CASE(shift_constant_left_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint a) {
				a = 0x42;
				a <<= 8;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x4200)));
}

BOOST_AUTO_TEST_CASE(shift_constant_right_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint a) {
				a = 0x4200;
				a >>= 8;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x42)));
}

BOOST_AUTO_TEST_CASE(shift_cleanup)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint16 x) {
				x = 0xffff;
				x += 32;
				x <<= 8;
				x >>= 16;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x0)));
}

BOOST_AUTO_TEST_CASE(shift_cleanup_garbled)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint8 x) {
				assembly {
					x := 0xffff
				}
				x >>= 8;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x0)));
}

BOOST_AUTO_TEST_CASE(shift_overflow)
{
	char const* sourceCode = R"(
		contract C {
			function leftU(uint8 x, uint8 y) public returns (uint8) {
				return x << y;
			}
			function leftS(int8 x, int8 y) public returns (int8) {
				return x << y;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("leftU(uint8,uint8)", 255, 8), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("leftU(uint8,uint8)", 255, 1), encodeArgs(u256(254)));
	ABI_CHECK(callContractFunction("leftU(uint8,uint8)", 255, 0), encodeArgs(u256(255)));

	// Result is -128 and output is sign-extended, not zero-padded.
	ABI_CHECK(callContractFunction("leftS(int8,int8)", 1, 7), encodeArgs(u256(0) - 128));
	ABI_CHECK(callContractFunction("leftS(int8,int8)", 1, 6), encodeArgs(u256(64)));
}

BOOST_AUTO_TEST_CASE(shift_bytes)
{
	char const* sourceCode = R"(
		contract C {
			function left(bytes20 x, uint8 y) public returns (bytes20) {
				return x << y;
			}
			function right(bytes20 x, uint8 y) public returns (bytes20) {
				return x >> y;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("left(bytes20,uint8)", "12345678901234567890", 8 * 8), encodeArgs("901234567890" + string(8, 0)));
	ABI_CHECK(callContractFunction("right(bytes20,uint8)", "12345678901234567890", 8 * 8), encodeArgs(string(8, 0) + "123456789012"));
}

BOOST_AUTO_TEST_CASE(shift_bytes_cleanup)
{
	char const* sourceCode = R"(
		contract C {
			function left(uint8 y) public returns (bytes20) {
				bytes20 x;
				assembly { x := "12345678901234567890abcde" }
				return x << y;
			}
			function right(uint8 y) public returns (bytes20) {
				bytes20 x;
				assembly { x := "12345678901234567890abcde" }
				return x >> y;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("left(uint8)", 8 * 8), encodeArgs("901234567890" + string(8, 0)));
	ABI_CHECK(callContractFunction("right(uint8)", 8 * 8), encodeArgs(string(8, 0) + "123456789012"));
}

BOOST_AUTO_TEST_CASE(exp_cleanup)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (uint8 x) {
				uint8 y = uint8(2) ** uint8(8);
				return 0 ** y;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x1)));
}

BOOST_AUTO_TEST_CASE(exp_cleanup_direct)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (uint8 x) {
				return uint8(0) ** uint8(uint8(2) ** uint8(8));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x1)));
}

BOOST_AUTO_TEST_CASE(exp_cleanup_nonzero_base)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (uint8 x) {
				return uint8(0x166) ** uint8(uint8(2) ** uint8(8));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x1)));
}

BOOST_AUTO_TEST_CASE(cleanup_in_compound_assign)
{
	char const* sourceCode = R"(
		contract C {
			function test() public returns (uint, uint) {
				uint32 a = 0xffffffff;
				uint16 x = uint16(a);
				uint16 y = x;
				x /= 0x100;
				y = y / 0x100;
				return (x, y);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(0xff), u256(0xff)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_in_modifiers)
{
	char const* sourceCode = R"(
		contract C {
			modifier m {
				uint a = 1;
				assembly {
					a := 2
				}
				if (a != 2)
					revert();
				_;
			}
			function f() m public returns (bool) {
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(packed_storage_overflow)
{
	char const* sourceCode = R"(
		contract C {
			uint16 x = 0x1234;
			uint16 a = 0xffff;
			uint16 b;
			function f() public returns (uint, uint, uint, uint) {
				a++;
				uint c = b;
				delete b;
				a -= 2;
				return (x, c, b, a);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x1234), u256(0), u256(0), u256(0xfffe)));
}

BOOST_AUTO_TEST_CASE(contracts_separated_with_comment)
{
	char const* sourceCode = R"(
		contract C1 {}
		/**
		**/
		contract C2 {}
	)";
	compileAndRun(sourceCode, 0, "C1");
	compileAndRun(sourceCode, 0, "C2");
}


BOOST_AUTO_TEST_CASE(include_creation_bytecode_only_once)
{
	char const* sourceCode = R"(
		contract D {
			bytes a = hex"1237651237125387136581271652831736512837126583171583712358126123765123712538713658127165283173651283712658317158371235812612376512371253871365812716528317365128371265831715837123581261237651237125387136581271652831736512837126583171583712358126";
			bytes b = hex"1237651237125327136581271252831736512837126583171383712358126123765125712538713658127165253173651283712658357158371235812612376512371a5387136581271652a317365128371265a317158371235812612a765123712538a13658127165a83173651283712a58317158371235a126";
			constructor(uint) public {}
		}
		contract Double {
			function f() public {
				new D(2);
			}
			function g() public {
				new D(3);
			}
		}
		contract Single {
			function f() public {
				new D(2);
			}
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK_LE(
		double(m_compiler.object("Double").bytecode.size()),
		1.2 * double(m_compiler.object("Single").bytecode.size())
	);
}

BOOST_AUTO_TEST_CASE(recursive_structs)
{
	char const* sourceCode = R"(
		contract C {
			struct S {
				S[] x;
			}
			S sstorage;
			function f() public returns (uint) {
				S memory s;
				s.x = new S[](10);
				delete s;
				sstorage.x.length++;
				delete sstorage;
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(invalid_instruction)
{
	char const* sourceCode = R"(
		contract C {
			function f() public {
				assembly {
					invalid()
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(assert_require)
{
	char const* sourceCode = R"(
		contract C {
			function f() public {
				assert(false);
			}
			function g(bool val) public returns (bool) {
				assert(val == true);
				return true;
			}
			function h(bool val) public returns (bool) {
				require(val);
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("g(bool)", false), encodeArgs());
	ABI_CHECK(callContractFunction("g(bool)", true), encodeArgs(true));
	ABI_CHECK(callContractFunction("h(bool)", false), encodeArgs());
	ABI_CHECK(callContractFunction("h(bool)", true), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(revert)
{
	char const* sourceCode = R"(
		contract C {
			uint public a = 42;
			function f() public {
				a = 1;
				revert();
			}
			function g() public {
				a = 1;
				assembly {
					revert(0, 0)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(42)));
	ABI_CHECK(callContractFunction("g()"), encodeArgs());
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(42)));
}

BOOST_AUTO_TEST_CASE(revert_with_cause)
{
	char const* sourceCode = R"(
		contract D {
			string constant msg1 = "test1234567890123456789012345678901234567890";
			string msg2 = "test1234567890123456789012345678901234567890";
			function f() public {
				revert("test123");
			}
			function g() public {
				revert("test1234567890123456789012345678901234567890");
			}
			function h() public {
				revert(msg1);
			}
			function i() public {
				revert(msg2);
			}
			function j() public {
				string memory msg3 = "test1234567890123456789012345678901234567890";
				revert(msg3);
			}
		}
		contract C {
			D d = new D();
			function forward(address target, bytes memory data) internal returns (bool success, bytes memory retval) {
				uint retsize;
				assembly {
					success := call(not(0), target, 0, add(data, 0x20), mload(data), 0, 0)
					retsize := returndatasize()
				}
				retval = new bytes(retsize);
				assembly {
					returndatacopy(add(retval, 0x20), 0, returndatasize())
				}
			}
			function f() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function g() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function h() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function i() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function j() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
		}
	)";
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		compileAndRun(sourceCode, 0, "C");
		bytes const errorSignature = bytes{0x08, 0xc3, 0x79, 0xa0};
		ABI_CHECK(callContractFunction("f()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 7, "test123") + bytes(28, 0));
		ABI_CHECK(callContractFunction("g()"), encodeArgs(0, 0x40, 0x84) + errorSignature + encodeArgs(0x20, 44, "test1234567890123456789012345678901234567890") + bytes(28, 0));
		ABI_CHECK(callContractFunction("h()"), encodeArgs(0, 0x40, 0x84) + errorSignature + encodeArgs(0x20, 44, "test1234567890123456789012345678901234567890") + bytes(28, 0));
		ABI_CHECK(callContractFunction("i()"), encodeArgs(0, 0x40, 0x84) + errorSignature + encodeArgs(0x20, 44, "test1234567890123456789012345678901234567890") + bytes(28, 0));
		ABI_CHECK(callContractFunction("j()"), encodeArgs(0, 0x40, 0x84) + errorSignature + encodeArgs(0x20, 44, "test1234567890123456789012345678901234567890") + bytes(28, 0));
	}
}

BOOST_AUTO_TEST_CASE(require_with_message)
{
	char const* sourceCode = R"(
		contract D {
			bool flag = false;
			string storageError = "abc";
			string constant constantError = "abc";
			function f(uint x) public {
				require(x > 7, "failed");
			}
			function g() public {
				// As a side-effect of internalFun, the flag will be set to true
				// (even if the condition is true),
				// but it will only throw in the next evaluation.
				bool flagCopy = flag;
				require(flagCopy == false, internalFun());
			}
			function internalFun() public returns (string memory) {
				flag = true;
				return "only on second run";
			}
			function h() public {
				require(false, storageError);
			}
			function i() public {
				require(false, constantError);
			}
			function j() public {
				string memory errMsg = "msg";
				require(false, errMsg);
			}
		}
		contract C {
			D d = new D();
			function forward(address target, bytes memory data) internal returns (bool success, bytes memory retval) {
				uint retsize;
				assembly {
					success := call(not(0), target, 0, add(data, 0x20), mload(data), 0, 0)
					retsize := returndatasize()
				}
				retval = new bytes(retsize);
				assembly {
					returndatacopy(add(retval, 0x20), 0, returndatasize())
				}
			}
			function f(uint x) public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function g() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function h() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function i() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function j() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
		}
	)";
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		compileAndRun(sourceCode, 0, "C");
		bytes const errorSignature = bytes{0x08, 0xc3, 0x79, 0xa0};
		ABI_CHECK(callContractFunction("f(uint256)", 8), encodeArgs(1, 0x40, 0));
		ABI_CHECK(callContractFunction("f(uint256)", 5), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 6, "failed") + bytes(28, 0));
		ABI_CHECK(callContractFunction("g()"), encodeArgs(1, 0x40, 0));
		ABI_CHECK(callContractFunction("g()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 18, "only on second run") + bytes(28, 0));
		ABI_CHECK(callContractFunction("h()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 3, "abc") + bytes(28, 0));
		ABI_CHECK(callContractFunction("i()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 3, "abc") + bytes(28, 0));
		ABI_CHECK(callContractFunction("j()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 3, "msg") + bytes(28, 0));
	}
}

BOOST_AUTO_TEST_CASE(bubble_up_error_messages)
{
	char const* sourceCode = R"(
		contract D {
			function f() public {
				revert("message");
			}
			function g() public {
				this.f();
			}
		}
		contract C {
			D d = new D();
			function forward(address target, bytes memory data) internal returns (bool success, bytes memory retval) {
				uint retsize;
				assembly {
					success := call(not(0), target, 0, add(data, 0x20), mload(data), 0, 0)
					retsize := returndatasize()
				}
				retval = new bytes(retsize);
				assembly {
					returndatacopy(add(retval, 0x20), 0, returndatasize())
				}
			}
			function f() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
			function g() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
		}
	)";
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		compileAndRun(sourceCode, 0, "C");
		bytes const errorSignature = bytes{0x08, 0xc3, 0x79, 0xa0};
		ABI_CHECK(callContractFunction("f()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 7, "message") + bytes(28, 0));
		ABI_CHECK(callContractFunction("g()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 7, "message") + bytes(28, 0));
	}
}

BOOST_AUTO_TEST_CASE(bubble_up_error_messages_through_transfer)
{
	char const* sourceCode = R"(
		contract D {
			function() external payable {
				revert("message");
			}
			function f() public {
				address(this).transfer(0);
			}
		}
		contract C {
			D d = new D();
			function forward(address target, bytes memory data) internal returns (bool success, bytes memory retval) {
				uint retsize;
				assembly {
					success := call(not(0), target, 0, add(data, 0x20), mload(data), 0, 0)
					retsize := returndatasize()
				}
				retval = new bytes(retsize);
				assembly {
					returndatacopy(add(retval, 0x20), 0, returndatasize())
				}
			}
			function f() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
		}
	)";
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		compileAndRun(sourceCode, 0, "C");
		bytes const errorSignature = bytes{0x08, 0xc3, 0x79, 0xa0};
		ABI_CHECK(callContractFunction("f()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 7, "message") + bytes(28, 0));
	}
}

BOOST_AUTO_TEST_CASE(bubble_up_error_messages_through_create)
{
	char const* sourceCode = R"(
		contract E {
			constructor() public {
				revert("message");
			}
		}
		contract D {
			function f() public {
				E x = new E();
			}
		}
		contract C {
			D d = new D();
			function forward(address target, bytes memory data) internal returns (bool success, bytes memory retval) {
				uint retsize;
				assembly {
					success := call(not(0), target, 0, add(data, 0x20), mload(data), 0, 0)
					retsize := returndatasize()
				}
				retval = new bytes(retsize);
				assembly {
					returndatacopy(add(retval, 0x20), 0, returndatasize())
				}
			}
			function f() public returns (bool, bytes memory) {
				return forward(address(d), msg.data);
			}
		}
	)";
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		compileAndRun(sourceCode, 0, "C");
		bytes const errorSignature = bytes{0x08, 0xc3, 0x79, 0xa0};
		ABI_CHECK(callContractFunction("f()"), encodeArgs(0, 0x40, 0x64) + errorSignature + encodeArgs(0x20, 7, "message") + bytes(28, 0));
	}
}

BOOST_AUTO_TEST_CASE(negative_stack_height)
{
	// This code was causing negative stack height during code generation
	// because the stack height was not adjusted at the beginning of functions.
	char const* sourceCode = R"(
		contract C {
			mapping(uint => Invoice) public invoices;
			struct Invoice {
				uint AID;
				bool Aboola;
				bool Aboolc;
				bool exists;
			}
			function nredit(uint startindex) public pure returns(uint[500] memory CIDs, uint[500] memory dates, uint[500] memory RIDs, bool[500] memory Cboolas, uint[500] memory amounts){}
			function return500InvoicesByDates(uint begindate, uint enddate, uint startindex) public view returns(uint[500] memory AIDs, bool[500] memory Aboolas, uint[500] memory dates, bytes32[3][500] memory Abytesas, bytes32[3][500] memory bytesbs, bytes32[2][500] memory bytescs, uint[500] memory amounts, bool[500] memory Aboolbs, bool[500] memory Aboolcs){}
			function return500PaymentsByDates(uint begindate, uint enddate, uint startindex) public view returns(uint[500] memory BIDs, uint[500] memory dates, uint[500] memory RIDs, bool[500] memory Bboolas, bytes32[3][500] memory bytesbs,bytes32[2][500] memory bytescs, uint[500] memory amounts, bool[500] memory Bboolbs){}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
}

BOOST_AUTO_TEST_CASE(literal_empty_string)
{
	char const* sourceCode = R"(
		contract C {
			bytes32 public x;
			uint public a;
			function f(bytes32 _x, uint _a) public {
				x = _x;
				a = _a;
			}
			function g() public {
				this.f("", 2);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("g()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(scientific_notation)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				return 2e10 wei;
			}
			function g() public returns (uint) {
				return 200e-2 wei;
			}
			function h() public returns (uint) {
				return 2.5e1;
			}
			function i() public returns (int) {
				return -2e10;
			}
			function j() public returns (int) {
				return -200e-2;
			}
			function k() public returns (int) {
				return -2.5e1;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(20000000000)));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(u256(25)));
	ABI_CHECK(callContractFunction("i()"), encodeArgs(u256(-20000000000)));
	ABI_CHECK(callContractFunction("j()"), encodeArgs(u256(-2)));
	ABI_CHECK(callContractFunction("k()"), encodeArgs(u256(-25)));
}

BOOST_AUTO_TEST_CASE(interface_contract)
{
	char const* sourceCode = R"(
		interface I {
			event A();
			function f() external returns (bool);
			function() external payable;
		}

		contract A is I {
			function f() public returns (bool) {
				return g();
			}

			function g() public returns (bool) {
				return true;
			}

			function() external payable {
			}
		}

		contract C {
			function f(address payable _interfaceAddress) public returns (bool) {
				I i = I(_interfaceAddress);
				return i.f();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "A");
	u160 const recipient = m_contractAddress;
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(address)", recipient), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(keccak256_assembly)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (bytes32 ret) {
				assembly {
					ret := keccak256(0, 0)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), fromHex("0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"));
}

BOOST_AUTO_TEST_CASE(multi_modifiers)
{
	// This triggered a bug in some version because the variable in the modifier was not
	// unregistered correctly.
	char const* sourceCode = R"(
		contract C {
			uint public x;
			modifier m1 {
				address a1 = msg.sender;
				x++;
				_;
			}
			function f1() m1() public {
				x += 7;
			}
			function f2() m1() public {
				x += 3;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f1()"), bytes());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(8)));
	ABI_CHECK(callContractFunction("f2()"), bytes());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(12)));
}

BOOST_AUTO_TEST_CASE(inlineasm_empty_let)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (uint a, uint b) {
				assembly {
					let x
					let y, z
					a := x
					b := z
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0), u256(0)));
}

BOOST_AUTO_TEST_CASE(bare_call_invalid_address)
{
	char const* sourceCode = R"YY(
		contract C {
			/// Calling into non-existant account is successful (creates the account)
			function f() external returns (bool) {
				(bool success,) = address(0x4242).call("");
				return success;
			}
			function h() external returns (bool) {
				(bool success,) = address(0x4242).delegatecall("");
				return success;
			}
		}
	)YY";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(u256(1)));

	if (dev::test::Options::get().evmVersion().hasStaticCall())
	{
		char const* sourceCode = R"YY(
			contract C {
				function f() external returns (bool, bytes memory) {
					return address(0x4242).staticcall("");
				}
			}
		)YY";
		compileAndRun(sourceCode, 0, "C");
		ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), 0x40, 0x00));
	}
}

BOOST_AUTO_TEST_CASE(bare_call_return_data)
{
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		vector<string> calltypes = {"call", "delegatecall"};
		if (dev::test::Options::get().evmVersion().hasStaticCall())
			calltypes.emplace_back("staticcall");
		for (string const& calltype: calltypes)
		{
			string sourceCode = R"DELIMITER(
				contract A {
					constructor() public {
					}
					function return_bool() public pure returns(bool) {
						return true;
					}
					function return_int32() public pure returns(int32) {
						return -32;
					}
					function return_uint32() public pure returns(uint32) {
						return 0x3232;
					}
					function return_int256() public pure returns(int256) {
						return -256;
					}
					function return_uint256() public pure returns(uint256) {
						return 0x256256;
					}
					function return_bytes4() public pure returns(bytes4) {
						return 0xabcd0012;
					}
					function return_multi() public pure returns(bool, uint32, bytes4) {
						return (false, 0x3232, 0xabcd0012);
					}
					function return_bytes() public pure returns(bytes memory b) {
						b = new bytes(2);
						b[0] = 0x42;
						b[1] = 0x21;
					}
				}
				contract C {
					A addr;
					constructor() public {
						addr = new A();
					}
					function f(string memory signature) public returns (bool, bytes memory) {
						return address(addr).)DELIMITER" + calltype + R"DELIMITER((abi.encodeWithSignature(signature));
					}
					function check_bool() external returns (bool) {
						(bool success, bytes memory data) = f("return_bool()");
						assert(success);
						bool a = abi.decode(data, (bool));
						assert(a);
						return true;
					}
					function check_int32() external returns (bool) {
						(bool success, bytes memory data) = f("return_int32()");
						assert(success);
						int32 a = abi.decode(data, (int32));
						assert(a == -32);
						return true;
					}
					function check_uint32() external returns (bool) {
						(bool success, bytes memory data) = f("return_uint32()");
						assert(success);
						uint32 a = abi.decode(data, (uint32));
						assert(a == 0x3232);
						return true;
					}
					function check_int256() external returns (bool) {
						(bool success, bytes memory data) = f("return_int256()");
						assert(success);
						int256 a = abi.decode(data, (int256));
						assert(a == -256);
						return true;
					}
					function check_uint256() external returns (bool) {
						(bool success, bytes memory data) = f("return_uint256()");
						assert(success);
						uint256 a = abi.decode(data, (uint256));
						assert(a == 0x256256);
						return true;
					}
					function check_bytes4() external returns (bool) {
						(bool success, bytes memory data) = f("return_bytes4()");
						assert(success);
						bytes4 a = abi.decode(data, (bytes4));
						assert(a == 0xabcd0012);
						return true;
					}
					function check_multi() external returns (bool) {
						(bool success, bytes memory data) = f("return_multi()");
						assert(success);
						(bool a, uint32 b, bytes4 c) = abi.decode(data, (bool, uint32, bytes4));
						assert(a == false && b == 0x3232 && c == 0xabcd0012);
						return true;
					}
					function check_bytes() external returns (bool) {
						(bool success, bytes memory data) = f("return_bytes()");
						assert(success);
						(bytes memory d) = abi.decode(data, (bytes));
						assert(d.length == 2 && d[0] == 0x42 && d[1] == 0x21);
						return true;
					}
				}
			)DELIMITER";
			compileAndRun(sourceCode, 0, "C");
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_bool()"))), encodeArgs(true, 0x40, 0x20, true));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_int32()"))), encodeArgs(true, 0x40, 0x20, u256(-32)));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_uint32()"))), encodeArgs(true, 0x40, 0x20, u256(0x3232)));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_int256()"))), encodeArgs(true, 0x40, 0x20, u256(-256)));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_uint256()"))), encodeArgs(true, 0x40, 0x20, u256(0x256256)));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_bytes4()"))), encodeArgs(true, 0x40, 0x20, u256(0xabcd0012) << (28*8)));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_multi()"))), encodeArgs(true, 0x40, 0x60, false, u256(0x3232), u256(0xabcd0012) << (28*8)));
			ABI_CHECK(callContractFunction("f(string)", encodeDyn(string("return_bytes()"))), encodeArgs(true, 0x40, 0x60, 0x20, 0x02, encode(bytes{0x42,0x21}, false)));
			ABI_CHECK(callContractFunction("check_bool()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_int32()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_uint32()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_int256()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_uint256()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_bytes4()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_multi()"), encodeArgs(true));
			ABI_CHECK(callContractFunction("check_bytes()"), encodeArgs(true));
		}
	}
}

BOOST_AUTO_TEST_CASE(delegatecall_return_value)
{
	if (dev::test::Options::get().evmVersion().supportsReturndata())
	{
		char const* sourceCode = R"DELIMITER(
			contract C {
				uint value;
				function set(uint _value) external {
					value = _value;
				}
				function get() external view returns (uint) {
					return value;
				}
				function get_delegated() external returns (bool, bytes memory) {
					return address(this).delegatecall(abi.encodeWithSignature("get()"));
				}
				function assert0() external view {
					assert(value == 0);
				}
				function assert0_delegated() external returns (bool, bytes memory) {
					return address(this).delegatecall(abi.encodeWithSignature("assert0()"));
				}
			}
		)DELIMITER";
		compileAndRun(sourceCode, 0, "C");
		ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(0)));
		ABI_CHECK(callContractFunction("assert0_delegated()"), encodeArgs(u256(1), 0x40, 0x00));
		ABI_CHECK(callContractFunction("get_delegated()"), encodeArgs(u256(1), 0x40, 0x20, 0x00));
		ABI_CHECK(callContractFunction("set(uint256)", u256(1)), encodeArgs());
		ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(1)));
		ABI_CHECK(callContractFunction("assert0_delegated()"), encodeArgs(u256(0), 0x40, 0x00));
		ABI_CHECK(callContractFunction("get_delegated()"), encodeArgs(u256(1), 0x40, 0x20, 1));
		ABI_CHECK(callContractFunction("set(uint256)", u256(42)), encodeArgs());
		ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(42)));
		ABI_CHECK(callContractFunction("assert0_delegated()"), encodeArgs(u256(0), 0x40, 0x00));
		ABI_CHECK(callContractFunction("get_delegated()"), encodeArgs(u256(1), 0x40, 0x20, 42));
	}
	else
	{
		char const* sourceCode = R"DELIMITER(
			contract C {
				uint value;
				function set(uint _value) external {
					value = _value;
				}
				function get() external view returns (uint) {
					return value;
				}
				function get_delegated() external returns (bool) {
					(bool success,) = address(this).delegatecall(abi.encodeWithSignature("get()"));
					return success;
				}
				function assert0() external view {
					assert(value == 0);
				}
				function assert0_delegated() external returns (bool) {
					(bool success,) = address(this).delegatecall(abi.encodeWithSignature("assert0()"));
					return success;
				}
			}
		)DELIMITER";
		compileAndRun(sourceCode, 0, "C");
		ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(0)));
		ABI_CHECK(callContractFunction("assert0_delegated()"), encodeArgs(u256(1)));
		ABI_CHECK(callContractFunction("get_delegated()"), encodeArgs(u256(1)));
		ABI_CHECK(callContractFunction("set(uint256)", u256(1)), encodeArgs());
		ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(1)));
		ABI_CHECK(callContractFunction("assert0_delegated()"), encodeArgs(u256(0)));
		ABI_CHECK(callContractFunction("get_delegated()"), encodeArgs(u256(1)));
		ABI_CHECK(callContractFunction("set(uint256)", u256(42)), encodeArgs());
		ABI_CHECK(callContractFunction("get()"), encodeArgs(u256(42)));
		ABI_CHECK(callContractFunction("assert0_delegated()"), encodeArgs(u256(0)));
		ABI_CHECK(callContractFunction("get_delegated()"), encodeArgs(u256(1)));
	}
}

BOOST_AUTO_TEST_CASE(function_types_sig)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			function f() public pure returns (bytes4) {
				return this.f.selector;
			}
			function g() public returns (bytes4) {
				function () pure external returns (bytes4) fun = this.f;
				return fun.selector;
			}
			function h() public returns (bytes4) {
				function () pure external returns (bytes4) fun = this.f;
				return fun.selector;
			}
			function i() public pure returns (bytes4) {
				return this.x.selector;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(asString(FixedHash<4>(dev::keccak256("f()")).asBytes())));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(asString(FixedHash<4>(dev::keccak256("f()")).asBytes())));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(asString(FixedHash<4>(dev::keccak256("f()")).asBytes())));
	ABI_CHECK(callContractFunction("i()"), encodeArgs(asString(FixedHash<4>(dev::keccak256("x()")).asBytes())));
}

BOOST_AUTO_TEST_CASE(constant_string)
{
	char const* sourceCode = R"(
		contract C {
			bytes constant a = "\x03\x01\x02";
			bytes constant b = hex"030102";
			string constant c = "hello";
			function f() public returns (bytes memory) {
				return a;
			}
			function g() public returns (bytes memory) {
				return b;
			}
			function h() public returns (bytes memory) {
				return bytes(c);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeDyn(string("\x03\x01\x02")));
	ABI_CHECK(callContractFunction("g()"), encodeDyn(string("\x03\x01\x02")));
	ABI_CHECK(callContractFunction("h()"), encodeDyn(string("hello")));
}

BOOST_AUTO_TEST_CASE(address_overload_resolution)
{
	char const* sourceCode = R"(
		contract C {
			function balance() public returns (uint) {
				return 1;
			}
			function transfer(uint amount) public returns (uint) {
				return amount;
			}
		}
		contract D {
			function f() public returns (uint) {
				return (new C()).balance();
			}
			function g() public returns (uint) {
				return (new C()).transfer(5);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	BOOST_CHECK(callContractFunction("f()") == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("g()") == encodeArgs(u256(5)));
}

BOOST_AUTO_TEST_CASE(snark)
{
	char const* sourceCode = R"(
	library Pairing {
		struct G1Point {
			uint X;
			uint Y;
		}
		// Encoding of field elements is: X[0] * z + X[1]
		struct G2Point {
			uint[2] X;
			uint[2] Y;
		}

		/// @return the generator of G1
		function P1() internal returns (G1Point memory) {
			return G1Point(1, 2);
		}

		/// @return the generator of G2
		function P2() internal returns (G2Point memory) {
			return G2Point(
				[11559732032986387107991004021392285783925812861821192530917403151452391805634,
				 10857046999023057135944570762232829481370756359578518086990519993285655852781],
				[4082367875863433681332203403145435568316851327593401208105741076214120093531,
				 8495653923123431417604973247489272438418190587263600148770280649306958101930]
			);
		}

		/// @return the negation of p, i.e. p.add(p.negate()) should be zero.
		function negate(G1Point memory p) internal returns (G1Point memory) {
			// The prime q in the base field F_q for G1
			uint q = 21888242871839275222246405745257275088696311157297823662689037894645226208583;
			if (p.X == 0 && p.Y == 0)
				return G1Point(0, 0);
			return G1Point(p.X, q - (p.Y % q));
		}

		/// @return the sum of two points of G1
		function add(G1Point memory p1, G1Point memory p2) internal returns (G1Point memory r) {
			uint[4] memory input;
			input[0] = p1.X;
			input[1] = p1.Y;
			input[2] = p2.X;
			input[3] = p2.Y;
			bool success;
			assembly {
				success := call(sub(gas, 2000), 6, 0, input, 0xc0, r, 0x60)
				// Use "invalid" to make gas estimation work
				switch success case 0 { invalid() }
			}
			require(success);
		}

		/// @return the product of a point on G1 and a scalar, i.e.
		/// p == p.mul(1) and p.add(p) == p.mul(2) for all points p.
		function mul(G1Point memory p, uint s) internal returns (G1Point memory r) {
			uint[3] memory input;
			input[0] = p.X;
			input[1] = p.Y;
			input[2] = s;
			bool success;
			assembly {
				success := call(sub(gas, 2000), 7, 0, input, 0x80, r, 0x60)
				// Use "invalid" to make gas estimation work
				switch success case 0 { invalid() }
			}
			require(success);
		}

		/// @return the result of computing the pairing check
		/// e(p1[0], p2[0]) *  .... * e(p1[n], p2[n]) == 1
		/// For example pairing([P1(), P1().negate()], [P2(), P2()]) should
		/// return true.
		function pairing(G1Point[] memory p1, G2Point[] memory p2) internal returns (bool) {
			require(p1.length == p2.length);
			uint elements = p1.length;
			uint inputSize = p1.length * 6;
			uint[] memory input = new uint[](inputSize);
			for (uint i = 0; i < elements; i++)
			{
				input[i * 6 + 0] = p1[i].X;
				input[i * 6 + 1] = p1[i].Y;
				input[i * 6 + 2] = p2[i].X[0];
				input[i * 6 + 3] = p2[i].X[1];
				input[i * 6 + 4] = p2[i].Y[0];
				input[i * 6 + 5] = p2[i].Y[1];
			}
			uint[1] memory out;
			bool success;
			assembly {
				success := call(sub(gas, 2000), 8, 0, add(input, 0x20), mul(inputSize, 0x20), out, 0x20)
				// Use "invalid" to make gas estimation work
				switch success case 0 { invalid() }
			}
			require(success);
			return out[0] != 0;
		}
		function pairingProd2(G1Point memory a1, G2Point memory a2, G1Point memory b1, G2Point memory b2) internal returns (bool) {
			G1Point[] memory p1 = new G1Point[](2);
			G2Point[] memory p2 = new G2Point[](2);
			p1[0] = a1;
			p1[1] = b1;
			p2[0] = a2;
			p2[1] = b2;
			return pairing(p1, p2);
		}
		function pairingProd3(
				G1Point memory a1, G2Point memory a2,
				G1Point memory b1, G2Point memory b2,
				G1Point memory c1, G2Point memory c2
		) internal returns (bool) {
			G1Point[] memory p1 = new G1Point[](3);
			G2Point[] memory p2 = new G2Point[](3);
			p1[0] = a1;
			p1[1] = b1;
			p1[2] = c1;
			p2[0] = a2;
			p2[1] = b2;
			p2[2] = c2;
			return pairing(p1, p2);
		}
		function pairingProd4(
				G1Point memory a1, G2Point memory a2,
				G1Point memory b1, G2Point memory b2,
				G1Point memory c1, G2Point memory c2,
				G1Point memory d1, G2Point memory d2
		) internal returns (bool) {
			G1Point[] memory p1 = new G1Point[](4);
			G2Point[] memory p2 = new G2Point[](4);
			p1[0] = a1;
			p1[1] = b1;
			p1[2] = c1;
			p1[3] = d1;
			p2[0] = a2;
			p2[1] = b2;
			p2[2] = c2;
			p2[3] = d2;
			return pairing(p1, p2);
		}
	}

	contract Test {
		using Pairing for *;
		struct VerifyingKey {
			Pairing.G2Point A;
			Pairing.G1Point B;
			Pairing.G2Point C;
			Pairing.G2Point gamma;
			Pairing.G1Point gammaBeta1;
			Pairing.G2Point gammaBeta2;
			Pairing.G2Point Z;
			Pairing.G1Point[] IC;
		}
		struct Proof {
			Pairing.G1Point A;
			Pairing.G1Point A_p;
			Pairing.G2Point B;
			Pairing.G1Point B_p;
			Pairing.G1Point C;
			Pairing.G1Point C_p;
			Pairing.G1Point K;
			Pairing.G1Point H;
		}
		function f() public returns (bool) {
			Pairing.G1Point memory p1;
			Pairing.G1Point memory p2;
			p1.X = 1; p1.Y = 2;
			p2.X = 1; p2.Y = 2;
			Pairing.G1Point memory explict_sum = Pairing.add(p1, p2);
			Pairing.G1Point memory scalar_prod = Pairing.mul(p1, 2);
			return (explict_sum.X == scalar_prod.X &&
					explict_sum.Y == scalar_prod.Y);
		}
		function g() public returns (bool) {
			Pairing.G1Point memory x = Pairing.add(Pairing.P1(), Pairing.negate(Pairing.P1()));
			// should be zero
			return (x.X == 0 && x.Y == 0);
		}
		function testMul() public returns (bool) {
			Pairing.G1Point memory p;
			// @TODO The points here are reported to be not well-formed
			p.X = 14125296762497065001182820090155008161146766663259912659363835465243039841726;
			p.Y = 16229134936871442251132173501211935676986397196799085184804749187146857848057;
			p = Pairing.mul(p, 13986731495506593864492662381614386532349950841221768152838255933892789078521);
			return
				p.X == 18256332256630856740336504687838346961237861778318632856900758565550522381207 &&
				p.Y == 6976682127058094634733239494758371323697222088503263230319702770853579280803;
		}
		function pair() public returns (bool) {
			Pairing.G2Point memory fiveTimesP2 = Pairing.G2Point(
				[4540444681147253467785307942530223364530218361853237193970751657229138047649, 20954117799226682825035885491234530437475518021362091509513177301640194298072],
				[11631839690097995216017572651900167465857396346217730511548857041925508482915, 21508930868448350162258892668132814424284302804699005394342512102884055673846]
			);
			// The prime p in the base field F_p for G1
			uint p = 21888242871839275222246405745257275088696311157297823662689037894645226208583;
			Pairing.G1Point[] memory g1points = new Pairing.G1Point[](2);
			Pairing.G2Point[] memory g2points = new Pairing.G2Point[](2);
			// check e(5 P1, P2)e(-P1, 5 P2) == 1
			g1points[0] = Pairing.P1().mul(5);
			g1points[1] = Pairing.P1().negate();
			g2points[0] = Pairing.P2();
			g2points[1] = fiveTimesP2;
			if (!Pairing.pairing(g1points, g2points))
				return false;
			// check e(P1, P2)e(-P1, P2) == 1
			g1points[0] = Pairing.P1();
			g1points[1] = Pairing.P1();
			g1points[1].Y = p - g1points[1].Y;
			g2points[0] = Pairing.P2();
			g2points[1] = Pairing.P2();
			if (!Pairing.pairing(g1points, g2points))
				return false;
			return true;
		}
		function verifyingKey() internal returns (VerifyingKey memory vk) {
			vk.A = Pairing.G2Point([0x209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf7, 0x04bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a41678], [0x2bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d, 0x120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550]);
			vk.B = Pairing.G1Point(0x2eca0c7238bf16e83e7a1e6c5d49540685ff51380f309842a98561558019fc02, 0x03d3260361bb8451de5ff5ecd17f010ff22f5c31cdf184e9020b06fa5997db84);
			vk.C = Pairing.G2Point([0x2e89718ad33c8bed92e210e81d1853435399a271913a6520736a4729cf0d51eb, 0x01a9e2ffa2e92599b68e44de5bcf354fa2642bd4f26b259daa6f7ce3ed57aeb3], [0x14a9a87b789a58af499b314e13c3d65bede56c07ea2d418d6874857b70763713, 0x178fb49a2d6cd347dc58973ff49613a20757d0fcc22079f9abd10c3baee24590]);
			vk.gamma = Pairing.G2Point([0x25f83c8b6ab9de74e7da488ef02645c5a16a6652c3c71a15dc37fe3a5dcb7cb1, 0x22acdedd6308e3bb230d226d16a105295f523a8a02bfc5e8bd2da135ac4c245d], [0x065bbad92e7c4e31bf3757f1fe7362a63fbfee50e7dc68da116e67d600d9bf68, 0x06d302580dc0661002994e7cd3a7f224e7ddc27802777486bf80f40e4ca3cfdb]);
			vk.gammaBeta1 = Pairing.G1Point(0x15794ab061441e51d01e94640b7e3084a07e02c78cf3103c542bc5b298669f21, 0x14db745c6780e9df549864cec19c2daf4531f6ec0c89cc1c7436cc4d8d300c6d);
			vk.gammaBeta2 = Pairing.G2Point([0x1f39e4e4afc4bc74790a4a028aff2c3d2538731fb755edefd8cb48d6ea589b5e, 0x283f150794b6736f670d6a1033f9b46c6f5204f50813eb85c8dc4b59db1c5d39], [0x140d97ee4d2b36d99bc49974d18ecca3e7ad51011956051b464d9e27d46cc25e, 0x0764bb98575bd466d32db7b15f582b2d5c452b36aa394b789366e5e3ca5aabd4]);
			vk.Z = Pairing.G2Point([0x217cee0a9ad79a4493b5253e2e4e3a39fc2df38419f230d341f60cb064a0ac29, 0x0a3d76f140db8418ba512272381446eb73958670f00cf46f1d9e64cba057b53c], [0x26f64a8ec70387a13e41430ed3ee4a7db2059cc5fc13c067194bcc0cb49a9855, 0x2fd72bd9edb657346127da132e5b82ab908f5816c826acb499e22f2412d1a2d7]);
			vk.IC = new Pairing.G1Point[](10);
			vk.IC[0] = Pairing.G1Point(0x0aee46a7ea6e80a3675026dfa84019deee2a2dedb1bbe11d7fe124cb3efb4b5a, 0x044747b6e9176e13ede3a4dfd0d33ccca6321b9acd23bf3683a60adc0366ebaf);
			vk.IC[1] = Pairing.G1Point(0x1e39e9f0f91fa7ff8047ffd90de08785777fe61c0e3434e728fce4cf35047ddc, 0x2e0b64d75ebfa86d7f8f8e08abbe2e7ae6e0a1c0b34d028f19fa56e9450527cb);
			vk.IC[2] = Pairing.G1Point(0x1c36e713d4d54e3a9644dffca1fc524be4868f66572516025a61ca542539d43f, 0x042dcc4525b82dfb242b09cb21909d5c22643dcdbe98c4d082cc2877e96b24db);
			vk.IC[3] = Pairing.G1Point(0x17d5d09b4146424bff7e6fb01487c477bbfcd0cdbbc92d5d6457aae0b6717cc5, 0x02b5636903efbf46db9235bbe74045d21c138897fda32e079040db1a16c1a7a1);
			vk.IC[4] = Pairing.G1Point(0x0f103f14a584d4203c27c26155b2c955f8dfa816980b24ba824e1972d6486a5d, 0x0c4165133b9f5be17c804203af781bcf168da7386620479f9b885ecbcd27b17b);
			vk.IC[5] = Pairing.G1Point(0x232063b584fb76c8d07995bee3a38fa7565405f3549c6a918ddaa90ab971e7f8, 0x2ac9b135a81d96425c92d02296322ad56ffb16299633233e4880f95aafa7fda7);
			vk.IC[6] = Pairing.G1Point(0x09b54f111d3b2d1b2fe1ae9669b3db3d7bf93b70f00647e65c849275de6dc7fe, 0x18b2e77c63a3e400d6d1f1fbc6e1a1167bbca603d34d03edea231eb0ab7b14b4);
			vk.IC[7] = Pairing.G1Point(0x0c54b42137b67cc268cbb53ac62b00ecead23984092b494a88befe58445a244a, 0x18e3723d37fae9262d58b548a0575f59d9c3266db7afb4d5739555837f6b8b3e);
			vk.IC[8] = Pairing.G1Point(0x0a6de0e2240aa253f46ce0da883b61976e3588146e01c9d8976548c145fe6e4a, 0x04fbaa3a4aed4bb77f30ebb07a3ec1c7d77a7f2edd75636babfeff97b1ea686e);
			vk.IC[9] = Pairing.G1Point(0x111e2e2a5f8828f80ddad08f9f74db56dac1cc16c1cb278036f79a84cf7a116f, 0x1d7d62e192b219b9808faa906c5ced871788f6339e8d91b83ac1343e20a16b30);
		}
		function verify(uint[] memory input, Proof memory proof) internal returns (uint) {
			VerifyingKey memory vk = verifyingKey();
			require(input.length + 1 == vk.IC.length);
			// Compute the linear combination vk_x
			Pairing.G1Point memory vk_x = Pairing.G1Point(0, 0);
			for (uint i = 0; i < input.length; i++)
				vk_x = Pairing.add(vk_x, Pairing.mul(vk.IC[i + 1], input[i]));
			vk_x = Pairing.add(vk_x, vk.IC[0]);
			if (!Pairing.pairingProd2(proof.A, vk.A, Pairing.negate(proof.A_p), Pairing.P2())) return 1;
			if (!Pairing.pairingProd2(vk.B, proof.B, Pairing.negate(proof.B_p), Pairing.P2())) return 2;
			if (!Pairing.pairingProd2(proof.C, vk.C, Pairing.negate(proof.C_p), Pairing.P2())) return 3;
			if (!Pairing.pairingProd3(
				proof.K, vk.gamma,
				Pairing.negate(Pairing.add(vk_x, Pairing.add(proof.A, proof.C))), vk.gammaBeta2,
				Pairing.negate(vk.gammaBeta1), proof.B
			)) return 4;
			if (!Pairing.pairingProd3(
					Pairing.add(vk_x, proof.A), proof.B,
					Pairing.negate(proof.H), vk.Z,
					Pairing.negate(proof.C), Pairing.P2()
			)) return 5;
			return 0;
		}
		event Verified(string);
		function verifyTx() public returns (bool) {
			uint[] memory input = new uint[](9);
			Proof memory proof;
			proof.A = Pairing.G1Point(12873740738727497448187997291915224677121726020054032516825496230827252793177, 21804419174137094775122804775419507726154084057848719988004616848382402162497);
			proof.A_p = Pairing.G1Point(7742452358972543465462254569134860944739929848367563713587808717088650354556, 7324522103398787664095385319014038380128814213034709026832529060148225837366);
			proof.B = Pairing.G2Point(
				[8176651290984905087450403379100573157708110416512446269839297438960217797614, 15588556568726919713003060429893850972163943674590384915350025440408631945055],
				[15347511022514187557142999444367533883366476794364262773195059233657571533367, 4265071979090628150845437155927259896060451682253086069461962693761322642015]);
			proof.B_p = Pairing.G1Point(2979746655438963305714517285593753729335852012083057917022078236006592638393, 6470627481646078059765266161088786576504622012540639992486470834383274712950);
			proof.C = Pairing.G1Point(6851077925310461602867742977619883934042581405263014789956638244065803308498, 10336382210592135525880811046708757754106524561907815205241508542912494488506);
			proof.C_p = Pairing.G1Point(12491625890066296859584468664467427202390981822868257437245835716136010795448, 13818492518017455361318553880921248537817650587494176379915981090396574171686);
			proof.H = Pairing.G1Point(12091046215835229523641173286701717671667447745509192321596954139357866668225, 14446807589950902476683545679847436767890904443411534435294953056557941441758);
			proof.K = Pairing.G1Point(21341087976609916409401737322664290631992568431163400450267978471171152600502, 2942165230690572858696920423896381470344658299915828986338281196715687693170);
			input[0] = 13986731495506593864492662381614386532349950841221768152838255933892789078521;
			input[1] = 622860516154313070522697309645122400675542217310916019527100517240519630053;
			input[2] = 11094488463398718754251685950409355128550342438297986977413505294941943071569;
			input[3] = 6627643779954497813586310325594578844876646808666478625705401786271515864467;
			input[4] = 2957286918163151606545409668133310005545945782087581890025685458369200827463;
			input[5] = 1384290496819542862903939282897996566903332587607290986044945365745128311081;
			input[6] = 5613571677741714971687805233468747950848449704454346829971683826953541367271;
			input[7] = 9643208548031422463313148630985736896287522941726746581856185889848792022807;
			input[8] = 18066496933330839731877828156604;
			if (verify(input, proof) == 0) {
				emit Verified("Transaction successfully verified.");
				return true;
			} else {
				return false;
			}
		}

	}
	)";
	compileAndRun(sourceCode, 0, "Pairing");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Pairing", m_contractAddress}});
	// Disabled because the point seems to be not well-formed, we need to find another example.
	//BOOST_CHECK(callContractFunction("testMul()") == encodeArgs(true));
	BOOST_CHECK(callContractFunction("f()") == encodeArgs(true));
	BOOST_CHECK(callContractFunction("g()") == encodeArgs(true));
	BOOST_CHECK(callContractFunction("pair()") == encodeArgs(true));
	BOOST_CHECK(callContractFunction("verifyTx()") == encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(abi_encode)
{
	char const* sourceCode = R"(
		contract C {
			function f0() public returns (bytes memory) {
				return abi.encode();
			}
			function f1() public returns (bytes memory) {
				return abi.encode(1, 2);
			}
			function f2() public returns (bytes memory) {
				string memory x = "abc";
				return abi.encode(1, x, 2);
			}
			function f3() public returns (bytes memory r) {
				// test that memory is properly allocated
				string memory x = "abc";
				r = abi.encode(1, x, 2);
				bytes memory y = "def";
				require(y[0] == "d");
				y[0] = "e";
				require(y[0] == "e");
			}
			function f4() public returns (bytes memory) {
				bytes4 x = "abcd";
				return abi.encode(bytes2(x));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 0));
	ABI_CHECK(callContractFunction("f1()"), encodeArgs(0x20, 0x40, 1, 2));
	ABI_CHECK(callContractFunction("f2()"), encodeArgs(0x20, 0xa0, 1, 0x60, 2, 3, "abc"));
	ABI_CHECK(callContractFunction("f3()"), encodeArgs(0x20, 0xa0, 1, 0x60, 2, 3, "abc"));
	ABI_CHECK(callContractFunction("f4()"), encodeArgs(0x20, 0x20, "ab"));
}

BOOST_AUTO_TEST_CASE(abi_encode_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint a; uint[] b; }
			function f0() public pure returns (bytes memory) {
				return abi.encode();
			}
			function f1() public pure returns (bytes memory) {
				return abi.encode(1, 2);
			}
			function f2() public pure returns (bytes memory) {
				string memory x = "abc";
				return abi.encode(1, x, 2);
			}
			function f3() public pure returns (bytes memory r) {
				// test that memory is properly allocated
				string memory x = "abc";
				r = abi.encode(1, x, 2);
				bytes memory y = "def";
				require(y[0] == "d");
				y[0] = "e";
				require(y[0] == "e");
			}
			S s;
			function f4() public returns (bytes memory r) {
				string memory x = "abc";
				s.a = 7;
				s.b.push(2);
				s.b.push(3);
				r = abi.encode(1, x, s, 2);
				bytes memory y = "def";
				require(y[0] == "d");
				y[0] = "e";
				require(y[0] == "e");
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 0));
	ABI_CHECK(callContractFunction("f1()"), encodeArgs(0x20, 0x40, 1, 2));
	ABI_CHECK(callContractFunction("f2()"), encodeArgs(0x20, 0xa0, 1, 0x60, 2, 3, "abc"));
	ABI_CHECK(callContractFunction("f3()"), encodeArgs(0x20, 0xa0, 1, 0x60, 2, 3, "abc"));
	ABI_CHECK(callContractFunction("f4()"), encodeArgs(0x20, 0x160, 1, 0x80, 0xc0, 2, 3, "abc", 7, 0x40, 2, 2, 3));
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
