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

BOOST_AUTO_TEST_CASE(using_for_overload)
{
	char const* sourceCode = R"(
		library D {
			struct s { uint a; }
			function mul(s storage self, uint x) public returns (uint) { return self.a *= x; }
			function mul(s storage self, bytes32 x) public returns (bytes32) { }
		}
		contract C {
			using D for D.s;
			D.s public x;
			function f(uint a) public returns (uint) {
				x.a = 6;
				return x.mul(a);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"D", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(7)), encodeArgs(u256(6 * 7)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(6 * 7)));
}

BOOST_AUTO_TEST_CASE(using_for_by_name)
{
	char const* sourceCode = R"(
		library D { struct s { uint a; } function mul(s storage self, uint x) public returns (uint) { return self.a *= x; } }
		contract C {
			using D for D.s;
			D.s public x;
			function f(uint a) public returns (uint) {
				x.a = 6;
				return x.mul({x: a});
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"D", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(7)), encodeArgs(u256(6 * 7)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(6 * 7)));
}

BOOST_AUTO_TEST_CASE(bound_function_in_function)
{
	char const* sourceCode = R"(
		library L {
			function g(function() internal returns (uint) _t) internal returns (uint) { return _t(); }
		}
		contract C {
			using L for *;
			function f() public returns (uint) {
				return t.g();
			}
			function t() public pure returns (uint)  { return 7; }
		}
	)";
	compileAndRun(sourceCode, 0, "L");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"L", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(bound_function_in_var)
{
	char const* sourceCode = R"(
		library D { struct s { uint a; } function mul(s storage self, uint x) public returns (uint) { return self.a *= x; } }
		contract C {
			using D for D.s;
			D.s public x;
			function f(uint a) public returns (uint) {
				x.a = 6;
				return (x.mul)({x: a});
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"D", m_contractAddress}});
	ABI_CHECK(callContractFunction("f(uint256)", u256(7)), encodeArgs(u256(6 * 7)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(6 * 7)));
}

BOOST_AUTO_TEST_CASE(bound_function_to_string)
{
	char const* sourceCode = R"(
		library D { function length(string memory self) public returns (uint) { return bytes(self).length; } }
		contract C {
			using D for string;
			string x;
			function f() public returns (uint) {
				x = "abc";
				return x.length();
			}
			function g() public returns (uint) {
				string memory s = "abc";
				return s.length();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"D", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(3)));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(3)));
}

BOOST_AUTO_TEST_CASE(inline_array_storage_to_memory_conversion_strings)
{
	char const* sourceCode = R"(
		contract C {
			string s = "doh";
			function f() public returns (string memory, string memory) {
				string memory t = "ray";
				string[3] memory x = [s, t, "mi"];
				return (x[1], x[2]);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(0x40), u256(0x80), u256(3), string("ray"), u256(2), string("mi")));
}

BOOST_AUTO_TEST_CASE(inline_array_strings_from_document)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint i) public returns (string memory) {
				string[4] memory x = ["This", "is", "an", "array"];
				return (x[i]);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256)", u256(0)), encodeArgs(u256(0x20), u256(4), string("This")));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs(u256(0x20), u256(2), string("is")));
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(0x20), u256(2), string("an")));
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(u256(0x20), u256(5), string("array")));
}

BOOST_AUTO_TEST_CASE(inline_array_storage_to_memory_conversion_ints)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint x, uint y) {
				x = 3;
				y = 6;
				uint[2] memory z = [x, y];
				return (z[0], z[1]);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(3, 6));
}

BOOST_AUTO_TEST_CASE(inline_array_index_access_ints)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				return ([1, 2, 3, 4][2]);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(inline_array_index_access_strings)
{
	char const* sourceCode = R"(
		contract C {
			string public tester;
			function f() public returns (string memory) {
				return (["abc", "def", "g"][0]);
			}
			function test() public {
				tester = f();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
	ABI_CHECK(callContractFunction("tester()"), encodeArgs(u256(0x20), u256(3), string("abc")));
}

BOOST_AUTO_TEST_CASE(inline_array_return)
{
	char const* sourceCode = R"(
		contract C {
			uint8[] tester;
			function f() public returns (uint8[5] memory) {
				return ([1,2,3,4,5]);
			}
			function test() public returns (uint8, uint8, uint8, uint8, uint8) {
				tester = f();
				return (tester[0], tester[1], tester[2], tester[3], tester[4]);
			}

		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1, 2, 3, 4, 5));
}

BOOST_AUTO_TEST_CASE(inline_array_singleton)
{
	// This caused a failure since the type was not converted to its mobile type.
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				return [4][0];
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(4)));
}

BOOST_AUTO_TEST_CASE(inline_long_string_return)
{
		char const* sourceCode = R"(
		contract C {
			function f() public returns (string memory) {
				return (["somethingShort", "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678900123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"][1]);
			}
		}
	)";

	string strLong = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678900123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789001234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeDyn(strLong));
}

BOOST_AUTO_TEST_CASE(fixed_bytes_index_access)
{
	char const* sourceCode = R"(
		contract C {
			bytes16[] public data;
			function f(bytes32 x) public returns (byte) {
				return x[2];
			}
			function g(bytes32 x) public returns (uint) {
				data = [x[0], x[1], x[2]];
				data[0] = "12345";
				return uint(uint8(data[0][4]));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(bytes32)", "789"), encodeArgs("9"));
	ABI_CHECK(callContractFunction("g(bytes32)", "789"), encodeArgs(u256(int('5'))));
	ABI_CHECK(callContractFunction("data(uint256)", u256(1)), encodeArgs("8"));
}

BOOST_AUTO_TEST_CASE(fixed_bytes_length_access)
{
	char const* sourceCode = R"(
		contract C {
			byte a;
			function f(bytes32 x) public returns (uint, uint, uint) {
				return (x.length, bytes16(uint128(2)).length, a.length + 7);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(bytes32)", "789"), encodeArgs(u256(32), u256(16), u256(8)));
}

BOOST_AUTO_TEST_CASE(byte_optimization_bug)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint x) public returns (uint a) {
				assembly {
					a := byte(x, 31)
				}
			}
			function g(uint x) public returns (uint a) {
				assembly {
					a := byte(31, x)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("g(uint256)", u256(2)), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_write_to_stack)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint r, bytes32 r2) {
				assembly { r := 7 r2 := "abcdef" }
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7), string("abcdef")));
}

BOOST_AUTO_TEST_CASE(inline_assembly_read_and_write_stack)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint r) {
				for (uint x = 0; x < 10; ++x)
					assembly { r := add(r, x) }
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(45)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_memory_access)
{
	char const* sourceCode = R"(
		contract C {
			function test() public returns (bytes memory) {
				bytes memory x = new bytes(5);
				for (uint i = 0; i < x.length; ++i)
					x[i] = byte(uint8(i + 1));
				assembly { mstore(add(x, 32), "12345678901234567890123456789012") }
				return x;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(0x20), u256(5), string("12345")));
}

BOOST_AUTO_TEST_CASE(inline_assembly_storage_access)
{
	char const* sourceCode = R"(
		contract C {
			uint16 x;
			uint16 public y;
			uint public z;
			function f() public returns (bool) {
				uint off1;
				uint off2;
				assembly {
					sstore(z_slot, 7)
					off1 := z_offset
					off2 := y_offset
				}
				assert(off1 == 0);
				assert(off2 == 2);
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("z()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_storage_access_inside_function)
{
	char const* sourceCode = R"(
		contract C {
			uint16 x;
			uint16 public y;
			uint public z;
			function f() public returns (bool) {
				uint off1;
				uint off2;
				assembly {
					function f() -> o1 {
						sstore(z_slot, 7)
						o1 := y_offset
					}
					off2 := f()
				}
				assert(off2 == 2);
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("z()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_storage_access_via_pointer)
{
	char const* sourceCode = R"(
		contract C {
			struct Data { uint contents; }
			uint public separator;
			Data public a;
			uint public separator2;
			function f() public returns (bool) {
				Data storage x = a;
				uint off;
				assembly {
					sstore(x_slot, 7)
					off := x_offset
				}
				assert(off == 0);
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("separator()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("separator2()"), encodeArgs(u256(0)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_function_call)
{
	char const* sourceCode = R"(
		contract C {
			function f() public {
				assembly {
					function asmfun(a, b, c) -> x, y, z {
						x := a
						y := b
						z := 7
					}
					let a1, b1, c1 := asmfun(1, 2, 3)
					mstore(0x00, a1)
					mstore(0x20, b1)
					mstore(0x40, c1)
					return(0, 0x60)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), u256(2), u256(7)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_function_call_assignment)
{
	char const* sourceCode = R"(
		contract C {
			function f() public {
				assembly {
					let a1, b1, c1
					function asmfun(a, b, c) -> x, y, z {
						x := a
						y := b
						z := 7
					}
					a1, b1, c1 := asmfun(1, 2, 3)
					mstore(0x00, a1)
					mstore(0x20, b1)
					mstore(0x40, c1)
					return(0, 0x60)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), u256(2), u256(7)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_function_call2)
{
	char const* sourceCode = R"(
		contract C {
			function f() public {
				assembly {
					let d := 0x10
					function asmfun(a, b, c) -> x, y, z {
						x := a
						y := b
						z := 7
					}
					let a1, b1, c1 := asmfun(1, 2, 3)
					mstore(0x00, a1)
					mstore(0x20, b1)
					mstore(0x40, c1)
					mstore(0x60, d)
					return(0, 0x80)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), u256(2), u256(7), u256(0x10)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_embedded_function_call)
{
	char const* sourceCode = R"(
		contract C {
			function f() public {
				assembly {
					let d := 0x10
					function asmfun(a, b, c) -> x, y, z {
						x := g(a)
						function g(r) -> s { s := mul(r, r) }
						y := g(b)
						z := 7
					}
					let a1, b1, c1 := asmfun(1, 2, 3)
					mstore(0x00, a1)
					mstore(0x20, b1)
					mstore(0x40, c1)
					mstore(0x60, d)
					return(0, 0x80)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(1), u256(4), u256(7), u256(0x10)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_if)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a) public returns (uint b) {
				assembly {
					if gt(a, 1) { b := 2 }
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", u256(0)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_switch)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a) public returns (uint b) {
				assembly {
					switch a
					case 1 { b := 8 }
					case 2 { b := 9 }
					default { b := 2 }
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", u256(0)), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs(u256(8)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(9)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_recursion)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a) public returns (uint b) {
				assembly {
					function fac(n) -> nf {
						switch n
						case 0 { nf := 1 }
						case 1 { nf := 1 }
						default { nf := mul(n, fac(sub(n, 1))) }
					}
					b := fac(a)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", u256(0)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(u256(6)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(4)), encodeArgs(u256(24)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_for)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint a) public returns (uint b) {
				assembly {
					function fac(n) -> nf {
						nf := 1
						for { let i := n } gt(i, 0) { i := sub(i, 1) } {
							nf := mul(nf, i)
						}
					}
					b := fac(a)
				}
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", u256(0)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(u256(6)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(4)), encodeArgs(u256(24)));
}

BOOST_AUTO_TEST_CASE(inline_assembly_for2)
{
	char const* sourceCode = R"(
		contract C {
			uint st;
			function f(uint a) public returns (uint b, uint c, uint d) {
				st = 0;
				assembly {
					function sideeffect(r) -> x { sstore(0, add(sload(0), r)) x := 1}
					for { let i := a } eq(i, sideeffect(2)) { d := add(d, 3) } {
						b := i
						i := 0
					}
				}
				c = st;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", u256(0)), encodeArgs(u256(0), u256(2), u256(0)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(1)), encodeArgs(u256(1), u256(4), u256(3)));
	ABI_CHECK(callContractFunction("f(uint256)", u256(2)), encodeArgs(u256(0), u256(2), u256(0)));
}

BOOST_AUTO_TEST_CASE(index_access_with_type_conversion)
{
	// Test for a bug where higher order bits cleanup was not done for array index access.
	char const* sourceCode = R"(
			contract C {
				function f(uint x) public returns (uint[256] memory r){
					r[uint8(x)] = 2;
				}
			}
	)";
	compileAndRun(sourceCode, 0, "C");
	// neither of the two should throw due to out-of-bounds access
	BOOST_CHECK(callContractFunction("f(uint256)", u256(0x01)).size() == 256 * 32);
	BOOST_CHECK(callContractFunction("f(uint256)", u256(0x101)).size() == 256 * 32);
}

BOOST_AUTO_TEST_CASE(delete_on_array_of_structs)
{
	// Test for a bug where we did not increment the counter properly while deleting a dynamic array.
	char const* sourceCode = R"(
		contract C {
			struct S { uint x; uint[] y; }
			S[] data;
			function f() public returns (bool) {
				data.length = 2;
				data[0].x = 2**200;
				data[1].x = 2**200;
				delete data;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	// This code interprets x as an array length and thus will go out of gas.
	// neither of the two should throw due to out-of-bounds access
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));

}

BOOST_AUTO_TEST_CASE(internal_library_function)
{
	// tests that internal library functions can be called from outside
	// and retain the same memory context (i.e. are pulled into the caller's code)
	char const* sourceCode = R"(
		library L {
			function f(uint[] memory _data) internal {
				_data[3] = 2;
			}
		}
		contract C {
			function f() public returns (uint) {
				uint[] memory x = new uint[](7);
				x[3] = 8;
				L.f(x);
				return x[3];
			}
		}
	)";
	// This has to work without linking, because everything will be inlined.
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(internal_library_function_calling_private)
{
	// tests that internal library functions that are called from outside and that
	// themselves call private functions are still able to (i.e. the private function
	// also has to be pulled into the caller's code)
	char const* sourceCode = R"(
		library L {
			function g(uint[] memory _data) private {
				_data[3] = 2;
			}
			function f(uint[] memory _data) internal {
				g(_data);
			}
		}
		contract C {
			function f() public returns (uint) {
				uint[] memory x = new uint[](7);
				x[3] = 8;
				L.f(x);
				return x[3];
			}
		}
	)";
	// This has to work without linking, because everything will be inlined.
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(internal_library_function_bound)
{
	char const* sourceCode = R"(
		library L {
			struct S { uint[] data; }
			function f(S memory _s) internal {
				_s.data[3] = 2;
			}
		}
		contract C {
			using L for L.S;
			function f() public returns (uint) {
				L.S memory x;
				x.data = new uint[](7);
				x.data[3] = 8;
				x.f();
				return x.data[3];
			}
		}
	)";
	// This has to work without linking, because everything will be inlined.
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(internal_library_function_return_var_size)
{
	char const* sourceCode = R"(
		library L {
			struct S { uint[] data; }
			function f(S memory _s) internal returns (uint[] memory) {
				_s.data[3] = 2;
				return _s.data;
			}
		}
		contract C {
			using L for L.S;
			function f() public returns (uint) {
				L.S memory x;
				x.data = new uint[](7);
				x.data[3] = 8;
				return x.f()[3];
			}
		}
	)";
	// This has to work without linking, because everything will be inlined.
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(iszero_bnot_correct)
{
	// A long time ago, some opcodes were renamed, which involved the opcodes
	// "iszero" and "not".
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bool) {
				bytes32 x = bytes32(uint256(1));
				assembly { x := not(x) }
				if (x != ~bytes32(uint256(1))) return false;
				assembly { x := iszero(x) }
				if (x != bytes32(0)) return false;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(cleanup_bytes_types)
{
	// Checks that bytesXX types are properly cleaned before they are compared.
	char const* sourceCode = R"(
		contract C {
			function f(bytes2 a, uint16 x) public returns (uint) {
				if (a != "ab") return 1;
				if (x != 0x0102) return 2;
				if (bytes3(uint24(x)) != 0x000102) return 3;
				return 0;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	// We input longer data on purpose.
	bool v2 = dev::test::Options::get().useABIEncoderV2;
	ABI_CHECK(callContractFunction("f(bytes2,uint16)", string("abc"), u256(0x040102)), v2 ? encodeArgs() : encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(cleanup_bytes_types_shortening)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (bytes32 r) {
				bytes4 x = 0xffffffff;
				bytes2 y = bytes2(x);
				assembly { r := y }
				// At this point, r and y both store four bytes, but
				// y is properly cleaned before the equality check
				require(y == bytes2(0xffff));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs("\xff\xff\xff\xff"));
}

BOOST_AUTO_TEST_CASE(cleanup_address_types)
{
	// Checks that address types are properly cleaned before they are compared.
	char const* sourceCode = R"(
		contract C {
			function f(address a) public returns (uint) {
				if (a != 0x1234567890123456789012345678901234567890) return 1;
				return 0;
			}
			function g(address payable a) public returns (uint) {
				if (a != 0x1234567890123456789012345678901234567890) return 1;
				return 0;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");

	bool v2 = dev::test::Options::get().useABIEncoderV2;
	// We input longer data on purpose.
	ABI_CHECK(callContractFunction("f(address)", u256("0xFFFF1234567890123456789012345678901234567890")), v2 ? encodeArgs() : encodeArgs(0));
	ABI_CHECK(callContractFunction("g(address)", u256("0xFFFF1234567890123456789012345678901234567890")), v2 ? encodeArgs() : encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(cleanup_address_types_shortening)
{
	char const* sourceCode = R"(
		contract C {
			function f() public pure returns (address r) {
				bytes21 x = 0x1122334455667788990011223344556677889900ff;
				bytes20 y;
				assembly { y := x }
				address z = address(y);
				assembly { r := z }
				require(z == 0x1122334455667788990011223344556677889900);
			}
			function g() public pure returns (address payable r) {
				bytes21 x = 0x1122334455667788990011223344556677889900ff;
				bytes20 y;
				assembly { y := x }
				address payable z = address(y);
				assembly { r := z }
				require(z == 0x1122334455667788990011223344556677889900);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256("0x1122334455667788990011223344556677889900")));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256("0x1122334455667788990011223344556677889900")));
}

BOOST_AUTO_TEST_CASE(skip_dynamic_types)
{
	// The EVM cannot provide access to dynamically-sized return values, so we have to skip them.
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint, uint[] memory, uint) {
				return (7, new uint[](2), 8);
			}
			function g() public returns (uint, uint) {
				// Previous implementation "moved" b to the second place and did not skip.
				(uint a,, uint b) = this.f();
				return (a, b);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(7), u256(8)));
}

BOOST_AUTO_TEST_CASE(skip_dynamic_types_for_structs)
{
	// For accessors, the dynamic types are already removed in the external signature itself.
	char const* sourceCode = R"(
		contract C {
			struct S {
				uint x;
				string a; // this is present in the accessor
				uint[] b; // this is not present
				uint y;
			}
			S public s;
			function g() public returns (uint, uint) {
				s.x = 2;
				s.a = "abc";
				s.b = [7, 8, 9];
				s.y = 6;
				(uint x,, uint y) = this.s();
				return (x, y);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(u256(2), u256(6)));
}

BOOST_AUTO_TEST_CASE(failed_create)
{
	char const* sourceCode = R"(
		contract D { constructor() public payable {} }
		contract C {
			uint public x;
			constructor() public payable {}
			function f(uint amount) public returns (D) {
				x++;
				return (new D).value(amount)();
			}
			function stack(uint depth) public returns (address) {
				if (depth < 1024)
					return this.stack(depth - 1);
				else
					return address(f(0));
			}
		}
	)";
	compileAndRun(sourceCode, 20, "C");
	BOOST_CHECK(callContractFunction("f(uint256)", 20) != encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("f(uint256)", 20), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("stack(uint256)", 1023), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(create_dynamic_array_with_zero_length)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				uint[][] memory a = new uint[][](0);
				return 7;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(correctly_initialize_memory_array_in_constructor)
{
	// Memory arrays are initialized using codecopy past the size of the code.
	// This test checks that it also works in the constructor context.
	char const* sourceCode = R"(
		contract C {
			bool public success;
			constructor() public {
				// Make memory dirty.
				assembly {
					for { let i := 0 } lt(i, 64) { i := add(i, 1) } {
						mstore(msize, not(0))
					}
				}
				uint16[3] memory c;
				require(c[0] == 0 && c[1] == 0 && c[2] == 0);
				uint16[] memory x = new uint16[](3);
				require(x[0] == 0 && x[1] == 0 && x[2] == 0);
				success = true;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("success()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(return_does_not_skip_modifier)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			modifier setsx {
				_;
				x = 9;
			}
			function f() setsx public returns (uint) {
				return 2;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(9)));
}

BOOST_AUTO_TEST_CASE(break_in_modifier)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			modifier run() {
				for (uint i = 0; i < 10; i++) {
					_;
					break;
				}
			}
			function f() run public {
				uint k = x;
				uint t = k + 1;
				x = t;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(continue_in_modifier)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			modifier run() {
				for (uint i = 0; i < 10; i++) {
					if (i % 2 == 1) continue;
					_;
				}
			}
			function f() run public {
				uint k = x;
				uint t = k + 1;
				x = t;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(5)));
}

BOOST_AUTO_TEST_CASE(return_in_modifier)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			modifier run() {
				for (uint i = 1; i < 10; i++) {
					if (i == 5) return;
					_;
				}
			}
			function f() run public {
				uint k = x;
				uint t = k + 1;
				x = t;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(4)));
}

BOOST_AUTO_TEST_CASE(stacked_return_with_modifiers)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			modifier run() {
				for (uint i = 0; i < 10; i++) {
					_;
					break;
				}
			}
			function f() run public {
				uint k = x;
				uint t = k + 1;
				x = t;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(0)));
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(mutex)
{
	char const* sourceCode = R"(
		contract mutexed {
			bool locked;
			modifier protected {
				if (locked) revert();
				locked = true;
				_;
				locked = false;
			}
		}
		contract Fund is mutexed {
			uint shares;
			constructor() public payable { shares = msg.value; }
			function withdraw(uint amount) public protected returns (uint) {
				// NOTE: It is very bad practice to write this function this way.
				// Please refer to the documentation of how to do this properly.
				if (amount > shares) revert();
				(bool success,) = msg.sender.call.value(amount)("");
				require(success);
				shares -= amount;
				return shares;
			}
			function withdrawUnprotected(uint amount) public returns (uint) {
				// NOTE: It is very bad practice to write this function this way.
				// Please refer to the documentation of how to do this properly.
				if (amount > shares) revert();
				(bool success,) = msg.sender.call.value(amount)("");
				require(success);
				shares -= amount;
				return shares;
			}
		}
		contract Attacker {
			Fund public fund;
			uint callDepth;
			bool protected;
			function setProtected(bool _protected) public { protected = _protected; }
			constructor(Fund _fund) public { fund = _fund; }
			function attack() public returns (uint) {
				callDepth = 0;
				return attackInternal();
			}
			function attackInternal() internal returns (uint) {
				if (protected)
					return fund.withdraw(10);
				else
					return fund.withdrawUnprotected(10);
			}
			function() external payable {
				callDepth++;
				if (callDepth < 4)
					attackInternal();
			}
		}
	)";
	compileAndRun(sourceCode, 500, "Fund");
	auto fund = m_contractAddress;
	BOOST_CHECK_EQUAL(balanceAt(fund), 500);
	compileAndRun(sourceCode, 0, "Attacker", encodeArgs(u160(fund)));
	ABI_CHECK(callContractFunction("setProtected(bool)", true), encodeArgs());
	ABI_CHECK(callContractFunction("attack()"), encodeArgs());
	BOOST_CHECK_EQUAL(balanceAt(fund), 500);
	ABI_CHECK(callContractFunction("setProtected(bool)", false), encodeArgs());
	ABI_CHECK(callContractFunction("attack()"), encodeArgs(u256(460)));
	BOOST_CHECK_EQUAL(balanceAt(fund), 460);
}

BOOST_AUTO_TEST_CASE(calling_nonexisting_contract_throws)
{
	char const* sourceCode = R"YY(
		contract D { function g() public; }
		contract C {
			D d = D(0x1212);
			function f() public returns (uint) {
				d.g();
				return 7;
			}
			function g() public returns (uint) {
				d.g.gas(200)();
				return 7;
			}
			function h() public returns (uint) {
				address(d).call(""); // this does not throw (low-level)
				return 7;
			}
		}
	)YY";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("g()"), encodeArgs());
	ABI_CHECK(callContractFunction("h()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(payable_constructor)
{
	char const* sourceCode = R"(
		contract C {
			constructor() public payable { }
		}
	)";
	compileAndRun(sourceCode, 27, "C");
}

BOOST_AUTO_TEST_CASE(payable_function)
{
	char const* sourceCode = R"(
		contract C {
			uint public a;
			function f() payable public returns (uint) {
				return msg.value;
			}
			function() external payable {
				a = msg.value + 1;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunctionWithValue("f()", 27), encodeArgs(u256(27)));
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 27);
	ABI_CHECK(callContractFunctionWithValue("", 27), encodeArgs());
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 27 + 27);
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(28)));
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 27 + 27);
}

BOOST_AUTO_TEST_CASE(payable_function_calls_library)
{
	char const* sourceCode = R"(
		library L {
			function f() public returns (uint) { return 7; }
		}
		contract C {
			function f() public payable returns (uint) {
				return L.f();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "L");
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"L", m_contractAddress}});
	ABI_CHECK(callContractFunctionWithValue("f()", 27), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(non_payable_throw)
{
	char const* sourceCode = R"(
		contract C {
			uint public a;
			function f() public returns (uint) {
				return msgvalue();
			}
			function msgvalue() internal returns (uint) {
				return msg.value;
			}
			function() external {
				update();
			}
			function update() internal {
				a = msg.value + 1;
			}

		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunctionWithValue("f()", 27), encodeArgs());
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 0);
	ABI_CHECK(callContractFunction(""), encodeArgs());
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunctionWithValue("", 27), encodeArgs());
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 0);
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunctionWithValue("a()", 27), encodeArgs());
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 0);
}

BOOST_AUTO_TEST_CASE(no_nonpayable_circumvention_by_modifier)
{
	char const* sourceCode = R"(
		contract C {
			modifier tryCircumvent {
				if (false) _; // avoid the function, we should still not accept ether
			}
			function f() tryCircumvent public returns (uint) {
				return msgvalue();
			}
			function msgvalue() internal returns (uint) {
				return msg.value;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunctionWithValue("f()", 27), encodeArgs());
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 0);
}

BOOST_AUTO_TEST_CASE(mem_resize_is_not_paid_at_call)
{
	// This tests that memory resize for return values is not paid during the call, which would
	// make the gas calculation overly complex. We access the end of the output area before
	// the call is made.
	// Tests that this also survives the optimizer.
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint[200] memory) {}
		}
		contract D {
			function f(C c) public returns (uint) { c.f(); return 7; }
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	u160 cAddr = m_contractAddress;
	compileAndRun(sourceCode, 0, "D");
	ABI_CHECK(callContractFunction("f(address)", cAddr), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(calling_uninitialized_function)
{
	char const* sourceCode = R"(
		contract C {
			function intern() public returns (uint) {
				function (uint) internal returns (uint) x;
				x(2);
				return 7;
			}
			function extern() public returns (uint) {
				function (uint) external returns (uint) x;
				x(2);
				return 7;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	// This should throw exceptions
	ABI_CHECK(callContractFunction("intern()"), encodeArgs());
	ABI_CHECK(callContractFunction("extern()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(calling_uninitialized_function_in_detail)
{
	char const* sourceCode = R"(
		contract C {
			function() internal returns (uint) x;
			int mutex;
			function t() public returns (uint) {
				if (mutex > 0)
					{ assembly { mstore(0, 7) return(0, 0x20) } }
				mutex = 1;
				// Avoid re-executing this function if we jump somewhere.
				x();
				return 2;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("t()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(calling_uninitialized_function_through_array)
{
	char const* sourceCode = R"(
		contract C {
			int mutex;
			function t() public returns (uint) {
				if (mutex > 0)
					{ assembly { mstore(0, 7) return(0, 0x20) } }
				mutex = 1;
				// Avoid re-executing this function if we jump somewhere.
				function() internal returns (uint)[200] memory x;
				x[0]();
				return 2;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("t()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(pass_function_types_internally)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint x) public returns (uint) {
				return eval(g, x);
			}
			function eval(function(uint) internal returns (uint) x, uint a) internal returns (uint) {
				return x(a);
			}
			function g(uint x) public returns (uint) { return x + 1; }
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", 7), encodeArgs(u256(8)));
}

BOOST_AUTO_TEST_CASE(pass_function_types_externally)
{
	char const* sourceCode = R"(
		contract C {
			function f(uint x) public returns (uint) {
				return this.eval(this.g, x);
			}
			function f2(uint x) public returns (uint) {
				return eval(this.g, x);
			}
			function eval(function(uint) external returns (uint) x, uint a) public returns (uint) {
				return x(a);
			}
			function g(uint x) public returns (uint) { return x + 1; }
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256)", 7), encodeArgs(u256(8)));
	ABI_CHECK(callContractFunction("f2(uint256)", 7), encodeArgs(u256(8)));
}

BOOST_AUTO_TEST_CASE(receive_external_function_type)
{
	char const* sourceCode = R"(
		contract C {
			function g() public returns (uint) { return 7; }
			function f(function() external returns (uint) g) public returns (uint) {
				return g();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction(
		"f(function)",
		m_contractAddress.asBytes() + FixedHash<4>(dev::keccak256("g()")).asBytes() + bytes(32 - 4 - 20, 0)
	), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(return_external_function_type)
{
	char const* sourceCode = R"(
		contract C {
			function g() public {}
			function f() public returns (function() external) {
				return this.g;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(
		callContractFunction("f()"),
		m_contractAddress.asBytes() + FixedHash<4>(dev::keccak256("g()")).asBytes() + bytes(32 - 4 - 20, 0)
	);
}

BOOST_AUTO_TEST_CASE(store_function)
{
	char const* sourceCode = R"(
		contract Other {
			function addTwo(uint x) public returns (uint) { return x + 2; }
		}
		contract C {
			function (function (uint) external returns (uint)) internal returns (uint) ev;
			function (uint) external returns (uint) x;
			function store(function(uint) external returns (uint) y) public {
				x = y;
			}
			function eval(function(uint) external returns (uint) y) public returns (uint) {
				return y(7);
			}
			function t() public returns (uint) {
				ev = eval;
				this.store((new Other()).addTwo);
				return ev(x);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("t()"), encodeArgs(u256(9)));
}

BOOST_AUTO_TEST_CASE(store_function_in_constructor)
{
	char const* sourceCode = R"(
		contract C {
			uint public result_in_constructor;
			function (uint) internal returns (uint) x;
			constructor() public {
				x = double;
				result_in_constructor = use(2);
			}
			function double(uint _arg) public returns (uint _ret) {
				_ret = _arg * 2;
			}
			function use(uint _arg) public returns (uint) {
				return x(_arg);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("use(uint256)", encodeArgs(u256(3))), encodeArgs(u256(6)));
	ABI_CHECK(callContractFunction("result_in_constructor()"), encodeArgs(u256(4)));
}

// TODO: store bound internal library functions

BOOST_AUTO_TEST_CASE(store_internal_unused_function_in_constructor)
{
	char const* sourceCode = R"(
		contract C {
			function () internal returns (uint) x;
			constructor() public {
				x = unused;
			}
			function unused() internal returns (uint) {
				return 7;
			}
			function t() public returns (uint) {
				return x();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("t()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(store_internal_unused_library_function_in_constructor)
{
	char const* sourceCode = R"(
		library L { function x() internal returns (uint) { return 7; } }
		contract C {
			function () internal returns (uint) x;
			constructor() public {
				x = L.x;
			}
			function t() public returns (uint) {
				return x();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("t()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(same_function_in_construction_and_runtime)
{
	char const* sourceCode = R"(
		contract C {
			uint public initial;
			constructor() public {
				initial = double(2);
			}
			function double(uint _arg) public returns (uint _ret) {
				_ret = _arg * 2;
			}
			function runtime(uint _arg) public returns (uint) {
				return double(_arg);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("runtime(uint256)", encodeArgs(u256(3))), encodeArgs(u256(6)));
	ABI_CHECK(callContractFunction("initial()"), encodeArgs(u256(4)));
}

BOOST_AUTO_TEST_CASE(same_function_in_construction_and_runtime_equality_check)
{
	char const* sourceCode = R"(
		contract C {
			function (uint) internal returns (uint) x;
			constructor() public {
				x = double;
			}
			function test() public returns (bool) {
				return x == double;
			}
			function double(uint _arg) public returns (uint _ret) {
				_ret = _arg * 2;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(function_type_library_internal)
{
	char const* sourceCode = R"(
		library Utils {
			function reduce(uint[] memory array, function(uint, uint) internal returns (uint) f, uint init) internal returns (uint) {
				for (uint i = 0; i < array.length; i++) {
					init = f(array[i], init);
				}
				return init;
			}
			function sum(uint a, uint b) internal returns (uint) {
				return a + b;
			}
		}
		contract C {
			function f(uint[] memory x) public returns (uint) {
				return Utils.reduce(x, Utils.sum, 0);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f(uint256[])", 0x20, 3, u256(1), u256(7), u256(3)), encodeArgs(u256(11)));
}


BOOST_AUTO_TEST_CASE(call_function_returning_function)
{
	char const* sourceCode = R"(
		contract test {
			function f0() public returns (uint) {
				return 2;
			}
			function f1() internal returns (function() internal returns (uint)) {
				return f0;
			}
			function f2() internal returns (function() internal returns (function () internal returns (uint))) {
				return f1;
			}
			function f3() internal returns (function() internal returns (function () internal returns (function () internal returns (uint))))
			{
				return f2;
			}
			function f() public returns (uint) {
				function() internal returns(function() internal returns(function() internal returns(function() internal returns(uint)))) x;
				x = f3;
				return x()()()();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "test");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(mapping_of_functions)
{
	char const* sourceCode = R"(
		contract Flow {
			bool public success;

			mapping (address => function () internal) stages;

			function stage0() internal {
				stages[msg.sender] = stage1;
			}

			function stage1() internal {
				stages[msg.sender] = stage2;
			}

			function stage2() internal {
				success = true;
			}

			constructor() public {
				stages[msg.sender] = stage0;
			}

			function f() public returns (uint) {
				stages[msg.sender]();
				return 7;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "Flow");
	ABI_CHECK(callContractFunction("success()"), encodeArgs(false));
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("success()"), encodeArgs(false));
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("success()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(packed_functions)
{
	char const* sourceCode = R"(
		contract C {
			// these should take the same slot
			function() internal returns (uint) a;
			function() external returns (uint) b;
			function() external returns (uint) c;
			function() internal returns (uint) d;
			uint8 public x;

			function set() public {
				x = 2;
				d = g;
				c = this.h;
				b = this.h;
				a = g;
			}
			function t1() public returns (uint) {
				return a();
			}
			function t2() public returns (uint) {
				return b();
			}
			function t3() public returns (uint) {
				return a();
			}
			function t4() public returns (uint) {
				return b();
			}
			function g() public returns (uint) {
				return 7;
			}
			function h() public returns (uint) {
				return 8;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("set()"), encodeArgs());
	ABI_CHECK(callContractFunction("t1()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("t2()"), encodeArgs(u256(8)));
	ABI_CHECK(callContractFunction("t3()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("t4()"), encodeArgs(u256(8)));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(u256(2)));
}

BOOST_AUTO_TEST_CASE(function_memory_array)
{
	char const* sourceCode = R"(
		contract C {
			function a(uint x) public returns (uint) { return x + 1; }
			function b(uint x) public returns (uint) { return x + 2; }
			function c(uint x) public returns (uint) { return x + 3; }
			function d(uint x) public returns (uint) { return x + 5; }
			function e(uint x) public returns (uint) { return x + 8; }
			function test(uint x, uint i) public returns (uint) {
				function(uint) internal returns (uint)[] memory arr =
					new function(uint) internal returns (uint)[](10);
				arr[0] = a;
				arr[1] = b;
				arr[2] = c;
				arr[3] = d;
				arr[4] = e;
				return arr[i](x);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test(uint256,uint256)", u256(10), u256(0)), encodeArgs(u256(11)));
	ABI_CHECK(callContractFunction("test(uint256,uint256)", u256(10), u256(1)), encodeArgs(u256(12)));
	ABI_CHECK(callContractFunction("test(uint256,uint256)", u256(10), u256(2)), encodeArgs(u256(13)));
	ABI_CHECK(callContractFunction("test(uint256,uint256)", u256(10), u256(3)), encodeArgs(u256(15)));
	ABI_CHECK(callContractFunction("test(uint256,uint256)", u256(10), u256(4)), encodeArgs(u256(18)));
	ABI_CHECK(callContractFunction("test(uint256,uint256)", u256(10), u256(5)), encodeArgs());
}

BOOST_AUTO_TEST_CASE(function_delete_storage)
{
	char const* sourceCode = R"(
		contract C {
			function a() public returns (uint) { return 7; }
			function() internal returns (uint) y;
			function set() public returns (uint) {
				y = a;
				return y();
			}
			function d() public returns (uint) {
				delete y;
				return 1;
			}
			function ca() public returns (uint) {
				return y();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("set()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("ca()"), encodeArgs(u256(7)));
	ABI_CHECK(callContractFunction("d()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("ca()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(function_delete_stack)
{
	char const* sourceCode = R"(
		contract C {
			function a() public returns (uint) { return 7; }
			function test() public returns (uint) {
				function () returns (uint) y = a;
				delete y;
				y();
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(copy_function_storage_array)
{
	char const* sourceCode = R"(
		contract C {
			function() internal returns (uint)[] x;
			function() internal returns (uint)[] y;
			function test() public returns (uint) {
				x.length = 10;
				x[9] = a;
				y = x;
				return y[9]();
			}
			function a() public returns (uint) {
				return 7;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(7)));
}

BOOST_AUTO_TEST_CASE(function_array_cross_calls)
{
	char const* sourceCode = R"(
		contract D {
			function f(function() external returns (function() external returns (uint))[] memory x)
				public returns (function() external returns (uint)[3] memory r)
			{
				r[0] = x[0]();
				r[1] = x[1]();
				r[2] = x[2]();
			}
		}
		contract C {
			function test() public returns (uint, uint, uint) {
				function() external returns (function() external returns (uint))[] memory x =
					new function() external returns (function() external returns (uint))[](10);
				for (uint i = 0; i < x.length; i ++)
					x[i] = this.h;
				x[0] = this.htwo;
				function() external returns (uint)[3] memory y = (new D()).f(x);
				return (y[0](), y[1](), y[2]());
			}
			function e() public returns (uint) { return 5; }
			function f() public returns (uint) { return 6; }
			function g() public returns (uint) { return 7; }
			uint counter;
			function h() public returns (function() external returns (uint)) {
				return counter++ == 0 ? this.f : this.g;
			}
			function htwo() public returns (function() external returns (uint)) {
				return this.e;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(u256(5), u256(6), u256(7)));
}

BOOST_AUTO_TEST_CASE(external_function_to_address)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bool) {
				return address(this.f) == address(this);
			}
			function g(function() external cb) public returns (address) {
				return address(cb);
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g(function)", fromHex("00000000000000000000000000000000000004226121ff00000000000000000")), encodeArgs(u160(0x42)));
}


BOOST_AUTO_TEST_CASE(copy_internal_function_array_to_storage)
{
	char const* sourceCode = R"(
		contract C {
			function() internal returns (uint)[20] x;
			int mutex;
			function one() public returns (uint) {
				function() internal returns (uint)[20] memory xmem;
				x = xmem;
				return 3;
			}
			function two() public returns (uint) {
				if (mutex > 0)
					return 7;
				mutex = 1;
				// If this test fails, it might re-execute this function.
				x[0]();
				return 2;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("one()"), encodeArgs(u256(3)));
	ABI_CHECK(callContractFunction("two()"), encodeArgs());
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
