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

BOOST_AUTO_TEST_CASE(abi_encodePacked)
{
	char const* sourceCode = R"(
		contract C {
			function f0() public pure returns (bytes memory) {
				return abi.encodePacked();
			}
			function f1() public pure returns (bytes memory) {
				return abi.encodePacked(uint8(1), uint8(2));
			}
			function f2() public pure returns (bytes memory) {
				string memory x = "abc";
				return abi.encodePacked(uint8(1), x, uint8(2));
			}
			function f3() public pure returns (bytes memory r) {
				// test that memory is properly allocated
				string memory x = "abc";
				r = abi.encodePacked(uint8(1), x, uint8(2));
				bytes memory y = "def";
				require(y[0] == "d");
				y[0] = "e";
				require(y[0] == "e");
			}
			function f4() public pure returns (bytes memory) {
				string memory x = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz";
				return abi.encodePacked(uint16(0x0701), x, uint16(0x1201));
			}
			function f_literal() public pure returns (bytes memory) {
				return abi.encodePacked(uint8(0x01), "abc", uint8(0x02));
			}
			function f_calldata() public pure returns (bytes memory) {
				return abi.encodePacked(uint8(0x01), msg.data, uint8(0x02));
			}
		}
	)";
	for (auto v2: {false, true})
	{
		compileAndRun(string(v2 ? "pragma experimental ABIEncoderV2;\n" : "") + sourceCode, 0, "C");
		ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 0));
		ABI_CHECK(callContractFunction("f1()"), encodeArgs(0x20, 2, "\x01\x02"));
		ABI_CHECK(callContractFunction("f2()"), encodeArgs(0x20, 5, "\x01" "abc" "\x02"));
		ABI_CHECK(callContractFunction("f3()"), encodeArgs(0x20, 5, "\x01" "abc" "\x02"));
		ABI_CHECK(callContractFunction("f4()"), encodeArgs(
			0x20,
			2 + 26 + 26 + 2,
			"\x07\x01" "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" "\x12\x01"
		));
		ABI_CHECK(callContractFunction("f_literal()"), encodeArgs(0x20, 5, "\x01" "abc" "\x02"));
		ABI_CHECK(callContractFunction("f_calldata()"), encodeArgs(0x20, 6, "\x01" "\xa5\xbf\xa1\xee" "\x02"));
	}
}

BOOST_AUTO_TEST_CASE(abi_encodePacked_from_storage)
{
	char const* sourceCode = R"(
		contract C {
			uint24[9] small_fixed;
			int24[9] small_fixed_signed;
			uint24[] small_dyn;
			uint248[5] large_fixed;
			uint248[] large_dyn;
			bytes bytes_storage;
			function sf() public returns (bytes memory) {
				small_fixed[0] = 0xfffff1;
				small_fixed[2] = 0xfffff2;
				small_fixed[5] = 0xfffff3;
				small_fixed[8] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), small_fixed, uint8(0x02));
			}
			function sd() public returns (bytes memory) {
				small_dyn.length = 9;
				small_dyn[0] = 0xfffff1;
				small_dyn[2] = 0xfffff2;
				small_dyn[5] = 0xfffff3;
				small_dyn[8] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), small_dyn, uint8(0x02));
			}
			function sfs() public returns (bytes memory) {
				small_fixed_signed[0] = -2;
				small_fixed_signed[2] = 0xffff2;
				small_fixed_signed[5] = -200;
				small_fixed_signed[8] = 0xffff4;
				return abi.encodePacked(uint8(0x01), small_fixed_signed, uint8(0x02));
			}
			function lf() public returns (bytes memory) {
				large_fixed[0] = 2**248-1;
				large_fixed[1] = 0xfffff2;
				large_fixed[2] = 2**248-2;
				large_fixed[4] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), large_fixed, uint8(0x02));
			}
			function ld() public returns (bytes memory) {
				large_dyn.length = 5;
				large_dyn[0] = 2**248-1;
				large_dyn[1] = 0xfffff2;
				large_dyn[2] = 2**248-2;
				large_dyn[4] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), large_dyn, uint8(0x02));
			}
			function bytes_short() public returns (bytes memory) {
				bytes_storage = "abcd";
				return abi.encodePacked(uint8(0x01), bytes_storage, uint8(0x02));
			}
			function bytes_long() public returns (bytes memory) {
				bytes_storage = "0123456789012345678901234567890123456789";
				return abi.encodePacked(uint8(0x01), bytes_storage, uint8(0x02));
			}
		}
	)";
	for (auto v2: {false, true})
	{
		compileAndRun(string(v2 ? "pragma experimental ABIEncoderV2;\n" : "") + sourceCode, 0, "C");
		bytes payload = encodeArgs(0xfffff1, 0, 0xfffff2, 0, 0, 0xfffff3, 0, 0, 0xfffff4);
		bytes encoded = encodeArgs(0x20, 0x122, "\x01" + asString(payload) + "\x02");
		ABI_CHECK(callContractFunction("sf()"), encoded);
		ABI_CHECK(callContractFunction("sd()"), encoded);
		ABI_CHECK(callContractFunction("sfs()"), encodeArgs(0x20, 0x122, "\x01" + asString(encodeArgs(
			u256(-2), 0, 0xffff2, 0, 0, u256(-200), 0, 0, 0xffff4
		)) + "\x02"));
		payload = encodeArgs(
			u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"),
			0xfffff2,
			u256("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe"),
			0,
			0xfffff4
		);
		ABI_CHECK(callContractFunction("lf()"), encodeArgs(0x20, 5 * 32 + 2, "\x01" + asString(encodeArgs(payload)) + "\x02"));
		ABI_CHECK(callContractFunction("ld()"), encodeArgs(0x20, 5 * 32 + 2, "\x01" + asString(encodeArgs(payload)) + "\x02"));
		ABI_CHECK(callContractFunction("bytes_short()"), encodeArgs(0x20, 6, "\x01" "abcd\x02"));
		ABI_CHECK(
			callContractFunction("bytes_long()"),
			encodeArgs(0x20, 42, "\x01" "0123456789012345678901234567890123456789\x02")
		);
	}
}

BOOST_AUTO_TEST_CASE(abi_encodePacked_from_memory)
{
	char const* sourceCode = R"(
		contract C {
			function sf() public pure returns (bytes memory) {
				uint24[9] memory small_fixed;
				small_fixed[0] = 0xfffff1;
				small_fixed[2] = 0xfffff2;
				small_fixed[5] = 0xfffff3;
				small_fixed[8] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), small_fixed, uint8(0x02));
			}
			function sd() public pure returns (bytes memory) {
				uint24[] memory small_dyn = new uint24[](9);
				small_dyn[0] = 0xfffff1;
				small_dyn[2] = 0xfffff2;
				small_dyn[5] = 0xfffff3;
				small_dyn[8] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), small_dyn, uint8(0x02));
			}
			function sfs() public pure returns (bytes memory) {
				int24[9] memory small_fixed_signed;
				small_fixed_signed[0] = -2;
				small_fixed_signed[2] = 0xffff2;
				small_fixed_signed[5] = -200;
				small_fixed_signed[8] = 0xffff4;
				return abi.encodePacked(uint8(0x01), small_fixed_signed, uint8(0x02));
			}
			function lf() public pure returns (bytes memory) {
				uint248[5] memory large_fixed;
				large_fixed[0] = 2**248-1;
				large_fixed[1] = 0xfffff2;
				large_fixed[2] = 2**248-2;
				large_fixed[4] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), large_fixed, uint8(0x02));
			}
			function ld() public pure returns (bytes memory) {
				uint248[] memory large_dyn = new uint248[](5);
				large_dyn[0] = 2**248-1;
				large_dyn[1] = 0xfffff2;
				large_dyn[2] = 2**248-2;
				large_dyn[4] = 0xfffff4;
				return abi.encodePacked(uint8(0x01), large_dyn, uint8(0x02));
			}
		}
	)";
	for (auto v2: {false, true})
	{
		compileAndRun(string(v2 ? "pragma experimental ABIEncoderV2;\n" : "") + sourceCode, 0, "C");
		bytes payload = encodeArgs(0xfffff1, 0, 0xfffff2, 0, 0, 0xfffff3, 0, 0, 0xfffff4);
		bytes encoded = encodeArgs(0x20, 0x122, "\x01" + asString(payload) + "\x02");
		ABI_CHECK(callContractFunction("sf()"), encoded);
		ABI_CHECK(callContractFunction("sd()"), encoded);
		ABI_CHECK(callContractFunction("sfs()"), encodeArgs(0x20, 0x122, "\x01" + asString(encodeArgs(
			u256(-2), 0, 0xffff2, 0, 0, u256(-200), 0, 0, 0xffff4
		)) + "\x02"));
		payload = encodeArgs(
			u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"),
			0xfffff2,
			u256("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe"),
			0,
			0xfffff4
		);
		ABI_CHECK(callContractFunction("lf()"), encodeArgs(0x20, 5 * 32 + 2, "\x01" + asString(encodeArgs(payload)) + "\x02"));
		ABI_CHECK(callContractFunction("ld()"), encodeArgs(0x20, 5 * 32 + 2, "\x01" + asString(encodeArgs(payload)) + "\x02"));
	}
}

BOOST_AUTO_TEST_CASE(abi_encodePacked_functionPtr)
{
	char const* sourceCode = R"(
		contract C {
			C other = C(0x1112131400000000000011121314000000000087);
			function testDirect() public view returns (bytes memory) {
				return abi.encodePacked(uint8(8), other.f, uint8(2));
			}
			function testFixedArray() public view returns (bytes memory) {
				function () external pure returns (bytes memory)[1] memory x;
				x[0] = other.f;
				return abi.encodePacked(uint8(8), x, uint8(2));
			}
			function testDynamicArray() public view returns (bytes memory) {
				function () external pure returns (bytes memory)[] memory x = new function() external pure returns (bytes memory)[](1);
				x[0] = other.f;
				return abi.encodePacked(uint8(8), x, uint8(2));
			}
			function f() public pure returns (bytes memory) {}
		}
	)";
	for (auto v2: {false, true})
	{
		compileAndRun(string(v2 ? "pragma experimental ABIEncoderV2;\n" : "") + sourceCode, 0, "C");
		string directEncoding = asString(fromHex("08" "1112131400000000000011121314000000000087" "26121ff0" "02"));
		ABI_CHECK(callContractFunction("testDirect()"), encodeArgs(0x20, directEncoding.size(), directEncoding));
		string arrayEncoding = asString(fromHex("08" "1112131400000000000011121314000000000087" "26121ff0" "0000000000000000" "02"));
		ABI_CHECK(callContractFunction("testFixedArray()"), encodeArgs(0x20, arrayEncoding.size(), arrayEncoding));
		ABI_CHECK(callContractFunction("testDynamicArray()"), encodeArgs(0x20, arrayEncoding.size(), arrayEncoding));
	}
}

BOOST_AUTO_TEST_CASE(abi_encodePackedV2_structs)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S {
				uint8 a;
				int16 b;
				uint8[2] c;
				int16[] d;
			}
			S s;
			event E(S indexed);
			constructor() public {
				s.a = 0x12;
				s.b = -7;
				s.c[0] = 2;
				s.c[1] = 3;
				s.d.length = 2;
				s.d[0] = -7;
				s.d[1] = -8;
			}
			function testStorage() public {
				emit E(s);
			}
			function testMemory() public {
				S memory m = s;
				emit E(m);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	bytes structEnc = encodeArgs(int(0x12), u256(-7), int(2), int(3), u256(-7), u256(-8));
	ABI_CHECK(callContractFunction("testStorage()"), encodeArgs());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E((uint8,int16,uint8[2],int16[]))")));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], dev::keccak256(asString(structEnc)));
	ABI_CHECK(callContractFunction("testMemory()"), encodeArgs());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E((uint8,int16,uint8[2],int16[]))")));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], dev::keccak256(asString(structEnc)));
}

BOOST_AUTO_TEST_CASE(abi_encodePackedV2_nestedArray)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S {
				uint8 a;
				int16 b;
			}
			event E(S[2][][3] indexed);
			function testNestedArrays() public {
				S[2][][3] memory x;
				x[1] = new S[2][](2);
				x[1][0][0].a = 1;
				x[1][0][0].b = 2;
				x[1][0][1].a = 3;
				x[1][1][1].b = 4;
				emit E(x);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	bytes structEnc = encodeArgs(1, 2, 3, 0, 0, 0, 0, 4);
	ABI_CHECK(callContractFunction("testNestedArrays()"), encodeArgs());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E((uint8,int16)[2][][3])")));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], dev::keccak256(asString(structEnc)));
}

BOOST_AUTO_TEST_CASE(abi_encodePackedV2_arrayOfStrings)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			string[] x;
			event E(string[] indexed);
			constructor() public {
				x.length = 2;
				x[0] = "abc";
				x[1] = "0123456789012345678901234567890123456789";
			}
			function testStorage() public {
				emit E(x);
			}
			function testMemory() public {
				string[] memory y = x;
				emit E(y);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	bytes arrayEncoding = encodeArgs("abc", "0123456789012345678901234567890123456789");
	ABI_CHECK(callContractFunction("testStorage()"), encodeArgs());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(string[])")));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], dev::keccak256(asString(arrayEncoding)));
	ABI_CHECK(callContractFunction("testMemory()"), encodeArgs());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(string[])")));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], dev::keccak256(asString(arrayEncoding)));
}

BOOST_AUTO_TEST_CASE(event_signature_in_library)
{
	// This tests a bug that was present where the "internal signature"
	// for structs was also used for events.
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		library L {
			struct S {
				uint8 a;
				int16 b;
			}
			event E(S indexed, S);
			function f() internal {
				S memory s;
				emit E(s, s);
			}
		}
		contract C {
			constructor() public {
				L.f();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E((uint8,int16),(uint8,int16))")));
}


BOOST_AUTO_TEST_CASE(abi_encode_with_selector)
{
	char const* sourceCode = R"(
		contract C {
			function f0() public pure returns (bytes memory) {
				return abi.encodeWithSelector(0x12345678);
			}
			function f1() public pure returns (bytes memory) {
				return abi.encodeWithSelector(0x12345678, "abc");
			}
			function f2() public pure returns (bytes memory) {
				bytes4 x = 0x12345678;
				return abi.encodeWithSelector(x, "abc");
			}
			function f3() public pure returns (bytes memory) {
				bytes4 x = 0x12345678;
				return abi.encodeWithSelector(x, uint(-1));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 4, "\x12\x34\x56\x78"));
	bytes expectation;
	expectation = encodeArgs(0x20, 4 + 0x60) + bytes{0x12, 0x34, 0x56, 0x78} + encodeArgs(0x20, 3, "abc") + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f1()"), expectation);
	expectation = encodeArgs(0x20, 4 + 0x60) + bytes{0x12, 0x34, 0x56, 0x78} + encodeArgs(0x20, 3, "abc") + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f2()"), expectation);
	expectation = encodeArgs(0x20, 4 + 0x20) + bytes{0x12, 0x34, 0x56, 0x78} + encodeArgs(u256(-1)) + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f3()"), expectation);
}

BOOST_AUTO_TEST_CASE(abi_encode_with_selectorv2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f0() public pure returns (bytes memory) {
				return abi.encodeWithSelector(0x12345678);
			}
			function f1() public pure returns (bytes memory) {
				return abi.encodeWithSelector(0x12345678, "abc");
			}
			function f2() public pure returns (bytes memory) {
				bytes4 x = 0x12345678;
				return abi.encodeWithSelector(x, "abc");
			}
			function f3() public pure returns (bytes memory) {
				bytes4 x = 0x12345678;
				return abi.encodeWithSelector(x, uint(-1));
			}
			struct S { uint a; string b; uint16 c; }
			function f4() public pure returns (bytes memory) {
				bytes4 x = 0x12345678;
				S memory s;
				s.a = 0x1234567;
				s.b = "Lorem ipsum dolor sit ethereum........";
				s.c = 0x1234;
				return abi.encodeWithSelector(x, uint(-1), s, uint(3));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 4, "\x12\x34\x56\x78"));
	bytes expectation;
	expectation = encodeArgs(0x20, 4 + 0x60) + bytes{0x12, 0x34, 0x56, 0x78} + encodeArgs(0x20, 3, "abc") + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f1()"), expectation);
	expectation = encodeArgs(0x20, 4 + 0x60) + bytes{0x12, 0x34, 0x56, 0x78} + encodeArgs(0x20, 3, "abc") + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f2()"), expectation);
	expectation = encodeArgs(0x20, 4 + 0x20) + bytes{0x12, 0x34, 0x56, 0x78} + encodeArgs(u256(-1)) + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f3()"), expectation);
	expectation =
		encodeArgs(0x20, 4 + 0x120) +
		bytes{0x12, 0x34, 0x56, 0x78} +
		encodeArgs(u256(-1), 0x60, u256(3), 0x1234567, 0x60, 0x1234, 38, "Lorem ipsum dolor sit ethereum........") +
		bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f4()"), expectation);
}

BOOST_AUTO_TEST_CASE(abi_encode_with_signature)
{
	char const* sourceCode = R"T(
		contract C {
			function f0() public pure returns (bytes memory) {
				return abi.encodeWithSignature("f(uint256)");
			}
			function f1() public pure returns (bytes memory) {
				string memory x = "f(uint256)";
				return abi.encodeWithSignature(x, "abc");
			}
			string xstor;
			function f1s() public returns (bytes memory) {
				xstor = "f(uint256)";
				return abi.encodeWithSignature(xstor, "abc");
			}
			function f2() public pure returns (bytes memory r, uint[] memory ar) {
				string memory x = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";
				uint[] memory y = new uint[](4);
				y[0] = uint(-1);
				y[1] = uint(-2);
				y[2] = uint(-3);
				y[3] = uint(-4);
				r = abi.encodeWithSignature(x, y);
				// The hash uses temporary memory. This allocation re-uses the memory
				// and should initialize it properly.
				ar = new uint[](2);
			}
		}
	)T";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 4, "\xb3\xde\x64\x8b"));
	bytes expectation;
	expectation = encodeArgs(0x20, 4 + 0x60) + bytes{0xb3, 0xde, 0x64, 0x8b} + encodeArgs(0x20, 3, "abc") + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f1()"), expectation);
	ABI_CHECK(callContractFunction("f1s()"), expectation);
	expectation =
		encodeArgs(0x40, 0x140, 4 + 0xc0) +
		(bytes{0xe9, 0xc9, 0x21, 0xcd} + encodeArgs(0x20, 4, u256(-1), u256(-2), u256(-3), u256(-4)) + bytes(0x20 - 4)) +
		encodeArgs(2, 0, 0);
	ABI_CHECK(callContractFunction("f2()"), expectation);
}

BOOST_AUTO_TEST_CASE(abi_encode_with_signaturev2)
{
	char const* sourceCode = R"T(
		pragma experimental ABIEncoderV2;
		contract C {
			function f0() public pure returns (bytes memory) {
				return abi.encodeWithSignature("f(uint256)");
			}
			function f1() public pure returns (bytes memory) {
				string memory x = "f(uint256)";
				return abi.encodeWithSignature(x, "abc");
			}
			string xstor;
			function f1s() public returns (bytes memory) {
				xstor = "f(uint256)";
				return abi.encodeWithSignature(xstor, "abc");
			}
			function f2() public pure returns (bytes memory r, uint[] memory ar) {
				string memory x = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.";
				uint[] memory y = new uint[](4);
				y[0] = uint(-1);
				y[1] = uint(-2);
				y[2] = uint(-3);
				y[3] = uint(-4);
				r = abi.encodeWithSignature(x, y);
				// The hash uses temporary memory. This allocation re-uses the memory
				// and should initialize it properly.
				ar = new uint[](2);
			}
			struct S { uint a; string b; uint16 c; }
			function f4() public pure returns (bytes memory) {
				bytes4 x = 0x12345678;
				S memory s;
				s.a = 0x1234567;
				s.b = "Lorem ipsum dolor sit ethereum........";
				s.c = 0x1234;
				return abi.encodeWithSignature(s.b, uint(-1), s, uint(3));
			}
		}
	)T";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f0()"), encodeArgs(0x20, 4, "\xb3\xde\x64\x8b"));
	bytes expectation;
	expectation = encodeArgs(0x20, 4 + 0x60) + bytes{0xb3, 0xde, 0x64, 0x8b} + encodeArgs(0x20, 3, "abc") + bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f1()"), expectation);
	ABI_CHECK(callContractFunction("f1s()"), expectation);
	expectation =
		encodeArgs(0x40, 0x140, 4 + 0xc0) +
		(bytes{0xe9, 0xc9, 0x21, 0xcd} + encodeArgs(0x20, 4, u256(-1), u256(-2), u256(-3), u256(-4)) + bytes(0x20 - 4)) +
		encodeArgs(2, 0, 0);
	ABI_CHECK(callContractFunction("f2()"), expectation);
	expectation =
		encodeArgs(0x20, 4 + 0x120) +
		bytes{0x7c, 0x79, 0x30, 0x02} +
		encodeArgs(u256(-1), 0x60, u256(3), 0x1234567, 0x60, 0x1234, 38, "Lorem ipsum dolor sit ethereum........") +
		bytes(0x20 - 4);
	ABI_CHECK(callContractFunction("f4()"), expectation);
}

BOOST_AUTO_TEST_CASE(abi_encode_empty_string)
{
	char const* sourceCode = R"(
		// Tests that this will not end up using a "bytes0" type
		// (which would assert)
		contract C {
			function f() public pure returns (bytes memory, bytes memory) {
				return (abi.encode(""), abi.encodePacked(""));
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	if (!dev::test::Options::get().useABIEncoderV2)
	{
		// ABI Encoder V2 has slightly different padding, tested below.
		ABI_CHECK(callContractFunction("f()"), encodeArgs(
			0x40, 0xc0,
			0x60, 0x20, 0x00, 0x00,
			0x00
		));
	}
}

BOOST_AUTO_TEST_CASE(abi_encode_empty_string_v2)
{
	char const* sourceCode = R"(
		// Tests that this will not end up using a "bytes0" type
		// (which would assert)
		pragma experimental ABIEncoderV2;
		contract C {
			function f() public pure returns (bytes memory, bytes memory) {
				return (abi.encode(""), abi.encodePacked(""));
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(
		0x40, 0xa0,
		0x40, 0x20, 0x00,
		0x00
	));
}

BOOST_AUTO_TEST_CASE(abi_encode_rational)
{
	char const* sourceCode = R"(
		// Tests that rational numbers (even negative ones) are encoded properly.
		contract C {
			function f() public pure returns (bytes memory) {
				return abi.encode(1, -2);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(
		0x20,
		0x40, u256(1), u256(-2)
	));
}

BOOST_AUTO_TEST_CASE(abi_encode_rational_v2)
{
	char const* sourceCode = R"(
		// Tests that rational numbers (even negative ones) are encoded properly.
		pragma experimental ABIEncoderV2;
		contract C {
			function f() public pure returns (bytes memory) {
				return abi.encode(1, -2);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(
		0x20,
		0x40, u256(1), u256(-2)
	));
}

BOOST_AUTO_TEST_CASE(abi_encode_call)
{
	char const* sourceCode = R"T(
		contract C {
			bool x;
			function c(uint a, uint[] memory b) public {
				require(a == 5);
				require(b.length == 2);
				require(b[0] == 6);
				require(b[1] == 7);
				x = true;
			}
			function f() public returns (bool) {
				uint a = 5;
				uint[] memory b = new uint[](2);
				b[0] = 6;
				b[1] = 7;
				(bool success,) = address(this).call(abi.encodeWithSignature("c(uint256,uint256[])", a, b));
				require(success);
				return x;
			}
		}
	)T";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(staticcall_for_view_and_pure)
{
	char const* sourceCode = R"(
		contract C {
			uint x;
			function f() public returns (uint) {
				x = 3;
				return 1;
			}
		}
		interface CView {
			function f() view external returns (uint);
		}
		interface CPure {
			function f() pure external returns (uint);
		}
		contract D {
			function f() public returns (uint) {
				return (new C()).f();
			}
			function fview() public returns (uint) {
				return (CView(address(new C()))).f();
			}
			function fpure() public returns (uint) {
				return (CPure(address(new C()))).f();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	// This should work (called via CALL)
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1));
	if (dev::test::Options::get().evmVersion().hasStaticCall())
	{
		// These should throw (called via STATICCALL)
		ABI_CHECK(callContractFunction("fview()"), encodeArgs());
		ABI_CHECK(callContractFunction("fpure()"), encodeArgs());
	}
	else
	{
		ABI_CHECK(callContractFunction("fview()"), encodeArgs(1));
		ABI_CHECK(callContractFunction("fpure()"), encodeArgs(1));
	}
}

BOOST_AUTO_TEST_CASE(bitwise_shifting_constantinople)
{
	if (!dev::test::Options::get().evmVersion().hasBitwiseShifting())
		return;
	char const* sourceCode = R"(
		contract C {
			function shl(uint a, uint b) public returns (uint c) {
				assembly {
					c := shl(b, a)
				}
			}
			function shr(uint a, uint b) public returns (uint c) {
				assembly {
					c := shr(b, a)
				}
			}
			function sar(uint a, uint b) public returns (uint c) {
				assembly {
					c := sar(b, a)
				}
			}
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(callContractFunction("shl(uint256,uint256)", u256(1), u256(2)) == encodeArgs(u256(4)));
	BOOST_CHECK(callContractFunction("shl(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(1)) == encodeArgs(u256("0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe")));
	BOOST_CHECK(callContractFunction("shl(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(256)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr(uint256,uint256)", u256(3), u256(1)) == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shr(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(1)) == encodeArgs(u256("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("shr(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(255)) == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shr(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(256)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar(uint256,uint256)", u256(3), u256(1)) == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("sar(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(1)) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("sar(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(255)) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("sar(uint256,uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"), u256(256)) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
}

BOOST_AUTO_TEST_CASE(bitwise_shifting_constants_constantinople)
{
	if (!dev::test::Options::get().evmVersion().hasBitwiseShifting())
		return;
	char const* sourceCode = R"(
		contract C {
			function shl_1() public returns (bool) {
				uint c;
				assembly {
					c := shl(2, 1)
				}
				assert(c == 4);
				return true;
			}
			function shl_2() public returns (bool) {
				uint c;
				assembly {
					c := shl(1, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
				}
				assert(c == 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe);
				return true;
			}
			function shl_3() public returns (bool) {
				uint c;
				assembly {
					c := shl(256, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
				}
				assert(c == 0);
				return true;
			}
			function shr_1() public returns (bool) {
				uint c;
				assembly {
					c := shr(1, 3)
				}
				assert(c == 1);
				return true;
			}
			function shr_2() public returns (bool) {
				uint c;
				assembly {
					c := shr(1, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
				}
				assert(c == 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);
				return true;
			}
			function shr_3() public returns (bool) {
				uint c;
				assembly {
					c := shr(256, 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff)
				}
				assert(c == 0);
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(callContractFunction("shl_1()") == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shl_2()") == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shl_3()") == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shr_1()") == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shr_2()") == encodeArgs(u256(1)));
	BOOST_CHECK(callContractFunction("shr_3()") == encodeArgs(u256(1)));
}

BOOST_AUTO_TEST_CASE(bitwise_shifting_constantinople_combined)
{
	if (!dev::test::Options::get().evmVersion().hasBitwiseShifting())
		return;
	char const* sourceCode = R"(
		contract C {
			function shl_zero(uint a) public returns (uint c) {
				assembly {
					c := shl(0, a)
				}
			}
			function shr_zero(uint a) public returns (uint c) {
				assembly {
					c := shr(0, a)
				}
			}
			function sar_zero(uint a) public returns (uint c) {
				assembly {
					c := sar(0, a)
				}
			}

			function shl_large(uint a) public returns (uint c) {
				assembly {
					c := shl(0x110, a)
				}
			}
			function shr_large(uint a) public returns (uint c) {
				assembly {
					c := shr(0x110, a)
				}
			}
			function sar_large(uint a) public returns (uint c) {
				assembly {
					c := sar(0x110, a)
				}
			}

			function shl_combined(uint a) public returns (uint c) {
				assembly {
					c := shl(4, shl(12, a))
				}
			}
			function shr_combined(uint a) public returns (uint c) {
				assembly {
					c := shr(4, shr(12, a))
				}
			}
			function sar_combined(uint a) public returns (uint c) {
				assembly {
					c := sar(4, sar(12, a))
				}
			}

			function shl_combined_large(uint a) public returns (uint c) {
				assembly {
					c := shl(0xd0, shl(0x40, a))
				}
			}
			function shl_combined_overflow(uint a) public returns (uint c) {
				assembly {
					c := shl(0x01, shl(not(0x00), a))
				}
			}
			function shr_combined_large(uint a) public returns (uint c) {
				assembly {
					c := shr(0xd0, shr(0x40, a))
				}
			}
			function shr_combined_overflow(uint a) public returns (uint c) {
				assembly {
					c := shr(0x01, shr(not(0x00), a))
				}
			}
			function sar_combined_large(uint a) public returns (uint c) {
				assembly {
					c := sar(0xd0, sar(0x40, a))
				}
			}
		}
	)";
	compileAndRun(sourceCode);

	BOOST_CHECK(callContractFunction("shl_zero(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_zero(uint256)", u256("0xffff")) == encodeArgs(u256("0xffff")));
	BOOST_CHECK(callContractFunction("shl_zero(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("shr_zero(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_zero(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("sar_zero(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_zero(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));

	BOOST_CHECK(callContractFunction("shl_large(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_large(uint256)", u256("0xffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_large(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_large(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_large(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_large(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_large(uint256)", u256("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_large(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));

	BOOST_CHECK(callContractFunction("shl_combined(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_combined(uint256)", u256("0xffff")) == encodeArgs(u256("0xffff0000")));
	BOOST_CHECK(callContractFunction("shl_combined(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000")));
	BOOST_CHECK(callContractFunction("shr_combined(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_combined(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0x0000ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("sar_combined(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_combined(uint256)", u256("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0x00007fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
	BOOST_CHECK(callContractFunction("sar_combined(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));

	BOOST_CHECK(callContractFunction("shl_combined_large(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_combined_large(uint256)", u256("0xffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_combined_large(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shl_combined_overflow(uint256)", u256(2)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_combined_large(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_combined_large(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("shr_combined_overflow(uint256)", u256(2)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_combined_large(uint256)", u256(0)) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_combined_large(uint256)", u256("0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256(0)));
	BOOST_CHECK(callContractFunction("sar_combined_large(uint256)", u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")) == encodeArgs(u256("0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")));
}

BOOST_AUTO_TEST_CASE(senders_balance)
{
	char const* sourceCode = R"(
		contract C {
			function f() public view returns (uint) {
				return msg.sender.balance;
			}
		}
		contract D {
			C c = new C();
			constructor() public payable { }
			function f() public view returns (uint) {
				return c.f();
			}
		}
	)";
	compileAndRun(sourceCode, 27, "D");
	BOOST_CHECK(callContractFunction("f()") == encodeArgs(u256(27)));
}

BOOST_AUTO_TEST_CASE(abi_decode_trivial)
{
	char const* sourceCode = R"(
		contract C {
			function f(bytes memory data) public pure returns (uint) {
				return abi.decode(data, (uint));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bytes)", 0x20, 0x20, 33), encodeArgs(u256(33)));
}

BOOST_AUTO_TEST_CASE(abi_encode_decode_simple)
{
	char const* sourceCode = R"XX(
		contract C {
			function f() public pure returns (uint, bytes memory) {
				bytes memory arg = "abcdefg";
				return abi.decode(abi.encode(uint(33), arg), (uint, bytes));
			}
		}
	)XX";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f()"),
		encodeArgs(33, 0x40, 7, "abcdefg")
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_simple)
{
	char const* sourceCode = R"(
		contract C {
			function f(bytes memory data) public pure returns (uint, bytes memory) {
				return abi.decode(data, (uint, bytes));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 0x20 * 4, 33, 0x40, 7, "abcdefg"),
		encodeArgs(33, 0x40, 7, "abcdefg")
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint a; uint[] b; }
			function f() public pure returns (S memory) {
				S memory s;
				s.a = 8;
				s.b = new uint[](3);
				s.b[0] = 9;
				s.b[1] = 10;
				s.b[2] = 11;
				return abi.decode(abi.encode(s), (S));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f()"),
		encodeArgs(0x20, 8, 0x40, 3, 9, 10, 11)
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_simple_storage)
{
	char const* sourceCode = R"(
		contract C {
			bytes data;
			function f(bytes memory _data) public returns (uint, bytes memory) {
				data = _data;
				return abi.decode(data, (uint, bytes));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 0x20 * 4, 33, 0x40, 7, "abcdefg"),
		encodeArgs(33, 0x40, 7, "abcdefg")
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_v2_storage)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			bytes data;
			struct S { uint a; uint[] b; }
			function f() public returns (S memory) {
				S memory s;
				s.a = 8;
				s.b = new uint[](3);
				s.b[0] = 9;
				s.b[1] = 10;
				s.b[2] = 11;
				data = abi.encode(s);
				return abi.decode(data, (S));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f()"),
		encodeArgs(0x20, 8, 0x40, 3, 9, 10, 11)
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_calldata)
{
	char const* sourceCode = R"(
		contract C {
			function f(bytes calldata data) external pure returns (uint, bytes memory r) {
				return abi.decode(data, (uint, bytes));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 0x20 * 4, 33, 0x40, 7, "abcdefg"),
		encodeArgs(33, 0x40, 7, "abcdefg")
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_v2_calldata)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint a; uint[] b; }
			function f(bytes calldata data) external pure returns (S memory) {
				return abi.decode(data, (S));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 0x20 * 7, 0x20, 33, 0x40, 3, 10, 11, 12),
		encodeArgs(0x20, 33, 0x40, 3, 10, 11, 12)
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_static_array)
{
	char const* sourceCode = R"(
		contract C {
			function f(bytes calldata data) external pure returns (uint[2][3] memory) {
				return abi.decode(data, (uint[2][3]));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 6 * 0x20, 1, 2, 3, 4, 5, 6),
		encodeArgs(1, 2, 3, 4, 5, 6)
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_static_array_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			function f(bytes calldata data) external pure returns (uint[2][3] memory) {
				return abi.decode(data, (uint[2][3]));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 6 * 0x20, 1, 2, 3, 4, 5, 6),
		encodeArgs(1, 2, 3, 4, 5, 6)
	);
}

BOOST_AUTO_TEST_CASE(abi_decode_dynamic_array)
{
	char const* sourceCode = R"(
		contract C {
			function f(bytes calldata data) external pure returns (uint[] memory) {
				return abi.decode(data, (uint[]));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("f(bytes)", 0x20, 6 * 0x20, 0x20, 4, 3, 4, 5, 6),
		encodeArgs(0x20, 4, 3, 4, 5, 6)
	);
}

BOOST_AUTO_TEST_CASE(write_storage_external)
{
	char const* sourceCode = R"(
		contract C {
			uint public x;
			function f(uint y) public payable {
				x = y;
			}
			function g(uint y) external {
				x = y;
			}
			function h() public {
				this.g(12);
			}
		}
		contract D {
			C c = new C();
			function f() public payable returns (uint) {
				c.g(3);
				return c.x();
			}
			function g() public returns (uint) {
				c.g(8);
				return c.x();
			}
			function h() public returns (uint) {
				c.h();
				return c.x();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "D");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(3));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(8));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(12));
}

BOOST_AUTO_TEST_CASE(test_underscore_in_hex)
{
	char const* sourceCode = R"(
		contract test {
			function f(bool cond) public pure returns (uint) {
				uint32 x = 0x1234_ab;
				uint y = 0x1234_abcd_1234;
				return cond ? x : y;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(u256(0x1234ab)));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(u256(0x1234abcd1234)));
}

BOOST_AUTO_TEST_CASE(flipping_sign_tests)
{
	char const* sourceCode = R"(
		contract test {
			function f() public returns (bool){
				int x = -2**255;
				assert(-x == x);
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(external_public_override)
{
	char const* sourceCode = R"(
		contract A {
			function f() external returns (uint) { return 1; }
		}
		contract B is A {
			function f() public returns (uint) { return 2; }
			function g() public returns (uint) { return f(); }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(2));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(base_access_to_function_type_variables)
{
	char const* sourceCode = R"(
		contract C {
			function () internal returns (uint) x;
			function set() public {
				C.x = g;
			}
			function g() public pure returns (uint) { return 2; }
			function h() public returns (uint) { return C.x(); }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("g()"), encodeArgs(2));
	ABI_CHECK(callContractFunction("h()"), encodeArgs());
	ABI_CHECK(callContractFunction("set()"), encodeArgs());
	ABI_CHECK(callContractFunction("h()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(code_access)
{
	char const* sourceCode = R"(
		contract C {
			function lengths() public pure returns (bool) {
				uint crLen = type(D).creationCode.length;
				uint runLen = type(D).runtimeCode.length;
				require(runLen < crLen);
				require(crLen >= 0x20);
				require(runLen >= 0x20);
				return true;
			}
			function creation() public pure returns (bytes memory) {
				return type(D).creationCode;
			}
			function runtime() public pure returns (bytes memory) {
				return type(D).runtimeCode;
			}
			function runtimeAllocCheck() public pure returns (bytes memory) {
				uint[] memory a = new uint[](2);
				bytes memory c = type(D).runtimeCode;
				uint[] memory b = new uint[](2);
				a[0] = 0x1111;
				a[1] = 0x2222;
				b[0] = 0x3333;
				b[1] = 0x4444;
				return c;
			}
		}
		contract D {
			function f() public pure returns (uint) { return 7; }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("lengths()"), encodeArgs(true));
	bytes codeCreation = callContractFunction("creation()");
	bytes codeRuntime1 = callContractFunction("runtime()");
	bytes codeRuntime2 = callContractFunction("runtimeAllocCheck()");
	ABI_CHECK(codeRuntime1, codeRuntime2);
}

BOOST_AUTO_TEST_CASE(code_access_padding)
{
	char const* sourceCode = R"(
		contract C {
			function diff() public pure returns (uint remainder) {
				bytes memory a = type(D).creationCode;
				bytes memory b = type(D).runtimeCode;
				assembly { remainder := mod(sub(b, a), 0x20) }
			}
		}
		contract D {
			function f() public pure returns (uint) { return 7; }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	// This checks that the allocation function pads to multiples of 32 bytes
	ABI_CHECK(callContractFunction("diff()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(code_access_create)
{
	char const* sourceCode = R"(
		contract C {
			function test() public returns (uint) {
				bytes memory c = type(D).creationCode;
				D d;
				assembly {
					d := create(0, add(c, 0x20), mload(c))
				}
				return d.f();
			}
		}
		contract D {
			uint x;
			constructor() public { x = 7; }
			function f() public view returns (uint) { return x; }
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("test()"), encodeArgs(7));
}

BOOST_AUTO_TEST_CASE(code_access_content)
{
	char const* sourceCode = R"(
		contract C {
			function testRuntime() public returns (bool) {
				D d = new D();
				bytes32 runtimeHash = keccak256(type(D).runtimeCode);
				bytes32 otherHash;
				uint size;
				assembly {
					size := extcodesize(d)
					extcodecopy(d, mload(0x40), 0, size)
					otherHash := keccak256(mload(0x40), size)
				}
				require(size == type(D).runtimeCode.length);
				require(runtimeHash == otherHash);
				return true;
			}
			function testCreation() public returns (bool) {
				D d = new D();
				bytes32 creationHash = keccak256(type(D).creationCode);
				require(creationHash == d.x());
				return true;
			}
		}
		contract D {
			bytes32 public x;
			constructor() public {
				bytes32 codeHash;
				assembly {
					let size := codesize()
					codecopy(mload(0x40), 0, size)
					codeHash := keccak256(mload(0x40), size)
				}
				x = codeHash;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C");
	ABI_CHECK(callContractFunction("testRuntime()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("testCreation()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(contract_name)
{
	char const* sourceCode = R"(
		contract C {
			string public nameAccessor = type(C).name;
			string public constant constantNameAccessor = type(C).name;

			function name() public pure returns (string memory) {
				return type(C).name;
			}
		}
		contract D is C {
			function name() public pure returns (string memory) {
				return type(D).name;
			}
			function name2() public pure returns (string memory) {
				return type(C).name;
			}
		}
		contract ThisIsAVeryLongContractNameExceeding256bits {
			string public nameAccessor = type(ThisIsAVeryLongContractNameExceeding256bits).name;
			string public constant constantNameAccessor = type(ThisIsAVeryLongContractNameExceeding256bits).name;

			function name() public pure returns (string memory) {
				return type(ThisIsAVeryLongContractNameExceeding256bits).name;
			}
		}
	)";

	compileAndRun(sourceCode, 0, "C");
	bytes argsC = encodeArgs(u256(0x20), u256(1), "C");
	ABI_CHECK(callContractFunction("name()"), argsC);
	ABI_CHECK(callContractFunction("nameAccessor()"), argsC);
	ABI_CHECK(callContractFunction("constantNameAccessor()"), argsC);

	compileAndRun(sourceCode, 0, "D");
	bytes argsD = encodeArgs(u256(0x20), u256(1), "D");
	ABI_CHECK(callContractFunction("name()"), argsD);
	ABI_CHECK(callContractFunction("name2()"), argsC);

	string longName = "ThisIsAVeryLongContractNameExceeding256bits";
	compileAndRun(sourceCode, 0, longName);
	bytes argsLong = encodeArgs(u256(0x20), u256(longName.length()), longName);
	ABI_CHECK(callContractFunction("name()"), argsLong);
	ABI_CHECK(callContractFunction("nameAccessor()"), argsLong);
	ABI_CHECK(callContractFunction("constantNameAccessor()"), argsLong);
}

BOOST_AUTO_TEST_CASE(event_wrong_abi_name)
{
	char const* sourceCode = R"(
		library ClientReceipt {
			event Deposit(Test indexed _from, bytes32 indexed _id, uint _value);
			function deposit(bytes32 _id) public {
				Test a;
				emit Deposit(a, _id, msg.value);
			}
		}
		contract Test {
			function f() public {
				ClientReceipt.deposit("123");
			}
		}
	)";
	compileAndRun(sourceCode, 0, "ClientReceipt", bytes());
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"ClientReceipt", m_contractAddress}});
	u256 value(18);
	u256 id(0x1234);

	callContractFunction("f()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 3);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,bytes32,uint256)")));
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
