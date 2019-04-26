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

BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_fixed_bytes_same_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToBytes(bytes4 input) public returns (bytes4 ret) {
				return bytes4(input);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("bytesToBytes(bytes4)", "abcd"), encodeArgs("abcd"));
}

// fixed bytes to uint conversion tests
BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_uint_same_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToUint(bytes32 s) public returns (uint256 h) {
				return uint(s);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("bytesToUint(bytes32)", string("abc2")),
		encodeArgs(u256("0x6162633200000000000000000000000000000000000000000000000000000000"))
	);
}

BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_uint_same_min_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToUint(bytes1 s) public returns (uint8 h) {
				return uint8(s);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("bytesToUint(bytes1)", string("a")),
		encodeArgs(u256("0x61"))
	);
}

BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_uint_smaller_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToUint(bytes4 s) public returns (uint16 h) {
				return uint16(uint32(s));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("bytesToUint(bytes4)", string("abcd")),
		encodeArgs(u256("0x6364"))
	);
}

BOOST_AUTO_TEST_CASE(convert_fixed_bytes_to_uint_greater_size)
{
	char const* sourceCode = R"(
		contract Test {
			function bytesToUint(bytes4 s) public returns (uint64 h) {
				return uint64(uint32(s));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("bytesToUint(bytes4)", string("abcd")),
		encodeArgs(u256("0x61626364"))
	);
}

// uint fixed bytes conversion tests
BOOST_AUTO_TEST_CASE(convert_uint_to_fixed_bytes_same_size)
{
	char const* sourceCode = R"(
		contract Test {
			function uintToBytes(uint256 h) public returns (bytes32 s) {
				return bytes32(h);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 a("0x6162630000000000000000000000000000000000000000000000000000000000");
	ABI_CHECK(callContractFunction("uintToBytes(uint256)", a), encodeArgs(a));
}

BOOST_AUTO_TEST_CASE(convert_uint_to_fixed_bytes_same_min_size)
{
	char const* sourceCode = R"(
		contract Test {
			function UintToBytes(uint8 h) public returns (bytes1 s) {
				return bytes1(h);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("UintToBytes(uint8)", u256("0x61")),
		encodeArgs(string("a"))
	);
}

BOOST_AUTO_TEST_CASE(convert_uint_to_fixed_bytes_smaller_size)
{
	char const* sourceCode = R"(
		contract Test {
			function uintToBytes(uint32 h) public returns (bytes2 s) {
				return bytes2(uint16(h));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("uintToBytes(uint32)", u160("0x61626364")),
		encodeArgs(string("cd"))
	);
}

BOOST_AUTO_TEST_CASE(convert_uint_to_fixed_bytes_greater_size)
{
	char const* sourceCode = R"(
		contract Test {
			function UintToBytes(uint16 h) public returns (bytes8 s) {
				return bytes8(uint64(h));
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(
		callContractFunction("UintToBytes(uint16)", u256("0x6162")),
		encodeArgs(string("\0\0\0\0\0\0ab", 8))
	);
}

BOOST_AUTO_TEST_CASE(send_ether)
{
	char const* sourceCode = R"(
		contract test {
			constructor() payable public {}
			function a(address payable addr, uint amount) public returns (uint ret) {
				addr.send(amount);
				return address(this).balance;
			}
		}
	)";
	u256 amount(130);
	compileAndRun(sourceCode, amount + 1);
	u160 address(23);
	ABI_CHECK(callContractFunction("a(address,uint256)", address, amount), encodeArgs(1));
	BOOST_CHECK_EQUAL(balanceAt(address), amount);
}

BOOST_AUTO_TEST_CASE(transfer_ether)
{
	char const* sourceCode = R"(
		contract A {
			constructor() public payable {}
			function a(address payable addr, uint amount) public returns (uint) {
				addr.transfer(amount);
				return address(this).balance;
			}
			function b(address payable addr, uint amount) public {
				addr.transfer(amount);
			}
		}

		contract B {
		}

		contract C {
			function () external payable {
				revert();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "B");
	u160 const nonPayableRecipient = m_contractAddress;
	compileAndRun(sourceCode, 0, "C");
	u160 const oogRecipient = m_contractAddress;
	compileAndRun(sourceCode, 20, "A");
	u160 payableRecipient(23);
	ABI_CHECK(callContractFunction("a(address,uint256)", payableRecipient, 10), encodeArgs(10));
	BOOST_CHECK_EQUAL(balanceAt(payableRecipient), 10);
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 10);
	ABI_CHECK(callContractFunction("b(address,uint256)", nonPayableRecipient, 10), encodeArgs());
	ABI_CHECK(callContractFunction("b(address,uint256)", oogRecipient, 10), encodeArgs());
}

BOOST_AUTO_TEST_CASE(uncalled_blockhash)
{
	char const* code = R"(
		contract C {
			function f() public view returns (bytes32)
			{
				return (blockhash)(block.number - 1);
			}
		}
	)";
	compileAndRun(code, 0, "C");
	bytes result = callContractFunction("f()");
	BOOST_REQUIRE_EQUAL(result.size(), 32);
	BOOST_CHECK(result[0] != 0 || result[1] != 0 || result[2] != 0);
}

BOOST_AUTO_TEST_CASE(blockhash_shadow_resolution)
{
	char const* code = R"(
		contract C {
			function blockhash(uint256 blockNumber) public returns(bytes32) { bytes32 x; return x; }
			function f() public returns(bytes32) { return blockhash(3); }
		}
	)";
	compileAndRun(code, 0, "C");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(log0)
{
	char const* sourceCode = R"(
		contract test {
			function a() public {
				log0(bytes32(uint256(1)));
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("a()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(1)));
	BOOST_CHECK_EQUAL(m_logs[0].topics.size(), 0);
}

BOOST_AUTO_TEST_CASE(log1)
{
	char const* sourceCode = R"(
		contract test {
			function a() public {
				log1(bytes32(uint256(1)), bytes32(uint256(2)));
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("a()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], h256(u256(2)));
}

BOOST_AUTO_TEST_CASE(log2)
{
	char const* sourceCode = R"(
		contract test {
			function a() public {
				log2(bytes32(uint256(1)), bytes32(uint256(2)), bytes32(uint256(3)));
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("a()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 2);
	for (unsigned i = 0; i < 2; ++i)
		BOOST_CHECK_EQUAL(m_logs[0].topics[i], h256(u256(i + 2)));
}

BOOST_AUTO_TEST_CASE(log3)
{
	char const* sourceCode = R"(
		contract test {
			function a() public {
				log3(bytes32(uint256(1)), bytes32(uint256(2)), bytes32(uint256(3)), bytes32(uint256(4)));
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("a()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 3);
	for (unsigned i = 0; i < 3; ++i)
		BOOST_CHECK_EQUAL(m_logs[0].topics[i], h256(u256(i + 2)));
}

BOOST_AUTO_TEST_CASE(log4)
{
	char const* sourceCode = R"(
		contract test {
			function a() public {
				log4(bytes32(uint256(1)), bytes32(uint256(2)), bytes32(uint256(3)), bytes32(uint256(4)), bytes32(uint256(5)));
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("a()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 4);
	for (unsigned i = 0; i < 4; ++i)
		BOOST_CHECK_EQUAL(m_logs[0].topics[i], h256(u256(i + 2)));
}

BOOST_AUTO_TEST_CASE(log_in_constructor)
{
	char const* sourceCode = R"(
		contract test {
			constructor() public {
				log1(bytes32(uint256(1)), bytes32(uint256(2)));
			}
		}
	)";
	compileAndRun(sourceCode);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], h256(u256(2)));
}

BOOST_AUTO_TEST_CASE(selfdestruct)
{
	char const* sourceCode = R"(
		contract test {
			constructor() public payable {}
			function a(address payable receiver) public returns (uint ret) {
				selfdestruct(receiver);
				return 10;
			}
		}
	)";
	u256 amount(130);
	compileAndRun(sourceCode, amount);
	u160 address(23);
	ABI_CHECK(callContractFunction("a(address)", address), bytes());
	BOOST_CHECK(!addressHasCode(m_contractAddress));
	BOOST_CHECK_EQUAL(balanceAt(address), amount);
}

BOOST_AUTO_TEST_CASE(keccak256)
{
	char const* sourceCode = R"(
		contract test {
			function a(bytes32 input) public returns (bytes32 hash) {
				return keccak256(abi.encodePacked(input));
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [&](u256 const& _x) -> u256
	{
		return dev::keccak256(toBigEndian(_x));
	};
	testContractAgainstCpp("a(bytes32)", f, u256(4));
	testContractAgainstCpp("a(bytes32)", f, u256(5));
	testContractAgainstCpp("a(bytes32)", f, u256(-1));
}

BOOST_AUTO_TEST_CASE(sha256)
{
	char const* sourceCode = R"(
		contract test {
			function a(bytes32 input) public returns (bytes32 sha256hash) {
				return sha256(abi.encodePacked(input));
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [&](u256 const& _x) -> bytes
	{
		if (_x == u256(4))
			return fromHex("e38990d0c7fc009880a9c07c23842e886c6bbdc964ce6bdd5817ad357335ee6f");
		if (_x == u256(5))
			return fromHex("96de8fc8c256fa1e1556d41af431cace7dca68707c78dd88c3acab8b17164c47");
		if (_x == u256(-1))
			return fromHex("af9613760f72635fbdb44a5a0a63c39f12af30f950a6ee5c971be188e89c4051");
		return fromHex("");
	};
	testContractAgainstCpp("a(bytes32)", f, u256(4));
	testContractAgainstCpp("a(bytes32)", f, u256(5));
	testContractAgainstCpp("a(bytes32)", f, u256(-1));
}

BOOST_AUTO_TEST_CASE(ripemd)
{
	char const* sourceCode = R"(
		contract test {
			function a(bytes32 input) public returns (bytes32 sha256hash) {
				return ripemd160(abi.encodePacked(input));
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [&](u256 const& _x) -> bytes
	{
		if (_x == u256(4))
			return fromHex("1b0f3c404d12075c68c938f9f60ebea4f74941a0000000000000000000000000");
		if (_x == u256(5))
			return fromHex("ee54aa84fc32d8fed5a5fe160442ae84626829d9000000000000000000000000");
		if (_x == u256(-1))
			return fromHex("1cf4e77f5966e13e109703cd8a0df7ceda7f3dc3000000000000000000000000");
		return fromHex("");
	};
	testContractAgainstCpp("a(bytes32)", f, u256(4));
	testContractAgainstCpp("a(bytes32)", f, u256(5));
	testContractAgainstCpp("a(bytes32)", f, u256(-1));
}

BOOST_AUTO_TEST_CASE(packed_keccak256)
{
	char const* sourceCode = R"(
		contract test {
			function a(bytes32 input) public returns (bytes32 hash) {
				uint24 b = 65536;
				uint c = 256;
				return keccak256(abi.encodePacked(uint8(8), input, b, input, c));
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [&](u256 const& _x) -> u256
	{
		return dev::keccak256(
			toCompactBigEndian(unsigned(8)) +
			toBigEndian(_x) +
			toCompactBigEndian(unsigned(65536)) +
			toBigEndian(_x) +
			toBigEndian(u256(256))
		);
	};
	testContractAgainstCpp("a(bytes32)", f, u256(4));
	testContractAgainstCpp("a(bytes32)", f, u256(5));
	testContractAgainstCpp("a(bytes32)", f, u256(-1));
}

BOOST_AUTO_TEST_CASE(packed_keccak256_complex_types)
{
	char const* sourceCode = R"(
		contract test {
			uint120[3] x;
			function f() public returns (bytes32 hash1, bytes32 hash2, bytes32 hash3) {
				uint120[] memory y = new uint120[](3);
				x[0] = y[0] = uint120(-2);
				x[1] = y[1] = uint120(-3);
				x[2] = y[2] = uint120(-4);
				hash1 = keccak256(abi.encodePacked(x));
				hash2 = keccak256(abi.encodePacked(y));
				hash3 = keccak256(abi.encodePacked(this.f));
			}
		}
	)";
	compileAndRun(sourceCode);
	// Strangely, arrays are encoded with intra-element padding.
	ABI_CHECK(callContractFunction("f()"), encodeArgs(
		dev::keccak256(encodeArgs(u256("0xfffffffffffffffffffffffffffffe"), u256("0xfffffffffffffffffffffffffffffd"), u256("0xfffffffffffffffffffffffffffffc"))),
		dev::keccak256(encodeArgs(u256("0xfffffffffffffffffffffffffffffe"), u256("0xfffffffffffffffffffffffffffffd"), u256("0xfffffffffffffffffffffffffffffc"))),
		dev::keccak256(fromHex(m_contractAddress.hex() + "26121ff0"))
	));
}

BOOST_AUTO_TEST_CASE(packed_sha256)
{
	char const* sourceCode = R"(
		contract test {
			function a(bytes32 input) public returns (bytes32 hash) {
				uint24 b = 65536;
				uint c = 256;
				return sha256(abi.encodePacked(uint8(8), input, b, input, c));
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [&](u256 const& _x) -> bytes
	{
		if (_x == u256(4))
			return fromHex("804e0d7003cfd70fc925dc103174d9f898ebb142ecc2a286da1abd22ac2ce3ac");
		if (_x == u256(5))
			return fromHex("e94921945f9068726c529a290a954f412bcac53184bb41224208a31edbf63cf0");
		if (_x == u256(-1))
			return fromHex("f14def4d07cd185ddd8b10a81b2238326196a38867e6e6adbcc956dc913488c7");
		return fromHex("");
	};
	testContractAgainstCpp("a(bytes32)", f, u256(4));
	testContractAgainstCpp("a(bytes32)", f, u256(5));
	testContractAgainstCpp("a(bytes32)", f, u256(-1));
}

BOOST_AUTO_TEST_CASE(packed_ripemd160)
{
	char const* sourceCode = R"(
		contract test {
			function a(bytes32 input) public returns (bytes32 hash) {
				uint24 b = 65536;
				uint c = 256;
				return ripemd160(abi.encodePacked(uint8(8), input, b, input, c));
			}
		}
	)";
	compileAndRun(sourceCode);
	auto f = [&](u256 const& _x) -> bytes
	{
		if (_x == u256(4))
			return fromHex("f93175303eba2a7b372174fc9330237f5ad202fc000000000000000000000000");
		if (_x == u256(5))
			return fromHex("04f4fc112e2bfbe0d38f896a46629e08e2fcfad5000000000000000000000000");
		if (_x == u256(-1))
			return fromHex("c0a2e4b1f3ff766a9a0089e7a410391730872495000000000000000000000000");
		return fromHex("");
	};
	testContractAgainstCpp("a(bytes32)", f, u256(4));
	testContractAgainstCpp("a(bytes32)", f, u256(5));
	testContractAgainstCpp("a(bytes32)", f, u256(-1));
}

BOOST_AUTO_TEST_CASE(inter_contract_calls)
{
	char const* sourceCode = R"(
		contract Helper {
			function multiply(uint a, uint b) public returns (uint c) {
				return a * b;
			}
		}
		contract Main {
			Helper h;
			function callHelper(uint a, uint b) public returns (uint c) {
				return h.multiply(a, b);
			}
			function getHelper() public returns (address haddress) {
				return address(h);
			}
			function setHelper(address haddress) public {
				h = Helper(haddress);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("setHelper(address)", c_helperAddress) == bytes());
	BOOST_REQUIRE(callContractFunction("getHelper()", c_helperAddress) == encodeArgs(c_helperAddress));
	u256 a(3456789);
	u256 b("0x282837623374623234aa74");
	BOOST_REQUIRE(callContractFunction("callHelper(uint256,uint256)", a, b) == encodeArgs(a * b));
}

BOOST_AUTO_TEST_CASE(inter_contract_calls_with_complex_parameters)
{
	char const* sourceCode = R"(
		contract Helper {
			function sel(uint a, bool select, uint b) public returns (uint c) {
				if (select) return a; else return b;
			}
		}
		contract Main {
			Helper h;
			function callHelper(uint a, bool select, uint b) public returns (uint c) {
				return h.sel(a, select, b) * 3;
			}
			function getHelper() public returns (address haddress) {
				return address(h);
			}
			function setHelper(address haddress) public {
				h = Helper(haddress);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("setHelper(address)", c_helperAddress) == bytes());
	BOOST_REQUIRE(callContractFunction("getHelper()", c_helperAddress) == encodeArgs(c_helperAddress));
	u256 a(3456789);
	u256 b("0x282837623374623234aa74");
	BOOST_REQUIRE(callContractFunction("callHelper(uint256,bool,uint256)", a, true, b) == encodeArgs(a * 3));
	BOOST_REQUIRE(callContractFunction("callHelper(uint256,bool,uint256)", a, false, b) == encodeArgs(b * 3));
}

BOOST_AUTO_TEST_CASE(inter_contract_calls_accessing_this)
{
	char const* sourceCode = R"(
		contract Helper {
			function getAddress() public returns (address addr) {
				return address(this);
			}
		}
		contract Main {
			Helper h;
			function callHelper() public returns (address addr) {
				return h.getAddress();
			}
			function getHelper() public returns (address addr) {
				return address(h);
			}
			function setHelper(address addr) public {
				h = Helper(addr);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("setHelper(address)", c_helperAddress) == bytes());
	BOOST_REQUIRE(callContractFunction("getHelper()", c_helperAddress) == encodeArgs(c_helperAddress));
	BOOST_REQUIRE(callContractFunction("callHelper()") == encodeArgs(c_helperAddress));
}

BOOST_AUTO_TEST_CASE(calls_to_this)
{
	char const* sourceCode = R"(
		contract Helper {
			function invoke(uint a, uint b) public returns (uint c) {
				return this.multiply(a, b, 10);
			}
			function multiply(uint a, uint b, uint8 c) public returns (uint ret) {
				return a * b + c;
			}
		}
		contract Main {
			Helper h;
			function callHelper(uint a, uint b) public returns (uint ret) {
				return h.invoke(a, b);
			}
			function getHelper() public returns (address addr) {
				return address(h);
			}
			function setHelper(address addr) public {
				h = Helper(addr);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("setHelper(address)", c_helperAddress) == bytes());
	BOOST_REQUIRE(callContractFunction("getHelper()", c_helperAddress) == encodeArgs(c_helperAddress));
	u256 a(3456789);
	u256 b("0x282837623374623234aa74");
	BOOST_REQUIRE(callContractFunction("callHelper(uint256,uint256)", a, b) == encodeArgs(a * b + 10));
}

BOOST_AUTO_TEST_CASE(inter_contract_calls_with_local_vars)
{
	// note that a reference to another contract's function occupies two stack slots,
	// so this tests correct stack slot allocation
	char const* sourceCode = R"(
		contract Helper {
			function multiply(uint a, uint b) public returns (uint c) {
				return a * b;
			}
		}
		contract Main {
			Helper h;
			function callHelper(uint a, uint b) public returns (uint c) {
				uint8 y = 9;
				uint256 ret = h.multiply(a, b);
				return ret + y;
			}
			function getHelper() public returns (address haddress) {
				return address(h);
			}
			function setHelper(address haddress) public {
				h = Helper(haddress);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("setHelper(address)", c_helperAddress) == bytes());
	BOOST_REQUIRE(callContractFunction("getHelper()", c_helperAddress) == encodeArgs(c_helperAddress));
	u256 a(3456789);
	u256 b("0x282837623374623234aa74");
	BOOST_REQUIRE(callContractFunction("callHelper(uint256,uint256)", a, b) == encodeArgs(a * b + 9));
}

BOOST_AUTO_TEST_CASE(fixed_bytes_in_calls)
{
	char const* sourceCode = R"(
		contract Helper {
			function invoke(bytes3 x, bool stop) public returns (bytes4 ret) {
				return x;
			}
		}
		contract Main {
			Helper h;
			function callHelper(bytes2 x, bool stop) public returns (bytes5 ret) {
				return h.invoke(x, stop);
			}
			function getHelper() public returns (address addr) {
				return address(h);
			}
			function setHelper(address addr) public {
				h = Helper(addr);
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Helper");
	u160 const c_helperAddress = m_contractAddress;
	compileAndRun(sourceCode, 0, "Main");
	BOOST_REQUIRE(callContractFunction("setHelper(address)", c_helperAddress) == bytes());
	BOOST_REQUIRE(callContractFunction("getHelper()", c_helperAddress) == encodeArgs(c_helperAddress));
	ABI_CHECK(callContractFunction("callHelper(bytes2,bool)", string("\0a", 2), true), encodeArgs(string("\0a\0\0\0", 5)));
}

BOOST_AUTO_TEST_CASE(constructor_arguments_internal)
{
	char const* sourceCode = R"(
		contract Helper {
			bytes3 name;
			bool flag;

			constructor(bytes3 x, bool f) public {
				name = x;
				flag = f;
			}
			function getName() public returns (bytes3 ret) { return name; }
			function getFlag() public returns (bool ret) { return flag; }
		}
		contract Main {
			Helper h;
			constructor() public {
				h = new Helper("abc", true);
			}
			function getFlag() public returns (bool ret) { return h.getFlag(); }
			function getName() public returns (bytes3 ret) { return h.getName(); }
		}
	)";
	compileAndRun(sourceCode, 0, "Main");
	ABI_CHECK(callContractFunction("getFlag()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("getName()"), encodeArgs("abc"));
}

BOOST_AUTO_TEST_CASE(constructor_arguments_external)
{
	char const* sourceCode = R"(
		contract Main {
			bytes3 name;
			bool flag;

			constructor(bytes3 x, bool f) public {
				name = x;
				flag = f;
			}
			function getName() public returns (bytes3 ret) { return name; }
			function getFlag() public returns (bool ret) { return flag; }
		}
	)";
	compileAndRun(sourceCode, 0, "Main", encodeArgs("abc", true));
	ABI_CHECK(callContractFunction("getFlag()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("getName()"), encodeArgs("abc"));
}

BOOST_AUTO_TEST_CASE(constructor_with_long_arguments)
{
	char const* sourceCode = R"(
		contract Main {
			string public a;
			string public b;

			constructor(string memory _a, string memory _b) public {
				a = _a;
				b = _b;
			}
		}
	)";
	string a = "01234567890123gabddunaouhdaoneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi45678907890123456789abcd123456787890123456789abcd90123456789012345678901234567890123456789aboneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi45678907890123456789abcd123456787890123456789abcd90123456789012345678901234567890123456789aboneudapcgadi4567890789012oneudapcgadi4567890789012oneudapcgadi45678907890123456789abcd123456787890123456789abcd90123456789012345678901234567890123456789aboneudapcgadi4567890789012cdef";
	string b = "AUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PYAUTAHIACIANOTUHAOCUHAOEUNAOEHUNTHDYDHPYDRCPYDRSTITOEUBXHUDGO>PY";

	compileAndRun(sourceCode, 0, "Main", encodeArgs(
		u256(0x40),
		u256(0x40 + 0x20 + ((a.length() + 31) / 32) * 32),
		u256(a.length()),
		a,
		u256(b.length()),
		b
	));
	ABI_CHECK(callContractFunction("a()"), encodeDyn(a));
	ABI_CHECK(callContractFunction("b()"), encodeDyn(b));
}

BOOST_AUTO_TEST_CASE(constructor_static_array_argument)
{
	char const* sourceCode = R"(
		contract C {
			uint public a;
			uint[3] public b;

			constructor(uint _a, uint[3] memory _b) public {
				a = _a;
				b = _b;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C", encodeArgs(u256(1), u256(2), u256(3), u256(4)));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("b(uint256)", u256(0)), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("b(uint256)", u256(1)), encodeArgs(u256(3)));
	ABI_CHECK(callContractFunction("b(uint256)", u256(2)), encodeArgs(u256(4)));
}

BOOST_AUTO_TEST_CASE(constant_var_as_array_length)
{
	char const* sourceCode = R"(
		contract C {
			uint constant LEN = 3;
			uint[LEN] public a;

			constructor(uint[LEN] memory _a) public {
				a = _a;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "C", encodeArgs(u256(1), u256(2), u256(3)));
	ABI_CHECK(callContractFunction("a(uint256)", u256(0)), encodeArgs(u256(1)));
	ABI_CHECK(callContractFunction("a(uint256)", u256(1)), encodeArgs(u256(2)));
	ABI_CHECK(callContractFunction("a(uint256)", u256(2)), encodeArgs(u256(3)));
}

BOOST_AUTO_TEST_CASE(functions_called_by_constructor)
{
	char const* sourceCode = R"(
		contract Test {
			bytes3 name;
			bool flag;
			constructor() public {
				setName("abc");
			}
			function getName() public returns (bytes3 ret) { return name; }
			function setName(bytes3 _name) private { name = _name; }
		}
	)";
	compileAndRun(sourceCode);
	BOOST_REQUIRE(callContractFunction("getName()") == encodeArgs("abc"));
}

BOOST_AUTO_TEST_CASE(contracts_as_addresses)
{
	char const* sourceCode = R"(
		contract helper {
			function() external payable { } // can receive ether
		}
		contract test {
			helper h;
			constructor() public payable { h = new helper(); address(h).send(5); }
			function getBalance() public returns (uint256 myBalance, uint256 helperBalance) {
				myBalance = address(this).balance;
				helperBalance = address(h).balance;
			}
		}
	)";
	compileAndRun(sourceCode, 20);
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 20 - 5);
	BOOST_REQUIRE(callContractFunction("getBalance()") == encodeArgs(u256(20 - 5), u256(5)));
}

BOOST_AUTO_TEST_CASE(gas_and_value_basic)
{
	char const* sourceCode = R"(
		contract helper {
			bool flag;
			function getBalance() payable public returns (uint256 myBalance) {
				return address(this).balance;
			}
			function setFlag() public { flag = true; }
			function getFlag() public returns (bool fl) { return flag; }
		}
		contract test {
			helper h;
			constructor() public payable { h = new helper(); }
			function sendAmount(uint amount) public payable returns (uint256 bal) {
				return h.getBalance.value(amount)();
			}
			function outOfGas() public returns (bool ret) {
				h.setFlag.gas(2)(); // should fail due to OOG
				return true;
			}
			function checkState() public returns (bool flagAfter, uint myBal) {
				flagAfter = h.getFlag();
				myBal = address(this).balance;
			}
		}
	)";
	compileAndRun(sourceCode, 20);
	BOOST_REQUIRE(callContractFunction("sendAmount(uint256)", 5) == encodeArgs(5));
	// call to helper should not succeed but amount should be transferred anyway
	BOOST_REQUIRE(callContractFunction("outOfGas()") == bytes());
	BOOST_REQUIRE(callContractFunction("checkState()") == encodeArgs(false, 20 - 5));
}

BOOST_AUTO_TEST_CASE(gasleft_decrease)
{
	char const* sourceCode = R"(
		contract C {
			uint v;
			function f() public returns (bool) {
				uint startGas = gasleft();
				v++;
				assert(startGas > gasleft());
				return true;
			}
			function g() public returns (bool) {
				uint startGas = gasleft();
				assert(startGas > gasleft());
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(gaslimit)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				return block.gaslimit;
			}
		}
	)";
	compileAndRun(sourceCode);
	auto result = callContractFunction("f()");
	ABI_CHECK(result, encodeArgs(gasLimit()));
}

BOOST_AUTO_TEST_CASE(gasprice)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint) {
				return tx.gasprice;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(gasPrice()));
}

BOOST_AUTO_TEST_CASE(blockhash)
{
	char const* sourceCode = R"(
		contract C {
			uint256 counter;
			function g() public returns (bool) { counter++; return true; }
			function f() public returns (bytes32[] memory r) {
				r = new bytes32[](259);
				for (uint i = 0; i < 259; i++)
					r[i] = blockhash(block.number - 257 + i);
			}
		}
	)";
	compileAndRun(sourceCode);
	// generate a sufficient amount of blocks
	while (blockNumber() < u256(255))
		ABI_CHECK(callContractFunction("g()"), encodeArgs(true));

	vector<u256> hashes;
	// ``blockhash()`` is only valid for the last 256 blocks, otherwise zero
	hashes.emplace_back(0);
	for (u256 i = blockNumber() - u256(255); i <= blockNumber(); i++)
		hashes.emplace_back(blockHash(i));
	// the current block hash is not yet known at execution time and therefore zero
	hashes.emplace_back(0);
	// future block hashes are zero
	hashes.emplace_back(0);

	ABI_CHECK(callContractFunction("f()"), encodeDyn(hashes));
}

BOOST_AUTO_TEST_CASE(value_complex)
{
	char const* sourceCode = R"(
		contract helper {
			function getBalance() payable public returns (uint256 myBalance) {
				return address(this).balance;
			}
		}
		contract test {
			helper h;
			constructor() public payable { h = new helper(); }
			function sendAmount(uint amount) public payable returns (uint256 bal) {
				uint someStackElement = 20;
				return h.getBalance.value(amount).gas(1000).value(amount + 3)();
			}
		}
	)";
	compileAndRun(sourceCode, 20);
	BOOST_REQUIRE(callContractFunction("sendAmount(uint256)", 5) == encodeArgs(8));
}

BOOST_AUTO_TEST_CASE(value_insane)
{
	char const* sourceCode = R"(
		contract helper {
			function getBalance() payable public returns (uint256 myBalance) {
				return address(this).balance;
			}
		}
		contract test {
			helper h;
			constructor() public payable { h = new helper(); }
			function sendAmount(uint amount) public returns (uint256 bal) {
				return h.getBalance.value(amount).gas(1000).value(amount + 3)();// overwrite value
			}
		}
	)";
	compileAndRun(sourceCode, 20);
	BOOST_REQUIRE(callContractFunction("sendAmount(uint256)", 5) == encodeArgs(8));
}

BOOST_AUTO_TEST_CASE(value_for_constructor)
{
	char const* sourceCode = R"(
		contract Helper {
			bytes3 name;
			bool flag;
			constructor(bytes3 x, bool f) public payable {
				name = x;
				flag = f;
			}
			function getName() public returns (bytes3 ret) { return name; }
			function getFlag() public returns (bool ret) { return flag; }
		}
		contract Main {
			Helper h;
			constructor() public payable {
				h = (new Helper).value(10)("abc", true);
			}
			function getFlag() public returns (bool ret) { return h.getFlag(); }
			function getName() public returns (bytes3 ret) { return h.getName(); }
			function getBalances() public returns (uint me, uint them) { me = address(this).balance; them = address(h).balance;}
		}
	)";
	compileAndRun(sourceCode, 22, "Main");
	BOOST_REQUIRE(callContractFunction("getFlag()") == encodeArgs(true));
	BOOST_REQUIRE(callContractFunction("getName()") == encodeArgs("abc"));
	BOOST_REQUIRE(callContractFunction("getBalances()") == encodeArgs(12, 10));
}

BOOST_AUTO_TEST_CASE(virtual_function_calls)
{
	char const* sourceCode = R"(
		contract Base {
			function f() public returns (uint i) { return g(); }
			function g() public returns (uint i) { return 1; }
		}
		contract Derived is Base {
			function g() public returns (uint i) { return 2; }
		}
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(2));
	ABI_CHECK(callContractFunction("f()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(access_base_storage)
{
	char const* sourceCode = R"(
		contract Base {
			uint dataBase;
			function getViaBase() public returns (uint i) { return dataBase; }
		}
		contract Derived is Base {
			uint dataDerived;
			function setData(uint base, uint derived) public returns (bool r) {
				dataBase = base;
				dataDerived = derived;
				return true;
			}
			function getViaDerived() public returns (uint base, uint derived) {
				base = dataBase;
				derived = dataDerived;
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("setData(uint256,uint256)", 1, 2), encodeArgs(true));
	ABI_CHECK(callContractFunction("getViaBase()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("getViaDerived()"), encodeArgs(1, 2));
}

BOOST_AUTO_TEST_CASE(single_copy_with_multiple_inheritance)
{
	char const* sourceCode = R"(
		contract Base {
			uint data;
			function setData(uint i) public { data = i; }
			function getViaBase() public returns (uint i) { return data; }
		}
		contract A is Base { function setViaA(uint i) public { setData(i); } }
		contract B is Base { function getViaB() public returns (uint i) { return getViaBase(); } }
		contract Derived is Base, B, A { }
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("getViaB()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("setViaA(uint256)", 23), encodeArgs());
	ABI_CHECK(callContractFunction("getViaB()"), encodeArgs(23));
}

BOOST_AUTO_TEST_CASE(explicit_base_class)
{
	char const* sourceCode = R"(
		contract BaseBase { function g() public returns (uint r) { return 1; } }
		contract Base is BaseBase { function g() public returns (uint r) { return 2; } }
		contract Derived is Base {
			function f() public returns (uint r) { return BaseBase.g(); }
			function g() public returns (uint r) { return 3; }
		}
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("g()"), encodeArgs(3));
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(base_constructor_arguments)
{
	char const* sourceCode = R"(
		contract BaseBase {
			uint m_a;
			constructor(uint a) public {
				m_a = a;
			}
		}
		contract Base is BaseBase(7) {
			constructor() public {
				m_a *= m_a;
			}
		}
		contract Derived is Base() {
			function getA() public returns (uint r) { return m_a; }
		}
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("getA()"), encodeArgs(7 * 7));
}

BOOST_AUTO_TEST_CASE(function_usage_in_constructor_arguments)
{
	char const* sourceCode = R"(
		contract BaseBase {
			uint m_a;
			constructor(uint a) public {
				m_a = a;
			}
			function g() public returns (uint r) { return 2; }
		}
		contract Base is BaseBase(BaseBase.g()) {
		}
		contract Derived is Base() {
			function getA() public returns (uint r) { return m_a; }
		}
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("getA()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(virtual_function_usage_in_constructor_arguments)
{
	char const* sourceCode = R"(
		contract BaseBase {
			uint m_a;
			constructor(uint a) public {
				m_a = a;
			}
			function overridden() public returns (uint r) { return 1; }
			function g() public returns (uint r) { return overridden(); }
		}
		contract Base is BaseBase(BaseBase.g()) {
		}
		contract Derived is Base() {
			function getA() public returns (uint r) { return m_a; }
			function overridden() public returns (uint r) { return 2; }
		}
	)";
	compileAndRun(sourceCode, 0, "Derived");
	ABI_CHECK(callContractFunction("getA()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(internal_constructor)
{
	char const* sourceCode = R"(
		contract C {
			constructor() internal {}
		}
	)";
	BOOST_CHECK(compileAndRunWithoutCheck(sourceCode, 0, "C").empty());
}

BOOST_AUTO_TEST_CASE(function_modifier)
{
	char const* sourceCode = R"(
		contract C {
			function getOne() payable nonFree public returns (uint r) { return 1; }
			modifier nonFree { if (msg.value > 0) _; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getOne()"), encodeArgs(0));
	ABI_CHECK(callContractFunctionWithValue("getOne()", 1), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(function_modifier_local_variables)
{
	char const* sourceCode = R"(
		contract C {
			modifier mod1 { uint8 a = 1; uint8 b = 2; _; }
			modifier mod2(bool a) { if (a) return; else _; }
			function f(bool a) mod1 mod2(a) public returns (uint r) { return 3; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(0));
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(function_modifier_loop)
{
	char const* sourceCode = R"(
		contract C {
			modifier repeat(uint count) { uint i; for (i = 0; i < count; ++i) _; }
			function f() repeat(10) public returns (uint r) { r += 1; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(10));
}

BOOST_AUTO_TEST_CASE(function_modifier_multi_invocation)
{
	char const* sourceCode = R"(
		contract C {
			modifier repeat(bool twice) { if (twice) _; _; }
			function f(bool twice) repeat(twice) public returns (uint r) { r += 1; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(1));
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(function_modifier_multi_with_return)
{
	// Note that return sets the return variable and jumps to the end of the current function or
	// modifier code block.
	char const* sourceCode = R"(
		contract C {
			modifier repeat(bool twice) { if (twice) _; _; }
			function f(bool twice) repeat(twice) public returns (uint r) { r += 1; return r; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(bool)", false), encodeArgs(1));
	ABI_CHECK(callContractFunction("f(bool)", true), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(function_modifier_overriding)
{
	char const* sourceCode = R"(
		contract A {
			function f() mod public returns (bool r) { return true; }
			modifier mod { _; }
		}
		contract C is A {
			modifier mod { if (false) _; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(false));
}

BOOST_AUTO_TEST_CASE(function_modifier_calling_functions_in_creation_context)
{
	char const* sourceCode = R"(
		contract A {
			uint data;
			constructor() mod1 public { f1(); }
			function f1() mod2 public { data |= 0x1; }
			function f2() public { data |= 0x20; }
			function f3() public { }
			modifier mod1 { f2(); _; }
			modifier mod2 { f3(); if (false) _; }
			function getData() public returns (uint r) { return data; }
		}
		contract C is A {
			modifier mod1 { f4(); _; }
			function f3() public { data |= 0x300; }
			function f4() public { data |= 0x4000; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getData()"), encodeArgs(0x4300));
}

BOOST_AUTO_TEST_CASE(function_modifier_for_constructor)
{
	char const* sourceCode = R"(
		contract A {
			uint data;
			constructor() mod1 public { data |= 2; }
			modifier mod1 { data |= 1; _; }
			function getData() public returns (uint r) { return data; }
		}
		contract C is A {
			modifier mod1 { data |= 4; _; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getData()"), encodeArgs(4 | 2));
}

BOOST_AUTO_TEST_CASE(function_modifier_multiple_times)
{
	char const* sourceCode = R"(
		contract C {
			uint public a;
			modifier mod(uint x) { a += x; _; }
			function f(uint x) mod(2) mod(5) mod(x) public returns(uint) { return a; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(2 + 5 + 3));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(2 + 5 + 3));
}

BOOST_AUTO_TEST_CASE(function_modifier_multiple_times_local_vars)
{
	char const* sourceCode = R"(
		contract C {
			uint public a;
			modifier mod(uint x) { uint b = x; a += b; _; a -= b; assert(b == x); }
			function f(uint x) mod(2) mod(5) mod(x) public returns(uint) { return a; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256)", u256(3)), encodeArgs(2 + 5 + 3));
	ABI_CHECK(callContractFunction("a()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(function_modifier_library)
{
	char const* sourceCode = R"(
		library L {
			struct S { uint v; }
			modifier mod(S storage s) { s.v++; _; }
			function libFun(S storage s) mod(s) internal { s.v += 0x100; }
		}

		contract Test {
			using L for *;
			L.S s;

			function f() public returns (uint) {
				s.libFun();
				L.libFun(s);
				return s.v;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0x202));
}

BOOST_AUTO_TEST_CASE(function_modifier_library_inheritance)
{
	// Tests that virtual lookup for modifiers in libraries does not consider
	// the current inheritance hierarchy.

	char const* sourceCode = R"(
		library L {
			struct S { uint v; }
			modifier mod(S storage s) { s.v++; _; }
			function libFun(S storage s) mod(s) internal { s.v += 0x100; }
		}

		contract Test {
			using L for *;
			L.S s;
			modifier mod(L.S storage) { revert(); _; }

			function f() public returns (uint) {
				s.libFun();
				L.libFun(s);
				return s.v;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0x202));
}

BOOST_AUTO_TEST_CASE(crazy_elementary_typenames_on_stack)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (uint r) {
				uint; uint; uint; uint;
				int x = -7;
				return uint(x);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(u256(-7)));
}

BOOST_AUTO_TEST_CASE(super)
{
	char const* sourceCode = R"(
		contract A { function f() public returns (uint r) { return 1; } }
		contract B is A { function f() public returns (uint r) { return super.f() | 2; } }
		contract C is A { function f() public returns (uint r) { return super.f() | 4; } }
		contract D is B, C { function f() public returns (uint r) { return super.f() | 8; } }
	)";
	compileAndRun(sourceCode, 0, "D");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1 | 2 | 4 | 8));
}

BOOST_AUTO_TEST_CASE(super_in_constructor)
{
	char const* sourceCode = R"(
		contract A { function f() public returns (uint r) { return 1; } }
		contract B is A { function f() public returns (uint r) { return super.f() | 2; } }
		contract C is A { function f() public returns (uint r) { return super.f() | 4; } }
		contract D is B, C { uint data; constructor() public { data = super.f() | 8; } function f() public returns (uint r) { return data; } }
	)";
	compileAndRun(sourceCode, 0, "D");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1 | 2 | 4 | 8));
}

BOOST_AUTO_TEST_CASE(super_alone)
{
	char const* sourceCode = R"(
		contract A { function f() public { super; } }
	)";
	compileAndRun(sourceCode, 0, "A");
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(fallback_function)
{
	char const* sourceCode = R"(
		contract A {
			uint data;
			function() external { data = 1; }
			function getData() public returns (uint r) { return data; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getData()"), encodeArgs(0));
	ABI_CHECK(callContractFunction(""), encodeArgs());
	ABI_CHECK(callContractFunction("getData()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(inherited_fallback_function)
{
	char const* sourceCode = R"(
		contract A {
			uint data;
			function() external { data = 1; }
			function getData() public returns (uint r) { return data; }
		}
		contract B is A {}
	)";
	compileAndRun(sourceCode, 0, "B");
	ABI_CHECK(callContractFunction("getData()"), encodeArgs(0));
	ABI_CHECK(callContractFunction(""), encodeArgs());
	ABI_CHECK(callContractFunction("getData()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(default_fallback_throws)
{
	char const* sourceCode = R"YY(
		contract A {
			function f() public returns (bool) {
				(bool success,) = address(this).call("");
				return success;
			}
		}
	)YY";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0));

	if (dev::test::Options::get().evmVersion().hasStaticCall())
	{
		char const* sourceCode = R"YY(
			contract A {
				function f() public returns (bool) {
					(bool success, bytes memory data) = address(this).staticcall("");
					assert(data.length == 0);
					return success;
				}
			}
		)YY";
		compileAndRun(sourceCode);
		ABI_CHECK(callContractFunction("f()"), encodeArgs(0));
	}
}

BOOST_AUTO_TEST_CASE(short_data_calls_fallback)
{
	char const* sourceCode = R"(
		contract A {
			uint public x;
			// Signature is d88e0b00
			function fow() public { x = 3; }
			function () external { x = 2; }
		}
	)";
	compileAndRun(sourceCode);
	// should call fallback
	sendMessage(asBytes("\xd8\x8e\x0b"), false, 0);
	BOOST_CHECK(m_transactionSuccessful);
	ABI_CHECK(callContractFunction("x()"), encodeArgs(2));
	// should call function
	sendMessage(asBytes(string("\xd8\x8e\x0b") + string(1, 0)), false, 0);
	BOOST_CHECK(m_transactionSuccessful);
	ABI_CHECK(callContractFunction("x()"), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(event)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit(address indexed _from, bytes32 indexed _id, uint _value);
			function deposit(bytes32 _id, bool _manually) public payable {
				if (_manually) {
					bytes32 s = 0x19dacbf83c5de6658e14cbf7bcae5c15eca2eedecf1c66fbca928e4d351bea0f;
					log3(bytes32(msg.value), s, bytes32(uint256(msg.sender)), _id);
				} else {
					emit Deposit(msg.sender, _id, msg.value);
				}
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 value(18);
	u256 id(0x1234);
	for (bool manually: {true, false})
	{
		callContractFunctionWithValue("deposit(bytes32,bool)", value, id, manually);
		BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
		BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
		BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(value)));
		BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 3);
		BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,bytes32,uint256)")));
		BOOST_CHECK_EQUAL(m_logs[0].topics[1], h256(m_sender, h256::AlignRight));
		BOOST_CHECK_EQUAL(m_logs[0].topics[2], h256(id));
	}
}

BOOST_AUTO_TEST_CASE(event_emit)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit(address indexed _from, bytes32 indexed _id, uint _value);
			function deposit(bytes32 _id) public payable {
				emit Deposit(msg.sender, _id, msg.value);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 value(18);
	u256 id(0x1234);
	callContractFunctionWithValue("deposit(bytes32)", value, id);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(h256(m_logs[0].data), h256(u256(value)));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 3);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,bytes32,uint256)")));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], h256(m_sender, h256::AlignRight));
	BOOST_CHECK_EQUAL(m_logs[0].topics[2], h256(id));
}

BOOST_AUTO_TEST_CASE(event_no_arguments)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit();
			function deposit() public {
				emit Deposit();
			}
		}
	)";

	compileAndRun(sourceCode);
	callContractFunction("deposit()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data.empty());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit()")));
}

BOOST_AUTO_TEST_CASE(event_access_through_base_name_emit)
{
	char const* sourceCode = R"(
		contract A {
			event x();
		}
		contract B is A {
			function f() public returns (uint) {
				emit A.x();
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("f()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data.empty());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("x()")));
}

BOOST_AUTO_TEST_CASE(events_with_same_name)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit();
			event Deposit(address _addr);
			event Deposit(address _addr, uint _amount);
			event Deposit(address _addr, bool _flag);
			function deposit() public returns (uint) {
				emit Deposit();
				return 1;
			}
			function deposit(address _addr) public returns (uint) {
				emit Deposit(_addr);
				return 2;
			}
			function deposit(address _addr, uint _amount) public returns (uint) {
				emit Deposit(_addr, _amount);
				return 3;
			}
			function deposit(address _addr, bool _flag) public returns (uint) {
				emit Deposit(_addr, _flag);
				return 4;
			}
		}
	)";
	u160 const c_loggedAddress = m_contractAddress;

	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("deposit()"), encodeArgs(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data.empty());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit()")));

	ABI_CHECK(callContractFunction("deposit(address)", c_loggedAddress), encodeArgs(u256(2)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(c_loggedAddress));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address)")));

	ABI_CHECK(callContractFunction("deposit(address,uint256)", c_loggedAddress, u256(100)), encodeArgs(u256(3)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(c_loggedAddress, 100));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,uint256)")));

	ABI_CHECK(callContractFunction("deposit(address,bool)", c_loggedAddress, false), encodeArgs(u256(4)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(c_loggedAddress, false));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,bool)")));
}

BOOST_AUTO_TEST_CASE(events_with_same_name_inherited_emit)
{
	char const* sourceCode = R"(
		contract A {
			event Deposit();
		}

		contract B {
			event Deposit(address _addr);
		}

		contract ClientReceipt is A, B {
			event Deposit(address _addr, uint _amount);
			function deposit() public returns (uint) {
				emit Deposit();
				return 1;
			}
			function deposit(address _addr) public returns (uint) {
				emit Deposit(_addr);
				return 1;
			}
			function deposit(address _addr, uint _amount) public returns (uint) {
				emit Deposit(_addr, _amount);
				return 1;
			}
		}
	)";
	u160 const c_loggedAddress = m_contractAddress;

	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("deposit()"), encodeArgs(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data.empty());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit()")));

	ABI_CHECK(callContractFunction("deposit(address)", c_loggedAddress), encodeArgs(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(c_loggedAddress));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address)")));

	ABI_CHECK(callContractFunction("deposit(address,uint256)", c_loggedAddress, u256(100)), encodeArgs(u256(1)));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(c_loggedAddress, 100));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,uint256)")));
}

BOOST_AUTO_TEST_CASE(event_anonymous)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit() anonymous;
			function deposit() public {
				emit Deposit();
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("deposit()");
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 0);
}

BOOST_AUTO_TEST_CASE(event_anonymous_with_topics)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit(address indexed _from, bytes32 indexed _id, uint indexed _value, uint indexed _value2, bytes32 data) anonymous;
			function deposit(bytes32 _id) public payable {
				emit Deposit(msg.sender, _id, msg.value, 2, "abc");
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 value(18);
	u256 id(0x1234);
	callContractFunctionWithValue("deposit(bytes32)", value, id);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs("abc"));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 4);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], h256(m_sender, h256::AlignRight));
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], h256(id));
	BOOST_CHECK_EQUAL(m_logs[0].topics[2], h256(value));
	BOOST_CHECK_EQUAL(m_logs[0].topics[3], h256(2));
}

BOOST_AUTO_TEST_CASE(event_lots_of_data)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit(address _from, bytes32 _id, uint _value, bool _flag);
			function deposit(bytes32 _id) public payable {
				emit Deposit(msg.sender, _id, msg.value, true);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 value(18);
	u256 id(0x1234);
	callContractFunctionWithValue("deposit(bytes32)", value, id);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs((u160)m_sender, id, value, true));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(address,bytes32,uint256,bool)")));
}

BOOST_AUTO_TEST_CASE(event_really_lots_of_data)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			event Deposit(uint fixeda, bytes dynx, uint fixedb);
			function deposit() public {
				emit Deposit(10, msg.data, 15);
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("deposit()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(toHex(m_logs[0].data), toHex(encodeArgs(10, 0x60, 15, 4, asString(FixedHash<4>(dev::keccak256("deposit()")).asBytes()))));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(uint256,bytes,uint256)")));
}

BOOST_AUTO_TEST_CASE(event_really_lots_of_data_from_storage)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			bytes x;
			event Deposit(uint fixeda, bytes dynx, uint fixedb);
			function deposit() public {
				x.length = 3;
				x[0] = "A";
				x[1] = "B";
				x[2] = "C";
				emit Deposit(10, x, 15);
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("deposit()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK_EQUAL(toHex(m_logs[0].data), toHex(encodeArgs(10, 0x60, 15, 3, string("ABC"))));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(uint256,bytes,uint256)")));
}

BOOST_AUTO_TEST_CASE(event_really_really_lots_of_data_from_storage)
{
	char const* sourceCode = R"(
		contract ClientReceipt {
			bytes x;
			event Deposit(uint fixeda, bytes dynx, uint fixedb);
			function deposit() public {
				x.length = 31;
				x[0] = "A";
				x[1] = "B";
				x[2] = "C";
				x[30] = "Z";
				emit Deposit(10, x, 15);
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("deposit()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(10, 0x60, 15, 31, string("ABC") + string(27, 0) + "Z"));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Deposit(uint256,bytes,uint256)")));
}

BOOST_AUTO_TEST_CASE(event_struct_memory_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint a; }
			event E(S);
			function createEvent(uint x) public {
				emit E(S(x));
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(x));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E((uint256))")));
}

BOOST_AUTO_TEST_CASE(event_struct_storage_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			struct S { uint a; }
			event E(S);
			S s;
			function createEvent(uint x) public {
				s.a = x;
				emit E(s);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(x));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E((uint256))")));
}

BOOST_AUTO_TEST_CASE(event_dynamic_array_memory)
{
	char const* sourceCode = R"(
		contract C {
			event E(uint[]);
			function createEvent(uint x) public {
				uint[] memory arr = new uint[](3);
				arr[0] = x;
				arr[1] = x + 1;
				arr[2] = x + 2;
				emit E(arr);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(0x20, 3, x, x + 1, x + 2));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(uint256[])")));
}

BOOST_AUTO_TEST_CASE(event_dynamic_array_memory_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			event E(uint[]);
			function createEvent(uint x) public {
				uint[] memory arr = new uint[](3);
				arr[0] = x;
				arr[1] = x + 1;
				arr[2] = x + 2;
				emit E(arr);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(0x20, 3, x, x + 1, x + 2));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(uint256[])")));
}

BOOST_AUTO_TEST_CASE(event_dynamic_nested_array_memory_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			event E(uint[][]);
			function createEvent(uint x) public {
				uint[][] memory arr = new uint[][](2);
				arr[0] = new uint[](2);
				arr[1] = new uint[](2);
				arr[0][0] = x;
				arr[0][1] = x + 1;
				arr[1][0] = x + 2;
				arr[1][1] = x + 3;
				emit E(arr);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(0x20, 2, 0x40, 0xa0, 2, x, x + 1, 2, x + 2, x + 3));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(uint256[][])")));
}

BOOST_AUTO_TEST_CASE(event_dynamic_array_storage)
{
	char const* sourceCode = R"(
		contract C {
			event E(uint[]);
			uint[] arr;
			function createEvent(uint x) public {
				arr.length = 3;
				arr[0] = x;
				arr[1] = x + 1;
				arr[2] = x + 2;
				emit E(arr);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(0x20, 3, x, x + 1, x + 2));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(uint256[])")));
}

BOOST_AUTO_TEST_CASE(event_dynamic_array_storage_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			event E(uint[]);
			uint[] arr;
			function createEvent(uint x) public {
				arr.length = 3;
				arr[0] = x;
				arr[1] = x + 1;
				arr[2] = x + 2;
				emit E(arr);
			}
		}
	)";
	compileAndRun(sourceCode);
	u256 x(42);
	callContractFunction("createEvent(uint256)", x);
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(0x20, 3, x, x + 1, x + 2));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(uint256[])")));
}

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
