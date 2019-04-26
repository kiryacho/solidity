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

BOOST_AUTO_TEST_CASE(event_dynamic_nested_array_storage_v2)
{
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract C {
			event E(uint[][]);
			uint[][] arr;
			function createEvent(uint x) public {
				arr.length = 2;
				arr[0].length = 2;
				arr[1].length = 2;
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

BOOST_AUTO_TEST_CASE(event_indexed_string)
{
	char const* sourceCode = R"(
		contract C {
			string x;
			uint[4] y;
			event E(string indexed r, uint[4] indexed t);
			function deposit() public {
				bytes(x).length = 90;
				for (uint8 i = 0; i < 90; i++)
					bytes(x)[i] = byte(i);
				y[0] = 4;
				y[1] = 5;
				y[2] = 6;
				y[3] = 7;
				emit E(x, y);
			}
		}
	)";
	compileAndRun(sourceCode);
	callContractFunction("deposit()");
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	string dynx(90, 0);
	for (size_t i = 0; i < dynx.size(); ++i)
		dynx[i] = i;
	BOOST_CHECK(m_logs[0].data == bytes());
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 3);
	BOOST_CHECK_EQUAL(m_logs[0].topics[1], dev::keccak256(dynx));
	BOOST_CHECK_EQUAL(m_logs[0].topics[2], dev::keccak256(
		encodeArgs(u256(4), u256(5), u256(6), u256(7))
	));
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("E(string,uint256[4])")));
}

BOOST_AUTO_TEST_CASE(empty_name_input_parameter_with_named_one)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint, uint k) public returns(uint ret_k, uint ret_g){
				uint g = 8;
				ret_k = k;
				ret_g = g;
			}
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(callContractFunction("f(uint256,uint256)", 5, 9) != encodeArgs(5, 8));
	ABI_CHECK(callContractFunction("f(uint256,uint256)", 5, 9), encodeArgs(9, 8));
}

BOOST_AUTO_TEST_CASE(empty_name_return_parameter)
{
	char const* sourceCode = R"(
		contract test {
			function f(uint k) public returns(uint){
				return k;
		}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f(uint256)", 9), encodeArgs(9));
}

BOOST_AUTO_TEST_CASE(sha256_empty)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bytes32) {
				return sha256("");
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), fromHex("0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"));
}

BOOST_AUTO_TEST_CASE(ripemd160_empty)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bytes20) {
				return ripemd160("");
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), fromHex("0x9c1185a5c5e9fc54612808977ee8f548b2258d31000000000000000000000000"));
}

BOOST_AUTO_TEST_CASE(keccak256_empty)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bytes32) {
				return keccak256("");
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("f()"), fromHex("0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"));
}

BOOST_AUTO_TEST_CASE(keccak256_multiple_arguments)
{
	char const* sourceCode = R"(
		contract c {
			function foo(uint a, uint b, uint c) public returns (bytes32 d)
			{
				d = keccak256(abi.encodePacked(a, b, c));
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("foo(uint256,uint256,uint256)", 10, 12, 13), encodeArgs(
		dev::keccak256(
			toBigEndian(u256(10)) +
			toBigEndian(u256(12)) +
			toBigEndian(u256(13))
		)
	));
}

BOOST_AUTO_TEST_CASE(keccak256_multiple_arguments_with_numeric_literals)
{
	char const* sourceCode = R"(
		contract c {
			function foo(uint a, uint16 b) public returns (bytes32 d)
			{
				d = keccak256(abi.encodePacked(a, b, uint8(145)));
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("foo(uint256,uint16)", 10, 12), encodeArgs(
		dev::keccak256(
			toBigEndian(u256(10)) +
			bytes{0x0, 0xc} +
			bytes(1, 0x91)
		)
	));
}

BOOST_AUTO_TEST_CASE(keccak256_multiple_arguments_with_string_literals)
{
	char const* sourceCode = R"(
		contract c {
			function foo() public returns (bytes32 d)
			{
				d = keccak256("foo");
			}
			function bar(uint a, uint16 b) public returns (bytes32 d)
			{
				d = keccak256(abi.encodePacked(a, b, uint8(145), "foo"));
			}
		}
	)";
	compileAndRun(sourceCode);

	ABI_CHECK(callContractFunction("foo()"), encodeArgs(dev::keccak256("foo")));

	ABI_CHECK(callContractFunction("bar(uint256,uint16)", 10, 12), encodeArgs(
		dev::keccak256(
			toBigEndian(u256(10)) +
			bytes{0x0, 0xc} +
			bytes(1, 0x91) +
			bytes{0x66, 0x6f, 0x6f}
		)
	));
}

BOOST_AUTO_TEST_CASE(keccak256_with_bytes)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function foo() public returns (bool)
			{
				data.length = 3;
				data[0] = "f";
				data[1] = "o";
				data[2] = "o";
				return keccak256(data) == keccak256("foo");
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("foo()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(iterated_keccak256_with_bytes)
{
	char const* sourceCode = R"ABC(
		contract c {
			bytes data;
			function foo() public returns (bytes32)
			{
				data.length = 3;
				data[0] = "x";
				data[1] = "y";
				data[2] = "z";
				return keccak256(abi.encodePacked("b", keccak256(data), "a"));
			}
		}
	)ABC";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("foo()"), encodeArgs(
		u256(dev::keccak256(bytes{'b'} + dev::keccak256("xyz").asBytes() + bytes{'a'}))
	));
}

BOOST_AUTO_TEST_CASE(generic_call)
{
	char const* sourceCode = R"**(
			contract receiver {
				uint public received;
				function receive(uint256 x) public payable { received = x; }
			}
			contract sender {
				constructor() public payable {}
				function doSend(address rec) public returns (uint d)
				{
					bytes4 signature = bytes4(bytes32(keccak256("receive(uint256)")));
					rec.call.value(2)(abi.encodeWithSelector(signature, 23));
					return receiver(rec).received();
				}
			}
	)**";
	compileAndRun(sourceCode, 0, "receiver");
	u160 const c_receiverAddress = m_contractAddress;
	compileAndRun(sourceCode, 50, "sender");
	BOOST_REQUIRE(callContractFunction("doSend(address)", c_receiverAddress) == encodeArgs(23));
	BOOST_CHECK_EQUAL(balanceAt(m_contractAddress), 50 - 2);
}

BOOST_AUTO_TEST_CASE(generic_delegatecall)
{
	char const* sourceCode = R"**(
			contract Receiver {
				uint public received;
				address public sender;
				uint public value;
				constructor() public payable {}
				function receive(uint256 x) public payable { received = x; sender = msg.sender; value = msg.value; }
			}
			contract Sender {
				uint public received;
				address public sender;
				uint public value;
				constructor() public payable {}
				function doSend(address rec) public payable
				{
					bytes4 signature = bytes4(bytes32(keccak256("receive(uint256)")));
					(bool success,) = rec.delegatecall(abi.encodeWithSelector(signature, 23));
					success;
				}
			}
	)**";

	for (auto v2: {false, true})
	{
		string source = (v2 ? "pragma experimental ABIEncoderV2;\n" : "") + string(sourceCode);

		compileAndRun(source, 0, "Receiver");
		u160 const c_receiverAddress = m_contractAddress;
		compileAndRun(source, 50, "Sender");
		u160 const c_senderAddress = m_contractAddress;
		BOOST_CHECK(m_sender != c_senderAddress); // just for sanity
		ABI_CHECK(callContractFunctionWithValue("doSend(address)", 11, c_receiverAddress), encodeArgs());
		ABI_CHECK(callContractFunction("received()"), encodeArgs(u256(23)));
		ABI_CHECK(callContractFunction("sender()"), encodeArgs(u160(m_sender)));
		ABI_CHECK(callContractFunction("value()"), encodeArgs(u256(11)));
		m_contractAddress = c_receiverAddress;
		ABI_CHECK(callContractFunction("received()"), encodeArgs(u256(0)));
		ABI_CHECK(callContractFunction("sender()"), encodeArgs(u256(0)));
		ABI_CHECK(callContractFunction("value()"), encodeArgs(u256(0)));
		BOOST_CHECK(storageEmpty(c_receiverAddress));
		BOOST_CHECK(!storageEmpty(c_senderAddress));
		BOOST_CHECK_EQUAL(balanceAt(c_receiverAddress), 0);
		BOOST_CHECK_EQUAL(balanceAt(c_senderAddress), 50 + 11);
	}
}

BOOST_AUTO_TEST_CASE(generic_staticcall)
{
	if (dev::test::Options::get().evmVersion().hasStaticCall())
	{
		char const* sourceCode = R"**(
				contract A {
					uint public x;
					constructor() public { x = 42; }
					function pureFunction(uint256 p) public pure returns (uint256) { return p; }
					function viewFunction(uint256 p) public view returns (uint256) { return p + x; }
					function nonpayableFunction(uint256 p) public returns (uint256) { x = p; return x; }
					function assertFunction(uint256 p) public view returns (uint256) { assert(x == p); return x; }
				}
				contract C {
					function f(address a) public view returns (bool, bytes memory)
					{
						return a.staticcall(abi.encodeWithSignature("pureFunction(uint256)", 23));
					}
					function g(address a) public view returns (bool, bytes memory)
					{
						return a.staticcall(abi.encodeWithSignature("viewFunction(uint256)", 23));
					}
					function h(address a) public view returns (bool, bytes memory)
					{
						return a.staticcall(abi.encodeWithSignature("nonpayableFunction(uint256)", 23));
					}
					function i(address a, uint256 v) public view returns (bool, bytes memory)
					{
						return a.staticcall(abi.encodeWithSignature("assertFunction(uint256)", v));
					}
				}
		)**";
		compileAndRun(sourceCode, 0, "A");
		u160 const c_addressA = m_contractAddress;
		compileAndRun(sourceCode, 0, "C");
		ABI_CHECK(callContractFunction("f(address)", c_addressA), encodeArgs(true, 0x40, 0x20, 23));
		ABI_CHECK(callContractFunction("g(address)", c_addressA), encodeArgs(true, 0x40, 0x20, 23 + 42));
		ABI_CHECK(callContractFunction("h(address)", c_addressA), encodeArgs(false, 0x40, 0x00));
		ABI_CHECK(callContractFunction("i(address,uint256)", c_addressA, 42), encodeArgs(true, 0x40, 0x20, 42));
		ABI_CHECK(callContractFunction("i(address,uint256)", c_addressA, 23), encodeArgs(false, 0x40, 0x00));
	}
}

BOOST_AUTO_TEST_CASE(library_call_in_homestead)
{
	char const* sourceCode = R"(
		library Lib { function m() public returns (address) { return msg.sender; } }
		contract Test {
			address public sender;
			function f() public {
				sender = Lib.m();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs());
	ABI_CHECK(callContractFunction("sender()"), encodeArgs(u160(m_sender)));
}

BOOST_AUTO_TEST_CASE(library_call_protection)
{
	// This tests code that reverts a call if it is a direct call to a library
	// as opposed to a delegatecall.
	char const* sourceCode = R"(
		library Lib {
			struct S { uint x; }
			// a direct call to this should revert
			function np(S storage s) public returns (address) { s.x = 3; return msg.sender; }
			// a direct call to this is fine
			function v(S storage) public view returns (address) { return msg.sender; }
			// a direct call to this is fine
			function pu() public pure returns (uint) { return 2; }
		}
		contract Test {
			Lib.S public s;
			function np() public returns (address) { return Lib.np(s); }
			function v() public view returns (address) { return Lib.v(s); }
			function pu() public pure returns (uint) { return Lib.pu(); }
		}
	)";
	compileAndRun(sourceCode, 0, "Lib");
	ABI_CHECK(callContractFunction("np(Lib.S storage)", 0), encodeArgs());
	ABI_CHECK(callContractFunction("v(Lib.S storage)", 0), encodeArgs(u160(m_sender)));
	ABI_CHECK(callContractFunction("pu()"), encodeArgs(2));
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("s()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("np()"), encodeArgs(u160(m_sender)));
	ABI_CHECK(callContractFunction("s()"), encodeArgs(3));
	ABI_CHECK(callContractFunction("v()"), encodeArgs(u160(m_sender)));
	ABI_CHECK(callContractFunction("pu()"), encodeArgs(2));
}


BOOST_AUTO_TEST_CASE(library_staticcall_delegatecall)
{
	char const* sourceCode = R"(
		 library Lib {
			 function x() public view returns (uint) {
				 return 1;
			 }
		 }
		 contract Test {
			 uint t;
			 function f() public returns (uint) {
				 t = 2;
				 return this.g();
			 }
			 function g() public view returns (uint) {
				 return Lib.x();
			 }
		 }
	)";
	compileAndRun(sourceCode, 0, "Lib");
	compileAndRun(sourceCode, 0, "Test", bytes(), map<string, Address>{{"Lib", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(store_bytes)
{
	// this test just checks that the copy loop does not mess up the stack
	char const* sourceCode = R"(
		contract C {
			function save() public returns (uint r) {
				r = 23;
				savedData = msg.data;
				r = 24;
			}
			bytes savedData;
		}
	)";
	compileAndRun(sourceCode);
	// empty copy loop
	ABI_CHECK(callContractFunction("save()"), encodeArgs(24));
	ABI_CHECK(callContractFunction("save()", "abcdefg"), encodeArgs(24));
}

BOOST_AUTO_TEST_CASE(bytes_from_calldata_to_memory)
{
	char const* sourceCode = R"(
		contract C {
			function f() public returns (bytes32) {
				return keccak256(abi.encodePacked("abc", msg.data));
			}
		}
	)";
	compileAndRun(sourceCode);
	bytes calldata1 = FixedHash<4>(dev::keccak256("f()")).asBytes() + bytes(61, 0x22) + bytes(12, 0x12);
	sendMessage(calldata1, false);
	BOOST_CHECK(m_transactionSuccessful);
	BOOST_CHECK(m_output == encodeArgs(dev::keccak256(bytes{'a', 'b', 'c'} + calldata1)));
}

BOOST_AUTO_TEST_CASE(call_forward_bytes)
{
	char const* sourceCode = R"(
		contract receiver {
			uint public received;
			function receive(uint x) public { received += x + 1; }
			function() external { received = 0x80; }
		}
		contract sender {
			constructor() public { rec = new receiver(); }
			function() external { savedData = msg.data; }
			function forward() public returns (bool) { address(rec).call(savedData); return true; }
			function clear() public returns (bool) { delete savedData; return true; }
			function val() public returns (uint) { return rec.received(); }
			receiver rec;
			bytes savedData;
		}
	)";
	compileAndRun(sourceCode, 0, "sender");
	ABI_CHECK(callContractFunction("receive(uint256)", 7), bytes());
	ABI_CHECK(callContractFunction("val()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("forward()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("val()"), encodeArgs(8));
	ABI_CHECK(callContractFunction("clear()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("val()"), encodeArgs(8));
	ABI_CHECK(callContractFunction("forward()"), encodeArgs(true));
	ABI_CHECK(callContractFunction("val()"), encodeArgs(0x80));
}

BOOST_AUTO_TEST_CASE(call_forward_bytes_length)
{
	char const* sourceCode = R"(
		contract receiver {
			uint public calledLength;
			function() external { calledLength = msg.data.length; }
		}
		contract sender {
			receiver rec;
			constructor() public { rec = new receiver(); }
			function viaCalldata() public returns (uint) {
				(bool success,) = address(rec).call(msg.data);
				require(success);
				return rec.calledLength();
			}
			function viaMemory() public returns (uint) {
				bytes memory x = msg.data;
				(bool success,) = address(rec).call(x);
				require(success);
				return rec.calledLength();
			}
			bytes s;
			function viaStorage() public returns (uint) {
				s = msg.data;
				(bool success,) = address(rec).call(s);
				require(success);
				return rec.calledLength();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "sender");

	// No additional data, just function selector
	ABI_CHECK(callContractFunction("viaCalldata()"), encodeArgs(4));
	ABI_CHECK(callContractFunction("viaMemory()"), encodeArgs(4));
	ABI_CHECK(callContractFunction("viaStorage()"), encodeArgs(4));

	// Some additional unpadded data
	bytes unpadded = asBytes(string("abc"));
	ABI_CHECK(callContractFunctionNoEncoding("viaCalldata()", unpadded), encodeArgs(7));
	ABI_CHECK(callContractFunctionNoEncoding("viaMemory()", unpadded), encodeArgs(7));
	ABI_CHECK(callContractFunctionNoEncoding("viaStorage()", unpadded), encodeArgs(7));
}

BOOST_AUTO_TEST_CASE(copying_bytes_multiassign)
{
	char const* sourceCode = R"(
		contract receiver {
			uint public received;
			function receive(uint x) public { received += x + 1; }
			function() external { received = 0x80; }
		}
		contract sender {
			constructor() public { rec = new receiver(); }
			function() external { savedData1 = savedData2 = msg.data; }
			function forward(bool selector) public returns (bool) {
				if (selector) { address(rec).call(savedData1); delete savedData1; }
				else { address(rec).call(savedData2); delete savedData2; }
				return true;
			}
			function val() public returns (uint) { return rec.received(); }
			receiver rec;
			bytes savedData1;
			bytes savedData2;
		}
	)";
	compileAndRun(sourceCode, 0, "sender");
	ABI_CHECK(callContractFunction("receive(uint256)", 7), bytes());
	ABI_CHECK(callContractFunction("val()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("forward(bool)", true), encodeArgs(true));
	ABI_CHECK(callContractFunction("val()"), encodeArgs(8));
	ABI_CHECK(callContractFunction("forward(bool)", false), encodeArgs(true));
	ABI_CHECK(callContractFunction("val()"), encodeArgs(16));
	ABI_CHECK(callContractFunction("forward(bool)", true), encodeArgs(true));
	ABI_CHECK(callContractFunction("val()"), encodeArgs(0x80));
}

BOOST_AUTO_TEST_CASE(delete_removes_bytes_data)
{
	char const* sourceCode = R"(
		contract c {
			function() external { data = msg.data; }
			function del() public returns (bool) { delete data; return true; }
			bytes data;
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("---", 7), bytes());
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("del()", 7), encodeArgs(true));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(copy_from_calldata_removes_bytes_data)
{
	char const* sourceCode = R"(
		contract c {
			function set() public returns (bool) { data = msg.data; return true; }
			function() external { data = msg.data; }
			bytes data;
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("set()", 1, 2, 3, 4, 5), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	sendMessage(bytes(), false);
	BOOST_CHECK(m_transactionSuccessful);
	BOOST_CHECK(m_output.empty());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(copy_removes_bytes_data)
{
	char const* sourceCode = R"(
		contract c {
			function set() public returns (bool) { data1 = msg.data; return true; }
			function reset() public returns (bool) { data1 = data2; return true; }
			bytes data1;
			bytes data2;
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("set()", 1, 2, 3, 4, 5), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("reset()"), encodeArgs(true));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(bytes_inside_mappings)
{
	char const* sourceCode = R"(
		contract c {
			function set(uint key) public returns (bool) { data[key] = msg.data; return true; }
			function copy(uint from, uint to) public returns (bool) { data[to] = data[from]; return true; }
			mapping(uint => bytes) data;
		}
	)";
	compileAndRun(sourceCode);
	// store a short byte array at 1 and a longer one at 2
	ABI_CHECK(callContractFunction("set(uint256)", 1, 2), encodeArgs(true));
	ABI_CHECK(callContractFunction("set(uint256)", 2, 2, 3, 4, 5), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	// copy shorter to longer
	ABI_CHECK(callContractFunction("copy(uint256,uint256)", 1, 2), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	// copy empty to both
	ABI_CHECK(callContractFunction("copy(uint256,uint256)", 99, 1), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("copy(uint256,uint256)", 99, 2), encodeArgs(true));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(bytes_length_member)
{
	char const* sourceCode = R"(
		contract c {
			function set() public returns (bool) { data = msg.data; return true; }
			function getLength() public returns (uint) { return data.length; }
			bytes data;
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getLength()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("set()", 1, 2), encodeArgs(true));
	ABI_CHECK(callContractFunction("getLength()"), encodeArgs(4+32+32));
}

BOOST_AUTO_TEST_CASE(struct_copy)
{
	char const* sourceCode = R"(
		contract c {
			struct Nested { uint x; uint y; }
			struct Struct { uint a; mapping(uint => Struct) b; Nested nested; uint c; }
			mapping(uint => Struct) data;
			function set(uint k) public returns (bool) {
				data[k].a = 1;
				data[k].nested.x = 3;
				data[k].nested.y = 4;
				data[k].c = 2;
				return true;
			}
			function copy(uint from, uint to) public returns (bool) {
				data[to] = data[from];
				return true;
			}
			function retrieve(uint k) public returns (uint a, uint x, uint y, uint c)
			{
				a = data[k].a;
				x = data[k].nested.x;
				y = data[k].nested.y;
				c = data[k].c;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("set(uint256)", 7), encodeArgs(true));
	ABI_CHECK(callContractFunction("retrieve(uint256)", 7), encodeArgs(1, 3, 4, 2));
	ABI_CHECK(callContractFunction("copy(uint256,uint256)", 7, 8), encodeArgs(true));
	ABI_CHECK(callContractFunction("retrieve(uint256)", 7), encodeArgs(1, 3, 4, 2));
	ABI_CHECK(callContractFunction("retrieve(uint256)", 8), encodeArgs(1, 3, 4, 2));
	ABI_CHECK(callContractFunction("copy(uint256,uint256)", 0, 7), encodeArgs(true));
	ABI_CHECK(callContractFunction("retrieve(uint256)", 7), encodeArgs(0, 0, 0, 0));
	ABI_CHECK(callContractFunction("retrieve(uint256)", 8), encodeArgs(1, 3, 4, 2));
	ABI_CHECK(callContractFunction("copy(uint256,uint256)", 7, 8), encodeArgs(true));
	ABI_CHECK(callContractFunction("retrieve(uint256)", 8), encodeArgs(0, 0, 0, 0));
}

BOOST_AUTO_TEST_CASE(struct_containing_bytes_copy_and_delete)
{
	char const* sourceCode = R"(
		contract c {
			struct Struct { uint a; bytes data; uint b; }
			Struct data1;
			Struct data2;
			function set(uint _a, bytes calldata _data, uint _b) external returns (bool) {
				data1.a = _a;
				data1.b = _b;
				data1.data = _data;
				return true;
			}
			function copy() public returns (bool) {
				data1 = data2;
				return true;
			}
			function del() public returns (bool) {
				delete data1;
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	string data = "123456789012345678901234567890123";
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("set(uint256,bytes,uint256)", 12, 0x60, 13, u256(data.length()), data), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("copy()"), encodeArgs(true));
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("set(uint256,bytes,uint256)", 12, 0x60, 13, u256(data.length()), data), encodeArgs(true));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("del()"), encodeArgs(true));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(struct_copy_via_local)
{
	char const* sourceCode = R"(
		contract c {
			struct Struct { uint a; uint b; }
			Struct data1;
			Struct data2;
			function test() public returns (bool) {
				data1.a = 1;
				data1.b = 2;
				Struct memory x = data1;
				data2 = x;
				return data2.a == data1.a && data2.b == data1.b;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(true));
}

BOOST_AUTO_TEST_CASE(using_enums)
{
	char const* sourceCode = R"(
			contract test {
				enum ActionChoices { GoLeft, GoRight, GoStraight, Sit }
				constructor() public
				{
					choices = ActionChoices.GoStraight;
				}
				function getChoice() public returns (uint d)
				{
					d = uint256(choices);
				}
				ActionChoices choices;
			}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getChoice()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(enum_explicit_overflow)
{
	char const* sourceCode = R"(
			contract test {
				enum ActionChoices { GoLeft, GoRight, GoStraight }
				constructor() public
				{
				}
				function getChoiceExp(uint x) public returns (uint d)
				{
					choice = ActionChoices(x);
					d = uint256(choice);
				}
				function getChoiceFromSigned(int x) public returns (uint d)
				{
					choice = ActionChoices(x);
					d = uint256(choice);
				}
				function getChoiceFromNegativeLiteral() public returns (uint d)
				{
					choice = ActionChoices(-1);
					d = uint256(choice);
				}
				ActionChoices choice;
			}
	)";
	compileAndRun(sourceCode);
	// These should throw
	ABI_CHECK(callContractFunction("getChoiceExp(uint256)", 3), encodeArgs());
	ABI_CHECK(callContractFunction("getChoiceFromSigned(int256)", -1), encodeArgs());
	ABI_CHECK(callContractFunction("getChoiceFromNegativeLiteral()"), encodeArgs());
	// These should work
	ABI_CHECK(callContractFunction("getChoiceExp(uint256)", 2), encodeArgs(2));
	ABI_CHECK(callContractFunction("getChoiceExp(uint256)", 0), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(storing_invalid_boolean)
{
	char const* sourceCode = R"(
		contract C {
			event Ev(bool);
			bool public perm;
			function set() public returns(uint) {
				bool tmp;
				assembly {
					tmp := 5
				}
				perm = tmp;
				return 1;
			}
			function ret() public returns(bool) {
				bool tmp;
				assembly {
					tmp := 5
				}
				return tmp;
			}
			function ev() public returns(uint) {
				bool tmp;
				assembly {
					tmp := 5
				}
				emit Ev(tmp);
				return 1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("set()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("perm()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("ret()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("ev()"), encodeArgs(1));
	BOOST_REQUIRE_EQUAL(m_logs.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].address, m_contractAddress);
	BOOST_CHECK(m_logs[0].data == encodeArgs(1));
	BOOST_REQUIRE_EQUAL(m_logs[0].topics.size(), 1);
	BOOST_CHECK_EQUAL(m_logs[0].topics[0], dev::keccak256(string("Ev(bool)")));
}


BOOST_AUTO_TEST_CASE(using_contract_enums_with_explicit_contract_name)
{
	char const* sourceCode = R"(
			contract test {
				enum Choice { A, B, C }
				function answer () public returns (test.Choice _ret)
				{
					_ret = test.Choice.B;
				}
			}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("answer()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(using_inherited_enum)
{
	char const* sourceCode = R"(
			contract base {
				enum Choice { A, B, C }
			}

			contract test is base {
				function answer () public returns (Choice _ret)
				{
					_ret = Choice.B;
				}
			}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("answer()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(using_inherited_enum_excplicitly)
{
	char const* sourceCode = R"(
			contract base {
				enum Choice { A, B, C }
			}

			contract test is base {
				function answer () public returns (base.Choice _ret)
				{
					_ret = base.Choice.B;
				}
			}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("answer()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(constructing_enums_from_ints)
{
	char const* sourceCode = R"(
			contract c {
				enum Truth { False, True }
				function test() public returns (uint)
				{
					return uint(Truth(uint8(0x701)));
				}
			}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1));
}

BOOST_AUTO_TEST_CASE(struct_referencing)
{
	static char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		interface I {
			struct S { uint a; }
		}
		library L {
			struct S { uint b; uint a; }
			function f() public pure returns (S memory) {
				S memory s;
				s.a = 3;
				return s;
			}
			function g() public pure returns (I.S memory) {
				I.S memory s;
				s.a = 4;
				return s;
			}
			// argument-dependant lookup tests
			function a(I.S memory) public pure returns (uint) { return 1; }
			function a(S memory) public pure returns (uint) { return 2; }
		}
		contract C is I {
			function f() public pure returns (S memory) {
				S memory s;
				s.a = 1;
				return s;
			}
			function g() public pure returns (I.S memory) {
				I.S memory s;
				s.a = 2;
				return s;
			}
			function h() public pure returns (L.S memory) {
				L.S memory s;
				s.a = 5;
				return s;
			}
			function x() public pure returns (L.S memory) {
				return L.f();
			}
			function y() public pure returns (I.S memory) {
				return L.g();
			}
			function a1() public pure returns (uint) { S memory s; return L.a(s); }
			function a2() public pure returns (uint) { L.S memory s; return L.a(s); }
		}
	)";
	compileAndRun(sourceCode, 0, "L");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(0, 3));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(4));
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{ {"L", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(2));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(0, 5));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(0, 3));
	ABI_CHECK(callContractFunction("y()"), encodeArgs(4));
	ABI_CHECK(callContractFunction("a1()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("a2()"), encodeArgs(2));
}

BOOST_AUTO_TEST_CASE(enum_referencing)
{
	char const* sourceCode = R"(
		interface I {
			enum Direction { A, B, Left, Right }
		}
		library L {
			enum Direction { Left, Right }
			function f() public pure returns (Direction) {
				return Direction.Right;
			}
			function g() public pure returns (I.Direction) {
				return I.Direction.Right;
			}
		}
		contract C is I {
			function f() public pure returns (Direction) {
				return Direction.Right;
			}
			function g() public pure returns (I.Direction) {
				return I.Direction.Right;
			}
			function h() public pure returns (L.Direction) {
				return L.Direction.Right;
			}
			function x() public pure returns (L.Direction) {
				return L.f();
			}
			function y() public pure returns (I.Direction) {
				return L.g();
			}
		}
	)";
	compileAndRun(sourceCode, 0, "L");
	ABI_CHECK(callContractFunction("f()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(3));
	compileAndRun(sourceCode, 0, "C", bytes(), map<string, Address>{{"L", m_contractAddress}});
	ABI_CHECK(callContractFunction("f()"), encodeArgs(3));
	ABI_CHECK(callContractFunction("g()"), encodeArgs(3));
	ABI_CHECK(callContractFunction("h()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("x()"), encodeArgs(1));
	ABI_CHECK(callContractFunction("y()"), encodeArgs(3));
}

BOOST_AUTO_TEST_CASE(inline_member_init)
{
	char const* sourceCode = R"(
		contract test {
			constructor() public {
				m_b = 6;
				m_c = 8;
			}
			uint m_a = 5;
			uint m_b;
			uint m_c = 7;
			function get() public returns (uint a, uint b, uint c){
				a = m_a;
				b = m_b;
				c = m_c;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("get()"), encodeArgs(5, 6, 8));
}

BOOST_AUTO_TEST_CASE(inline_member_init_inheritence)
{
	char const* sourceCode = R"(
		contract Base {
			constructor() public {}
			uint m_base = 5;
			function getBMember() public returns (uint i) { return m_base; }
		}
		contract Derived is Base {
			constructor() public {}
			uint m_derived = 6;
			function getDMember() public returns (uint i) { return m_derived; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getBMember()"), encodeArgs(5));
	ABI_CHECK(callContractFunction("getDMember()"), encodeArgs(6));
}

BOOST_AUTO_TEST_CASE(inline_member_init_inheritence_without_constructor)
{
	char const* sourceCode = R"(
		contract Base {
			uint m_base = 5;
			function getBMember() public returns (uint i) { return m_base; }
		}
		contract Derived is Base {
			uint m_derived = 6;
			function getDMember() public returns (uint i) { return m_derived; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getBMember()"), encodeArgs(5));
	ABI_CHECK(callContractFunction("getDMember()"), encodeArgs(6));
}

BOOST_AUTO_TEST_CASE(external_function)
{
	char const* sourceCode = R"(
		contract c {
			function f(uint a) public returns (uint) { return a; }
			function test(uint a, uint b) external returns (uint r_a, uint r_b) {
				r_a = f(a + 7);
				r_b = b;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test(uint256,uint256)", 2, 3), encodeArgs(2+7, 3));
}

BOOST_AUTO_TEST_CASE(bytes_in_arguments)
{
	char const* sourceCode = R"(
		contract c {
			uint result;
			function f(uint a, uint b) public { result += a + b; }
			function g(uint a) public { result *= a; }
			function test(uint a, bytes calldata data1, bytes calldata data2, uint b) external returns (uint r_a, uint r, uint r_b, uint l) {
				r_a = a;
				address(this).call(data1);
				address(this).call(data2);
				r = result;
				r_b = b;
				l = data1.length;
			}
		}
	)";
	compileAndRun(sourceCode);

	string innercalldata1 = asString(FixedHash<4>(dev::keccak256("f(uint256,uint256)")).asBytes() + encodeArgs(8, 9));
	string innercalldata2 = asString(FixedHash<4>(dev::keccak256("g(uint256)")).asBytes() + encodeArgs(3));
	bytes calldata = encodeArgs(
		12, 32 * 4, u256(32 * 4 + 32 + (innercalldata1.length() + 31) / 32 * 32), 13,
		u256(innercalldata1.length()), innercalldata1,
		u256(innercalldata2.length()), innercalldata2);
	ABI_CHECK(
		callContractFunction("test(uint256,bytes,bytes,uint256)", calldata),
		encodeArgs(12, (8 + 9) * 3, 13, u256(innercalldata1.length()))
	);
}

BOOST_AUTO_TEST_CASE(fixed_arrays_in_storage)
{
	char const* sourceCode = R"(
		contract c {
			struct Data { uint x; uint y; }
			Data[2**10] data;
			uint[2**10 + 3] ids;
			function setIDStatic(uint id) public { ids[2] = id; }
			function setID(uint index, uint id) public { ids[index] = id; }
			function setData(uint index, uint x, uint y) public { data[index].x = x; data[index].y = y; }
			function getID(uint index) public returns (uint) { return ids[index]; }
			function getData(uint index) public returns (uint x, uint y) { x = data[index].x; y = data[index].y; }
			function getLengths() public returns (uint l1, uint l2) { l1 = data.length; l2 = ids.length; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("setIDStatic(uint256)", 11), bytes());
	ABI_CHECK(callContractFunction("getID(uint256)", 2), encodeArgs(11));
	ABI_CHECK(callContractFunction("setID(uint256,uint256)", 7, 8), bytes());
	ABI_CHECK(callContractFunction("getID(uint256)", 7), encodeArgs(8));
	ABI_CHECK(callContractFunction("setData(uint256,uint256,uint256)", 7, 8, 9), bytes());
	ABI_CHECK(callContractFunction("setData(uint256,uint256,uint256)", 8, 10, 11), bytes());
	ABI_CHECK(callContractFunction("getData(uint256)", 7), encodeArgs(8, 9));
	ABI_CHECK(callContractFunction("getData(uint256)", 8), encodeArgs(10, 11));
	ABI_CHECK(callContractFunction("getLengths()"), encodeArgs(u256(1) << 10, (u256(1) << 10) + 3));
}

BOOST_AUTO_TEST_CASE(dynamic_arrays_in_storage)
{
	char const* sourceCode = R"(
		contract c {
			struct Data { uint x; uint y; }
			Data[] data;
			uint[] ids;
			function setIDStatic(uint id) public { ids[2] = id; }
			function setID(uint index, uint id) public { ids[index] = id; }
			function setData(uint index, uint x, uint y) public { data[index].x = x; data[index].y = y; }
			function getID(uint index) public returns (uint) { return ids[index]; }
			function getData(uint index) public returns (uint x, uint y) { x = data[index].x; y = data[index].y; }
			function getLengths() public returns (uint l1, uint l2) { l1 = data.length; l2 = ids.length; }
			function setLengths(uint l1, uint l2) public { data.length = l1; ids.length = l2; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("getLengths()"), encodeArgs(0, 0));
	ABI_CHECK(callContractFunction("setLengths(uint256,uint256)", 48, 49), bytes());
	ABI_CHECK(callContractFunction("getLengths()"), encodeArgs(48, 49));
	ABI_CHECK(callContractFunction("setIDStatic(uint256)", 11), bytes());
	ABI_CHECK(callContractFunction("getID(uint256)", 2), encodeArgs(11));
	ABI_CHECK(callContractFunction("setID(uint256,uint256)", 7, 8), bytes());
	ABI_CHECK(callContractFunction("getID(uint256)", 7), encodeArgs(8));
	ABI_CHECK(callContractFunction("setData(uint256,uint256,uint256)", 7, 8, 9), bytes());
	ABI_CHECK(callContractFunction("setData(uint256,uint256,uint256)", 8, 10, 11), bytes());
	ABI_CHECK(callContractFunction("getData(uint256)", 7), encodeArgs(8, 9));
	ABI_CHECK(callContractFunction("getData(uint256)", 8), encodeArgs(10, 11));
}

BOOST_AUTO_TEST_CASE(fixed_out_of_bounds_array_access)
{
	char const* sourceCode = R"(
		contract c {
			uint[4] data;
			function set(uint index, uint value) public returns (bool) { data[index] = value; return true; }
			function get(uint index) public returns (uint) { return data[index]; }
			function length() public returns (uint) { return data.length; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("length()"), encodeArgs(4));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", 3, 4), encodeArgs(true));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", 4, 5), bytes());
	ABI_CHECK(callContractFunction("set(uint256,uint256)", 400, 5), bytes());
	ABI_CHECK(callContractFunction("get(uint256)", 3), encodeArgs(4));
	ABI_CHECK(callContractFunction("get(uint256)", 4), bytes());
	ABI_CHECK(callContractFunction("get(uint256)", 400), bytes());
	ABI_CHECK(callContractFunction("length()"), encodeArgs(4));
}

BOOST_AUTO_TEST_CASE(dynamic_out_of_bounds_array_access)
{
	char const* sourceCode = R"(
		contract c {
			uint[] data;
			function enlarge(uint amount) public returns (uint) { return data.length += amount; }
			function set(uint index, uint value) public returns (bool) { data[index] = value; return true; }
			function get(uint index) public returns (uint) { return data[index]; }
			function length() public returns (uint) { return data.length; }
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("length()"), encodeArgs(0));
	ABI_CHECK(callContractFunction("get(uint256)", 3), bytes());
	ABI_CHECK(callContractFunction("enlarge(uint256)", 4), encodeArgs(4));
	ABI_CHECK(callContractFunction("length()"), encodeArgs(4));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", 3, 4), encodeArgs(true));
	ABI_CHECK(callContractFunction("get(uint256)", 3), encodeArgs(4));
	ABI_CHECK(callContractFunction("length()"), encodeArgs(4));
	ABI_CHECK(callContractFunction("set(uint256,uint256)", 4, 8), bytes());
	ABI_CHECK(callContractFunction("length()"), encodeArgs(4));
}

BOOST_AUTO_TEST_CASE(fixed_array_cleanup)
{
	char const* sourceCode = R"(
		contract c {
			uint spacer1;
			uint spacer2;
			uint[20] data;
			function fill() public {
				for (uint i = 0; i < data.length; ++i) data[i] = i+1;
			}
			function clear() public { delete data; }
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("fill()"), bytes());
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("clear()"), bytes());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(short_fixed_array_cleanup)
{
	char const* sourceCode = R"(
		contract c {
			uint spacer1;
			uint spacer2;
			uint[3] data;
			function fill() public {
				for (uint i = 0; i < data.length; ++i) data[i] = i+1;
			}
			function clear() public { delete data; }
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("fill()"), bytes());
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("clear()"), bytes());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(dynamic_array_cleanup)
{
	char const* sourceCode = R"(
		contract c {
			uint[20] spacer;
			uint[] dynamic;
			function fill() public {
				dynamic.length = 21;
				for (uint i = 0; i < dynamic.length; ++i) dynamic[i] = i+1;
			}
			function halfClear() public { dynamic.length = 5; }
			function fullClear() public { delete dynamic; }
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("fill()"), bytes());
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("halfClear()"), bytes());
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("fullClear()"), bytes());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(dynamic_multi_array_cleanup)
{
	char const* sourceCode = R"(
		contract c {
			struct s { uint[][] d; }
			s[] data;
			function fill() public returns (uint) {
				data.length = 3;
				data[2].d.length = 4;
				data[2].d[3].length = 5;
				data[2].d[3][4] = 8;
				return data[2].d[3][4];
			}
			function clear() public { delete data; }
		}
	)";
	compileAndRun(sourceCode);
	BOOST_CHECK(storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("fill()"), encodeArgs(8));
	BOOST_CHECK(!storageEmpty(m_contractAddress));
	ABI_CHECK(callContractFunction("clear()"), bytes());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(array_copy_storage_storage_dyn_dyn)
{
	char const* sourceCode = R"(
		contract c {
			uint[] data1;
			uint[] data2;
			function setData1(uint length, uint index, uint value) public {
				data1.length = length; if (index < length) data1[index] = value;
			}
			function copyStorageStorage() public { data2 = data1; }
			function getData2(uint index) public returns (uint len, uint val) {
				len = data2.length; if (index < len) val = data2[index];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("setData1(uint256,uint256,uint256)", 10, 5, 4), bytes());
	ABI_CHECK(callContractFunction("copyStorageStorage()"), bytes());
	ABI_CHECK(callContractFunction("getData2(uint256)", 5), encodeArgs(10, 4));
	ABI_CHECK(callContractFunction("setData1(uint256,uint256,uint256)", 0, 0, 0), bytes());
	ABI_CHECK(callContractFunction("copyStorageStorage()"), bytes());
	ABI_CHECK(callContractFunction("getData2(uint256)", 0), encodeArgs(0, 0));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(array_copy_storage_storage_static_static)
{
	char const* sourceCode = R"(
		contract c {
			uint[40] data1;
			uint[20] data2;
			function test() public returns (uint x, uint y){
				data1[30] = 4;
				data1[2] = 7;
				data1[3] = 9;
				data2[3] = 8;
				data1 = data2;
				x = data1[3];
				y = data1[30]; // should be cleared
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(8, 0));
}

BOOST_AUTO_TEST_CASE(array_copy_storage_storage_static_dynamic)
{
	char const* sourceCode = R"(
		contract c {
			uint[9] data1;
			uint[] data2;
			function test() public returns (uint x, uint y){
				data1[8] = 4;
				data2 = data1;
				x = data2.length;
				y = data2[8];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(9, 4));
}

BOOST_AUTO_TEST_CASE(array_copy_different_packing)
{
	char const* sourceCode = R"(
		contract c {
			bytes8[] data1; // 4 per slot
			bytes10[] data2; // 3 per slot
			function test() public returns (bytes10 a, bytes10 b, bytes10 c, bytes10 d, bytes10 e) {
				data1.length = 9;
				for (uint i = 0; i < data1.length; ++i)
					data1[i] = bytes8(uint64(i));
				data2 = data1;
				a = data2[1];
				b = data2[2];
				c = data2[3];
				d = data2[4];
				e = data2[5];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(
		asString(fromHex("0000000000000001")),
		asString(fromHex("0000000000000002")),
		asString(fromHex("0000000000000003")),
		asString(fromHex("0000000000000004")),
		asString(fromHex("0000000000000005"))
	));
}

BOOST_AUTO_TEST_CASE(array_copy_target_simple)
{
	char const* sourceCode = R"(
		contract c {
			bytes8[9] data1; // 4 per slot
			bytes17[10] data2; // 1 per slot, no offset counter
			function test() public returns (bytes17 a, bytes17 b, bytes17 c, bytes17 d, bytes17 e) {
				for (uint i = 0; i < data1.length; ++i)
					data1[i] = bytes8(uint64(i));
				data2[8] = data2[9] = bytes8(uint64(2));
				data2 = data1;
				a = data2[1];
				b = data2[2];
				c = data2[3];
				d = data2[4];
				e = data2[9];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(
		asString(fromHex("0000000000000001")),
		asString(fromHex("0000000000000002")),
		asString(fromHex("0000000000000003")),
		asString(fromHex("0000000000000004")),
		asString(fromHex("0000000000000000"))
	));
}

BOOST_AUTO_TEST_CASE(array_copy_target_leftover)
{
	// test that leftover elements in the last slot of target are correctly cleared during assignment
	char const* sourceCode = R"(
		contract c {
			byte[10] data1;
			bytes2[32] data2;
			function test() public returns (uint check, uint res1, uint res2) {
				uint i;
				for (i = 0; i < data2.length; ++i)
					data2[i] = 0xffff;
				check = uint(uint16(data2[31])) * 0x10000 | uint(uint16(data2[14]));
				for (i = 0; i < data1.length; ++i)
					data1[i] = byte(uint8(1 + i));
				data2 = data1;
				for (i = 0; i < 16; ++i)
					res1 |= uint(uint16(data2[i])) * 0x10000**i;
				for (i = 0; i < 16; ++i)
					res2 |= uint(uint16(data2[16 + i])) * 0x10000**i;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(
		u256("0xffffffff"),
		asString(fromHex("0000000000000000""000000000a000900""0800070006000500""0400030002000100")),
		asString(fromHex("0000000000000000""0000000000000000""0000000000000000""0000000000000000"))
	));
}

BOOST_AUTO_TEST_CASE(array_copy_target_leftover2)
{
	// since the copy always copies whole slots, we have to make sure that the source size maxes
	// out a whole slot and at the same time there are still elements left in the target at that point
	char const* sourceCode = R"(
		contract c {
			bytes8[4] data1; // fits into one slot
			bytes10[6] data2; // 4 elements need two slots
			function test() public returns (bytes10 r1, bytes10 r2, bytes10 r3) {
				data1[0] = bytes8(uint64(1));
				data1[1] = bytes8(uint64(2));
				data1[2] = bytes8(uint64(3));
				data1[3] = bytes8(uint64(4));
				for (uint i = 0; i < data2.length; ++i)
					data2[i] = bytes10(uint80(0xffff00 | (1 + i)));
				data2 = data1;
				r1 = data2[3];
				r2 = data2[4];
				r3 = data2[5];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(
		asString(fromHex("0000000000000004")),
		asString(fromHex("0000000000000000")),
		asString(fromHex("0000000000000000"))
	));
}

BOOST_AUTO_TEST_CASE(array_copy_storage_storage_struct)
{
	char const* sourceCode = R"(
		contract c {
			struct Data { uint x; uint y; }
			Data[] data1;
			Data[] data2;
			function test() public returns (uint x, uint y) {
				data1.length = 9;
				data1[8].x = 4;
				data1[8].y = 5;
				data2 = data1;
				x = data2[8].x;
				y = data2[8].y;
				data1.length = 0;
				data2 = data1;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(4, 5));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(array_copy_storage_abi)
{
	// NOTE: This does not really test copying from storage to ABI directly,
	// because it will always copy to memory first.
	char const* sourceCode = R"(
		pragma experimental ABIEncoderV2;
		contract c {
			uint8[] x;
			uint16[] y;
			uint24[] z;
			uint24[][] w;
			function test1() public returns (uint8[] memory) {
				for (uint i = 0; i < 101; ++i)
					x.push(uint8(i));
				return x;
			}
			function test2() public returns (uint16[] memory) {
				for (uint i = 0; i < 101; ++i)
					y.push(uint16(i));
				return y;
			}
			function test3() public returns (uint24[] memory) {
				for (uint i = 0; i < 101; ++i)
					z.push(uint24(i));
				return z;
			}
			function test4() public returns (uint24[][] memory) {
				w.length = 5;
				for (uint i = 0; i < 5; ++i)
					for (uint j = 0; j < 101; ++j)
						w[i].push(uint24(j));
				return w;
			}
		}
	)";
	compileAndRun(sourceCode);
	bytes valueSequence;
	for (size_t i = 0; i < 101; ++i)
		valueSequence += toBigEndian(u256(i));
	ABI_CHECK(callContractFunction("test1()"), encodeArgs(0x20, 101) + valueSequence);
	ABI_CHECK(callContractFunction("test2()"), encodeArgs(0x20, 101) + valueSequence);
	ABI_CHECK(callContractFunction("test3()"), encodeArgs(0x20, 101) + valueSequence);
	ABI_CHECK(callContractFunction("test4()"),
		encodeArgs(0x20, 5, 0xa0, 0xa0 + 102 * 32 * 1, 0xa0 + 102 * 32 * 2, 0xa0 + 102 * 32 * 3, 0xa0 + 102 * 32 * 4) +
		encodeArgs(101) + valueSequence +
		encodeArgs(101) + valueSequence +
		encodeArgs(101) + valueSequence +
		encodeArgs(101) + valueSequence +
		encodeArgs(101) + valueSequence
	);
}

BOOST_AUTO_TEST_CASE(array_copy_storage_abi_signed)
{
	// NOTE: This does not really test copying from storage to ABI directly,
	// because it will always copy to memory first.
	char const* sourceCode = R"(
		contract c {
			int16[] x;
			function test() public returns (int16[] memory) {
				x.push(int16(-1));
				x.push(int16(-1));
				x.push(int16(8));
				x.push(int16(-16));
				x.push(int16(-2));
				x.push(int16(6));
				x.push(int16(8));
				x.push(int16(-1));
				return x;
			}
		}
	)";
	compileAndRun(sourceCode);
	bytes valueSequence;
	ABI_CHECK(callContractFunction("test()"), encodeArgs(0x20, 8,
		u256(-1),
		u256(-1),
		u256(8),
		u256(-16),
		u256(-2),
		u256(6),
		u256(8),
		u256(-1)
	));
}

BOOST_AUTO_TEST_CASE(array_push)
{
	char const* sourceCode = R"(
		contract c {
			uint[] data;
			function test() public returns (uint x, uint y, uint z, uint l) {
				data.push(5);
				x = data[0];
				data.push(4);
				y = data[1];
				l = data.push(3);
				z = data[2];
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(5, 4, 3, 3));
}

BOOST_AUTO_TEST_CASE(array_push_struct)
{
	char const* sourceCode = R"(
		contract c {
			struct S { uint16 a; uint16 b; uint16[3] c; uint16[] d; }
			S[] data;
			function test() public returns (uint16, uint16, uint16, uint16) {
				S memory s;
				s.a = 2;
				s.b = 3;
				s.c[2] = 4;
				s.d = new uint16[](4);
				s.d[2] = 5;
				data.push(s);
				return (data[0].a, data[0].b, data[0].c[2], data[0].d[2]);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(2, 3, 4, 5));
}

BOOST_AUTO_TEST_CASE(array_push_packed_array)
{
	char const* sourceCode = R"(
		contract c {
			uint80[] x;
			function test() public returns (uint80, uint80, uint80, uint80) {
				x.push(1);
				x.push(2);
				x.push(3);
				x.push(4);
				x.push(5);
				x.length = 4;
				return (x[0], x[1], x[2], x[3]);
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1, 2, 3, 4));
}

BOOST_AUTO_TEST_CASE(byte_array_push)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test() public returns (bool x) {
				if (data.push(0x05) != 1)  return true;
				if (data[0] != 0x05) return true;
				data.push(0x04);
				if (data[1] != 0x04) return true;
				uint l = data.push(0x03);
				if (data[2] != 0x03) return true;
				if (l != 0x03) return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(false));
}

BOOST_AUTO_TEST_CASE(byte_array_push_transition)
{
	// Tests transition between short and long encoding
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test() public returns (uint) {
				for (uint8 i = 1; i < 40; i++)
				{
					data.push(byte(i));
					if (data.length != i) return 0x1000 + i;
					if (data[data.length - 1] != byte(i)) return i;
				}
				for (uint8 i = 1; i < 40; i++)
					if (data[i - 1] != byte(i)) return 0x1000000 + i;
				return 0;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(0));
}

BOOST_AUTO_TEST_CASE(array_pop)
{
	char const* sourceCode = R"(
		contract c {
			uint[] data;
			function test() public returns (uint x, uint l) {
				data.push(7);
				x = data.push(3);
				data.pop();
				x = data.length;
				data.pop();
				l = data.length;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1, 0));
}

BOOST_AUTO_TEST_CASE(array_pop_uint16_transition)
{
	char const* sourceCode = R"(
		contract c {
			uint16[] data;
			function test() public returns (uint16 x, uint16 y, uint16 z) {
				for (uint i = 1; i <= 48; i++)
					data.push(uint16(i));
				for (uint j = 1; j <= 10; j++)
					data.pop();
				x = data[data.length - 1];
				for (uint k = 1; k <= 10; k++)
					data.pop();
				y = data[data.length - 1];
				for (uint l = 1; l <= 10; l++)
					data.pop();
				z = data[data.length - 1];
				for (uint m = 1; m <= 18; m++)
					data.pop();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(38, 28, 18));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(array_pop_uint24_transition)
{
	char const* sourceCode = R"(
		contract c {
			uint256 a;
			uint256 b;
			uint256 c;
			uint24[] data;
			function test() public returns (uint24 x, uint24 y) {
				for (uint i = 1; i <= 30; i++)
					data.push(uint24(i));
				for (uint j = 1; j <= 10; j++)
					data.pop();
				x = data[data.length - 1];
				for (uint k = 1; k <= 10; k++)
					data.pop();
				y = data[data.length - 1];
				for (uint l = 1; l <= 10; l++)
					data.pop();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(20, 10));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(array_pop_array_transition)
{
	char const* sourceCode = R"(
		contract c {
			uint256 a;
			uint256 b;
			uint256 c;
			uint16[] inner = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
			uint16[][] data;
			function test() public returns (uint x, uint y, uint z) {
				for (uint i = 1; i <= 48; i++)
					data.push(inner);
				for (uint j = 1; j <= 10; j++)
					data.pop();
				x = data[data.length - 1][0];
				for (uint k = 1; k <= 10; k++)
					data.pop();
				y = data[data.length - 1][1];
				for (uint l = 1; l <= 10; l++)
					data.pop();
				z = data[data.length - 1][2];
				for (uint m = 1; m <= 18; m++)
					data.pop();
				delete inner;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(1, 2, 3));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(array_pop_empty_exception)
{
	char const* sourceCode = R"(
		contract c {
			uint[] data;
			function test() public returns (bool) {
				data.pop();
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(array_pop_storage_empty)
{
	char const* sourceCode = R"(
		contract c {
			uint[] data;
			function test() public {
				data.push(7);
				data.pop();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(byte_array_pop)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test() public returns (uint x, uint y, uint l) {
				data.push(0x07);
				x = data.push(0x03);
				data.pop();
				data.pop();
				y = data.push(0x02);
				l = data.length;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(2, 1, 1));
}

BOOST_AUTO_TEST_CASE(byte_array_pop_empty_exception)
{
	char const* sourceCode = R"(
		contract c {
			uint256 a;
			uint256 b;
			uint256 c;
			bytes data;
			function test() public returns (bool) {
				data.pop();
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
}

BOOST_AUTO_TEST_CASE(byte_array_pop_storage_empty)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test() public {
				data.push(0x07);
				data.push(0x05);
				data.push(0x03);
				data.pop();
				data.pop();
				data.pop();
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(byte_array_pop_long_storage_empty)
{
	char const* sourceCode = R"(
		contract c {
			uint256 a;
			uint256 b;
			uint256 c;
			bytes data;
			function test() public returns (bool) {
				for (uint8 i = 0; i <= 40; i++)
					data.push(byte(i+1));
				for (int8 j = 40; j >= 0; j--) {
					require(data[uint8(j)] == byte(j+1));
					require(data.length == uint8(j+1));
					data.pop();
				}
				return true;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(true));
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(byte_array_pop_long_storage_empty_garbage_ref)
{
	char const* sourceCode = R"(
		contract c {
			uint256 a;
			uint256 b;
			bytes data;
			function test() public {
				for (uint8 i = 0; i <= 40; i++)
					data.push(0x03);
				for (uint8 j = 0; j <= 40; j++) {
					assembly {
						mstore(0, "garbage")
					}
					data.pop();
				}
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs());
	BOOST_CHECK(storageEmpty(m_contractAddress));
}

BOOST_AUTO_TEST_CASE(byte_array_pop_masking_long)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test() public returns (bytes memory) {
				for (uint i = 0; i < 34; i++)
					data.push(0x03);
				data.pop();
				return data;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(
		u256(0x20),
		u256(33),
		asString(fromHex("0303030303030303030303030303030303030303030303030303030303030303")),
		asString(fromHex("03"))
	));
}

BOOST_AUTO_TEST_CASE(byte_array_pop_copy_long)
{
	char const* sourceCode = R"(
		contract c {
			bytes data;
			function test() public returns (bytes memory) {
				for (uint i = 0; i < 33; i++)
					data.push(0x03);
				for (uint j = 0; j < 4; j++)
					data.pop();
				return data;
			}
		}
	)";
	compileAndRun(sourceCode);
	ABI_CHECK(callContractFunction("test()"), encodeArgs(
		u256(0x20),
		u256(29),
		asString(fromHex("0303030303030303030303030303030303030303030303030303030303"))
	));
}

BOOST_AUTO_TEST_CASE(array_pop_isolated)
{
	char const* sourceCode = R"(
		// This tests that the compiler knows the correct size of the function on the stack.
		contract c {
			uint[] data;
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

BOOST_AUTO_TEST_SUITE_END()

}
}
} // end namespaces
