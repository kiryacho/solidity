contract C {
    function short_dyn() public pure returns (string memory x) {
        x = "abc";
    }
    function long_dyn() public pure returns (string memory x) {
        x = "12345678901234567890123456789012345678901234567890123456789012345678901234567890";
    }
    function bytesNN() public pure returns (bytes3 x) {
        x = "abc";
    }
    function bytesNN_padded() public pure returns (bytes4 x) {
        x = "abc";
    }
}
// ====
// compileViaYul: true
// ----
// short_dyn(): -> 
// long_dyn(): -> 
// bytesNN(): -> 0x6162630000000000000000000000000000000000000000000000000000000000
// bytesNN_padded(): -> 0x6162630000000000000000000000000000000000000000000000000000000000
