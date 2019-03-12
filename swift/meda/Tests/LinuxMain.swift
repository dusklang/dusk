import XCTest

import medaTests

var tests = [XCTestCaseEntry]()
tests += medaTests.allTests()
XCTMain(tests)