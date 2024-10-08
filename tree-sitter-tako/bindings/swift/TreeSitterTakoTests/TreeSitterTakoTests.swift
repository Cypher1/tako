import XCTest
import SwiftTreeSitter
import TreeSitterTako

final class TreeSitterTakoTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_tako())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Tako grammar")
    }
}
