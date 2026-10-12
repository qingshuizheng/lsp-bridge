"""Tests for the Solidity helpers that work without a language server.

`core/handler/solidity.py` works out the definitions a Solidity server does not
answer itself, by reading the sources of the project.  The part of it that is a
question about a piece of text - is this a field declaration, what type is this
variable, what does this import bring in, is this inside an assembly block - is
asked here directly, with the text as the input and no project on disk.
"""

import os
import tempfile
import unittest
from unittest import mock

from core.handler.solidity import (
    _detect_solidity_project_root,
    _find_struct_field_decl_pos_in_line,
    _infer_variable_type_from_lines,
    _is_inside_inline_assembly,
    _normalize_solidity_type_name,
    _parse_all_imports,
)


class NormalizeTypeNameTest(unittest.TestCase):
    """`uint' and `int' are the aliases of the 256 bit types."""

    def test_the_aliases(self):
        self.assertEqual(_normalize_solidity_type_name("uint"), "uint256")
        self.assertEqual(_normalize_solidity_type_name("int"), "int256")

    def test_everything_else_is_left_alone(self):
        for name in ("uint8", "uint256", "int128", "address", "bytes32", "MyStruct"):
            self.assertEqual(_normalize_solidity_type_name(name), name)


class FindStructFieldDeclPosTest(unittest.TestCase):
    """Where a field is declared in a line, and when it is not."""

    def test_a_plain_declaration(self):
        self.assertEqual(_find_struct_field_decl_pos_in_line("    uint256 amount;", "amount"), 12)

    def test_a_mapping_declaration(self):
        line = "    mapping(address => uint256) balances;"
        self.assertEqual(_find_struct_field_decl_pos_in_line(line, "balances"),
                         line.index("balances"))

    def test_a_declaration_with_a_trailing_comment(self):
        line = "    address owner; // who owns it"
        self.assertEqual(_find_struct_field_decl_pos_in_line(line, "owner"), line.index("owner"))

    def test_comment_lines_have_no_declaration(self):
        for line in ("// uint256 amount;", "/* uint256 amount;", " * uint256 amount;", ""):
            self.assertIsNone(_find_struct_field_decl_pos_in_line(line, "amount"))

    def test_a_field_that_is_not_there(self):
        self.assertIsNone(_find_struct_field_decl_pos_in_line("    uint256 amount;", "total"))
        self.assertIsNone(_find_struct_field_decl_pos_in_line("    uint256 amount;", ""))


class ParseAllImportsTest(unittest.TestCase):
    """The imports of a source, in both of the shapes Solidity allows."""

    def test_a_simple_import(self):
        self.assertEqual(_parse_all_imports('import "path.sol";'), [(None, "path.sol")])

    def test_a_named_import(self):
        self.assertEqual(_parse_all_imports('import { A, B as C } from "p.sol";'),
                         [([("A", "A"), ("B", "C")], "p.sol")])

    def test_a_named_import_over_several_lines(self):
        content = 'import {\n    A,\n    B as C\n} from "p.sol";'
        self.assertEqual(_parse_all_imports(content), [([("A", "A"), ("B", "C")], "p.sol")])

    def test_several_imports(self):
        # The named imports are collected in one pass and the simple ones in
        # another, so the list is in that order and not in the order of the file.
        content = 'pragma solidity ^0.8.0;\nimport "a.sol";\nimport {B} from "b.sol";\n'
        self.assertEqual(_parse_all_imports(content),
                         [([("B", "B")], "b.sol"), (None, "a.sol")])

    def test_no_imports(self):
        self.assertEqual(_parse_all_imports("pragma solidity ^0.8.0;"), [])


class InferVariableTypeTest(unittest.TestCase):
    """The type of a variable, read from the declarations around it."""

    def test_a_simple_declaration(self):
        self.assertEqual(_infer_variable_type_from_lines(["uint256 amount;"], "amount"), "uint256")

    def test_a_mapping_gives_the_value_type(self):
        self.assertEqual(
            _infer_variable_type_from_lines(["mapping(address => uint256) balances;"], "balances"),
            "uint256")

    def test_an_array_gives_the_element_type(self):
        # The brackets are kept in what is inferred, which is what the callers
        # compare against the declarations they read.
        self.assertEqual(_infer_variable_type_from_lines(["address[] owners;"], "owners"),
                         "address[]")

    def test_comments_are_not_declarations(self):
        self.assertIsNone(_infer_variable_type_from_lines(["// uint256 amount;"], "amount"))

    def test_a_name_that_is_not_declared(self):
        self.assertIsNone(_infer_variable_type_from_lines(["uint256 amount;"], "total"))

    def test_no_name_at_all(self):
        self.assertIsNone(_infer_variable_type_from_lines(["uint256 amount;"], ""))


class IsInsideInlineAssemblyTest(unittest.TestCase):
    """The braces of an assembly block, and what is inside them."""

    def test_inside_and_outside(self):
        content = "assembly { let x := 1 }"
        self.assertTrue(_is_inside_inline_assembly(content, content.index("let")))
        self.assertFalse(_is_inside_inline_assembly(content, content.index("assembly")))

    def test_the_first_character_is_never_inside(self):
        self.assertFalse(_is_inside_inline_assembly("assembly { }", 0))

    def test_an_assembly_word_does_not_open_a_block(self):
        self.assertFalse(_is_inside_inline_assembly("uint256 assembly_like;", 10))


class DetectProjectRootTest(unittest.TestCase):
    """The root of a project is the closest directory carrying its markers."""

    def test_a_foundry_project(self):
        with tempfile.TemporaryDirectory() as root:
            os.makedirs(os.path.join(root, "src", "deep"))
            with open(os.path.join(root, "foundry.toml"), "w") as marker:
                marker.write("[profile.default]\n")
            self.assertEqual(_detect_solidity_project_root(os.path.join(root, "src", "deep")),
                             os.path.abspath(root))

    def test_a_directory_without_markers_is_its_own_root(self):
        with tempfile.TemporaryDirectory() as root:
            with mock.patch("core.handler.solidity.os.path.exists", return_value=False):
                self.assertEqual(_detect_solidity_project_root(root), os.path.abspath(root))


if __name__ == "__main__":
    unittest.main()
