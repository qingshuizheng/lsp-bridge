"""Tests for the Solidity helpers that work without a language server.

`core/handler/solidity.py` works out the definitions a Solidity server does not
answer itself, by reading the sources of the project.  The part of it that is a
question about a piece of text - is this a field declaration, what type is this
variable, what does this import bring in, is this inside an assembly block - is
asked here directly, with the text as the input and no project on disk.
"""

import os
import shutil
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
    _get_member_access_receiver,
    _is_solidity_builtin_member_access,
    _get_solidity_call_arity_at_cursor,
    _read_solidity_remappings,
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


class MemberAccessTest(unittest.TestCase):
    """Reading the receiver of `receiver.symbol` from the text of a file."""

    def file_with(self, text):
        """Return the path of a file holding TEXT."""
        handle, path = tempfile.mkstemp(prefix="lsp-bridge-solidity", suffix=".sol")
        with os.fdopen(handle, "w") as f:
            f.write(text)
        self.addCleanup(os.unlink, path)
        return path

    def test_the_receiver_of_a_member_access(self):
        path = self.file_with("contract C {\n  foo.bar();\n}\n")
        # The cursor is on `bar`, which is on line 1 (counted from nought).
        self.assertEqual(_get_member_access_receiver(path, {"line": 1, "character": 7}), "foo")

    def test_spaces_around_the_dot_are_skipped(self):
        path = self.file_with("  foo  .  bar()\n")
        self.assertEqual(_get_member_access_receiver(path, {"line": 0, "character": 11}), "foo")

    def test_a_symbol_with_no_receiver(self):
        path = self.file_with("  bar();\n")
        self.assertIsNone(_get_member_access_receiver(path, {"line": 0, "character": 4}))

    def test_a_line_that_is_not_there(self):
        path = self.file_with("  foo.bar()\n")
        self.assertIsNone(_get_member_access_receiver(path, {"line": 9, "character": 0}))

    def test_the_builtin_namespaces(self):
        # The namespaces Solidity itself provides, each of which had to be defined
        # before this function could answer at all.
        for namespace in ("msg", "block", "tx", "abi"):
            line = "  {}.sender".format(namespace)
            path = self.file_with(line + "\n")
            # The cursor is on , which begins after the namespace and its dot.
            self.assertTrue(_is_solidity_builtin_member_access(
                path, {"line": 0, "character": len(namespace) + 3}), namespace)
        other = self.file_with("  myToken.sender\n")
        self.assertFalse(_is_solidity_builtin_member_access(other, {"line": 0, "character": 10}))


class CallArityTest(unittest.TestCase):
    """Counting the arguments of the call the cursor is in."""

    def file_with(self, text):
        """Return the path of a file holding TEXT."""
        handle, path = tempfile.mkstemp(prefix="lsp-bridge-solidity-arity", suffix=".sol")
        with os.fdopen(handle, "w") as f:
            f.write(text)
        self.addCleanup(os.unlink, path)
        return path

    def arity(self, text, character=2):
        """Return the arity of the call `foo` on the first line of TEXT."""
        return _get_solidity_call_arity_at_cursor(self.file_with(text),
                                                  {"line": 0, "character": character}, "foo")

    def test_a_call_with_no_arguments(self):
        self.assertEqual(self.arity("  foo()\n"), 0)

    def test_a_call_with_one_argument(self):
        self.assertEqual(self.arity("  foo(a)\n"), 1)

    def test_a_call_with_arguments_over_more_than_one_line(self):
        self.assertEqual(self.arity("  foo(a, bar(b, c))\n"), 2)

    def test_a_symbol_that_is_not_called(self):
        self.assertIsNone(self.arity("  foo;\n"))


class RemappingsTest(unittest.TestCase):
    """Reading remappings.txt, which is a file of its own conventions."""

    def project_with(self, text):
        """Return the path of a project directory holding REMAPPINGS.TXT as TEXT."""
        path = tempfile.mkdtemp(prefix="lsp-bridge-project")
        self.addCleanup(shutil.rmtree, path, True)
        with open(os.path.join(path, "remappings.txt"), "w") as f:
            f.write(text)
        return path

    def test_the_pairs_are_read_in_order(self):
        project = self.project_with("@openzeppelin/=lib/openzeppelin/\n"
                                    "solmate/=lib/solmate/src/\n")
        self.assertEqual(_read_solidity_remappings(project),
                         [("@openzeppelin/", "lib/openzeppelin/"),
                          ("solmate/", "lib/solmate/src/")])

    def test_comments_blank_lines_and_lines_without_a_pair_are_skipped(self):
        project = self.project_with("# a comment\n"
                                    "\n"
                                    "not a remapping\n"
                                    "@oz/=lib/oz/\n")
        self.assertEqual(_read_solidity_remappings(project), [("@oz/", "lib/oz/")])

    def test_the_context_prefix_and_the_quotes_are_taken_off(self):
        project = self.project_with("ctx:@oz/='lib/oz/'\n")
        self.assertEqual(_read_solidity_remappings(project), [("@oz/", "lib/oz/")])

    def test_a_project_with_no_remappings_file(self):
        path = tempfile.mkdtemp(prefix="lsp-bridge-project-none")
        self.addCleanup(shutil.rmtree, path, True)
        self.assertEqual(_read_solidity_remappings(path), [])

if __name__ == "__main__":
    unittest.main()
