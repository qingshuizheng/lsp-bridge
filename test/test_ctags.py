"""Tests for the ctags helpers that work without running readtags.

`core/ctags.py` answers completion and jump-to-definition by asking `readtags`
about the `tags` file of a project.  Everything around that call is text: the line
of a tags file and the fields it carries, the annotation shown next to a result,
the candidate the menu is given, the command line that is built, and the walk up
the directories that finds the tags file in the first place.  All of that is asked
here.  What is not here is the call itself and the completion built from its
answer, which need a project and its tags file.
"""

import os
import tempfile
import unittest

from core.ctags import Ctags, remove_prefix


def make_ctags():
    """Return a Ctags instance for the pure helpers that do not use its state."""
    return Ctags()


class RemovePrefixTest(unittest.TestCase):
    """The stand-in for `str.removeprefix' on older Pythons."""

    def test_a_prefix_that_is_there(self):
        self.assertEqual(remove_prefix("typename:MyType", "typename:"), "MyType")

    def test_a_prefix_that_is_not_there(self):
        self.assertEqual(remove_prefix("MyType", "typename:"), "MyType")


class ParseTagLineTest(unittest.TestCase):
    """One line of a tags file becomes the fields it carries."""

    def test_the_three_normal_fields(self):
        tag = make_ctags().parse_tag_line("myFunction\t/tmp/a.py\t/^def myFunction($/;\"")
        self.assertEqual(tag["tagname"], "myFunction")
        self.assertEqual(tag["tagfile"], "/tmp/a.py")
        self.assertEqual(tag["tagaddress"], "/^def myFunction($/")

    def test_extension_fields(self):
        line = 'myFunction\t/tmp/a.py\t/^def myFunction($/;"\tkind:function\tsignature:()\tscope:MyClass'
        tag = make_ctags().parse_tag_line(line)
        self.assertEqual(tag["kind"], "function")
        self.assertEqual(tag["signature"], "()")
        self.assertEqual(tag["scope"], "MyClass")

    def test_a_field_without_a_name_is_the_kind(self):
        # readtags writes the kind on its own in its compact form, and it arrives
        # as the one-element list the parser makes of a field with no colon.
        tag = make_ctags().parse_tag_line('myFunction\t/tmp/a.py\t/^def myFunction($/;"\tfunction')
        self.assertEqual(tag["kind"], ["function"])

    def test_an_empty_line_is_none(self):
        self.assertIsNone(make_ctags().parse_tag_line(""))


class MakeTagAnnotationTest(unittest.TestCase):
    """The annotation names the kind, the type and the scope."""

    def test_a_plain_tag(self):
        self.assertEqual(make_ctags().make_tag_annotation({"kind": "function"}), "function")

    def test_kind_type_and_scope(self):
        annotation = make_ctags().make_tag_annotation(
            {"kind": "function", "typeref": "typename:MyType", "scope": "MyClass"})
        self.assertEqual(annotation, "function/MyType@MyClass")

    def test_a_reference_is_marked(self):
        self.assertEqual(make_ctags().make_tag_annotation(
            {"kind": "function", "extras": "reference"}), "<R>function")

    def test_anonymous_types_are_shortened(self):
        self.assertEqual(make_ctags().make_tag_annotation(
            {"kind": "function", "typeref": "typename:__anon1a2b"}), "function/__anon")

    def test_a_tag_with_nothing_in_it(self):
        self.assertEqual(make_ctags().make_tag_annotation({}), "")


class MakeCtagsAcmCandidateTest(unittest.TestCase):
    """The candidate the menu is given, from the tag."""

    def test_the_fields_of_a_candidate(self):
        candidate = make_ctags().make_ctags_acm_candidate(
            {"tagname": "myFunction", "kind": "function", "scope": "MyClass"})
        self.assertEqual(candidate["key"], "myFunction")
        self.assertEqual(candidate["label"], "myFunction")
        self.assertEqual(candidate["displayLabel"], "myFunction")
        self.assertEqual(candidate["icon"], "function")
        self.assertEqual(candidate["annotation"], "function@MyClass")
        self.assertEqual(candidate["backend"], "ctags")

    def test_the_icon_of_a_tag_without_a_kind(self):
        candidate = make_ctags().make_ctags_acm_candidate({"tagname": "something"})
        self.assertEqual(candidate["icon"], "NOKIND")


class ReadtagsGetCmdTest(unittest.TestCase):
    """The command line of a query, and what its flags mean."""

    def setUp(self):
        self.ctags = make_ctags()

    def test_a_prefix_query_without_case_folding(self):
        cmd = self.ctags.readtags_get_cmd("/tmp/tags", "sym", "prefix", False, None, None, None)
        self.assertEqual(cmd[0], "readtags")
        self.assertIn("-t", cmd)
        self.assertIn("/tmp/tags", cmd)
        # `p' asks for prefix matching; `i' would ask to fold case.
        self.assertIn("-Enep", cmd)
        self.assertIn("sym", cmd)

    def test_an_exact_query_with_case_folding(self):
        cmd = self.ctags.readtags_get_cmd("/tmp/tags", "sym", "exact", True, None, None, None)
        self.assertIn("-Enei", cmd)
        self.assertNotIn("-Enep", cmd)

    def test_the_filter_and_the_sorter(self):
        cmd = self.ctags.readtags_get_cmd("/tmp/tags", "sym", "exact", False, "(filter)", "(sort)", None)
        self.assertIn("(filter)", cmd)
        self.assertIn("(sort)", cmd)

    def test_the_name_is_asked_for(self):
        cmd = self.ctags.readtags_get_cmd("/tmp/tags", "symbol", "exact", False, None, None, None)
        self.assertIn("symbol", cmd)


class LocateDominatingFileTest(unittest.TestCase):
    """The walk up from a file to the directory that holds what is looked for."""

    def test_found_in_a_parent(self):
        with tempfile.TemporaryDirectory() as root:
            deep = os.path.join(root, "src", "deep")
            os.makedirs(deep)
            with open(os.path.join(root, "tags"), "w") as handle:
                handle.write("")
            self.assertEqual(make_ctags().locate_dominating_file(deep, "tags"),
                             os.path.join(root, "tags"))

    def test_not_found_at_all(self):
        with tempfile.TemporaryDirectory() as root:
            self.assertIsNone(make_ctags().locate_dominating_file(root, "no-such-file"))


if __name__ == "__main__":
    unittest.main()
