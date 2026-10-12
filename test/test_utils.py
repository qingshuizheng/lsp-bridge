"""Tests for the helpers of `core/utils.py` that are about text and paths.

The module is the toolbox of the package, and most of its helpers are pure: the
matching engine that decides whether a completion matches what was typed, the
parsers for the two kinds of remote path, and the conversions between the values
Emacs sends and the values Python wants.  Those are asked here.  What is not asked
is the client of the EPC server, the logging and the calls into Emacs, which need
the process and the Emacs on the other end of it.
"""

import unittest

from core.utils import (
    _fuzzy_match,
    _start_match,
    _substring_match,
    cmp,
    convert_emacs_bool,
    is_remote_path,
    is_valid_ip,
    path_as_key,
    split_docker_path,
    split_ssh_path,
    string_match,
    string_to_base64,
)


class MatchersTest(unittest.TestCase):
    """The three matchers a match mode is chosen from."""

    def test_start(self):
        self.assertTrue(_start_match("myFunction", "my"))
        self.assertFalse(_start_match("myFunction", "Function"))

    def test_substring(self):
        self.assertTrue(_substring_match("myFunction", "Func"))
        self.assertFalse(_substring_match("myFunction", "zzz"))

    def test_fuzzy(self):
        # The letters have to appear in order, with anything between them.
        self.assertTrue(_fuzzy_match("myFunction", "mFn"))
        self.assertTrue(_fuzzy_match("myFunction", "myFunction"))
        self.assertFalse(_fuzzy_match("myFunction", "Fnmy"))
        # A single letter is an ordinary search for it.
        self.assertTrue(_fuzzy_match("myFunction", "n"))
        self.assertFalse(_fuzzy_match("myFunction", "z"))

    def test_the_single_letter_case_of_a_substring(self):
        self.assertTrue(_substring_match("myFunction", "y"))
        self.assertFalse(_substring_match("myFunction", "z"))


class StringMatchTest(unittest.TestCase):
    """The mode and the case the completion was configured with."""

    def test_prefix(self):
        self.assertTrue(string_match("myFunction", "my", "prefix"))
        self.assertFalse(string_match("myFunction", "Function", "prefix"))

    def test_substring(self):
        self.assertTrue(string_match("myFunction", "Func", "substring"))
        self.assertFalse(string_match("myFunction", "zzz", "substring"))

    def test_fuzzy_is_anything_else(self):
        self.assertTrue(string_match("myFunction", "mFn", "fuzzy"))
        self.assertFalse(string_match("myFunction", "Fnmy", "fuzzy"))

    def test_prefix_case_sensitive_is_the_older_spelling(self):
        self.assertFalse(string_match("myFunction", "MY", "prefixCaseSensitive"))
        self.assertTrue(string_match("myFunction", "my", "prefixCaseSensitive"))

    def test_case_is_ignored_by_default(self):
        self.assertTrue(string_match("myFunction", "MY", "prefix"))
        self.assertFalse(string_match("myFunction", "MY", "prefix", "sensitive"))

    def test_smart_case_matches_capitals_exactly(self):
        # With a capital anywhere in what was typed, the case has to be the same,
        # which is what makes it smart rather than sensitive.
        self.assertFalse(string_match("myFunction", "MY", "prefix", "smart"))
        self.assertFalse(string_match("myFunction", "My", "prefix", "smart"))
        self.assertTrue(string_match("myFunction", "myF", "prefix", "smart"))
        # Without a capital it does not.
        self.assertTrue(string_match("myFunction", "my", "prefix", "smart"))
        self.assertTrue(string_match("myFunction", "MY".lower(), "prefix", "smart"))


class RemotePathTest(unittest.TestCase):
    """The two kinds of remote path, and what is a remote path at all."""

    def test_an_ssh_path(self):
        # The scheme is not part of what this parses: it takes the `user@host:port:path'
        # form, and the two tests below say that the scheme'd form is not accepted.
        remote_info, host, ssh_conf, path = split_ssh_path("user@1.2.3.4:2222:/tmp/a.py")
        self.assertEqual(host, "1.2.3.4")
        self.assertEqual(ssh_conf["user"], "user")
        self.assertEqual(ssh_conf["port"], "2222")
        self.assertEqual(path, "/tmp/a.py")
        self.assertEqual(remote_info, "user@1.2.3.4:2222:")

    def test_an_ssh_path_without_a_user_or_a_port(self):
        remote_info, host, ssh_conf, path = split_ssh_path("1.2.3.4:/tmp/a.py")
        self.assertEqual(host, "1.2.3.4")
        self.assertEqual(ssh_conf, {"hostname": "1.2.3.4"})
        self.assertEqual(path, "/tmp/a.py")

    def test_something_that_is_not_an_ssh_path(self):
        self.assertIsNone(split_ssh_path("notes.txt"))
        # The scheme'd form of a path is not what this takes.
        self.assertIsNone(split_ssh_path("/ssh:user@1.2.3.4:2222:/tmp/a.py"))

    def test_a_docker_path(self):
        self.assertEqual(split_docker_path("/docker:user@container:/tmp/a.py"),
                         ("user", "container", "/tmp/a.py"))

    def test_something_that_is_not_a_docker_path(self):
        self.assertIsNone(split_docker_path("/tmp/a.py"))

    def test_what_counts_as_remote(self):
        self.assertTrue(is_remote_path("/ssh:1.2.3.4:/tmp/a.py"))
        self.assertTrue(is_remote_path("/docker:user@container:/tmp/a.py"))
        self.assertFalse(is_remote_path("/tmp/a.py"))

    def test_an_ip_is_four_numbers(self):
        self.assertTrue(is_valid_ip("1.2.3.4"))
        self.assertTrue(is_valid_ip("255.255.255.255"))
        # The numbers are ranges, not just one to three digits.
        self.assertFalse(is_valid_ip("999.1.1.1"))
        self.assertFalse(is_valid_ip("1.2.3"))
        self.assertFalse(is_valid_ip("host.example.com"))


class ValueTest(unittest.TestCase):
    """Small conversions between the two sides of the bridge."""

    def test_a_boolean_from_emacs(self):
        self.assertTrue(convert_emacs_bool(True, "t"))
        self.assertFalse(convert_emacs_bool(False, "t"))
        self.assertFalse(convert_emacs_bool(1, "t"))

    def test_a_value_that_is_not_a_boolean(self):
        self.assertEqual(convert_emacs_bool("a string", ""), "a string")

    def test_the_comparison_of_two_values(self):
        self.assertEqual(cmp(1, 2), -1)
        self.assertEqual(cmp(2, 2), 0)
        self.assertEqual(cmp(3, 2), 1)

    def test_base64(self):
        self.assertEqual(string_to_base64("hello"), "aGVsbG8=")
        # The text is encoded as UTF-8 before it is encoded as base64.
        self.assertEqual(string_to_base64("\u4f60\u597d"), "5L2g5aW9")

    def test_a_path_key(self):
        # On a case-sensitive system the key is the path itself.
        self.assertEqual(path_as_key("/tmp/A.py"), "/tmp/A.py")


if __name__ == "__main__":
    unittest.main()
