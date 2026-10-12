"""Tests for the words the project search indexes and how it matches them.

`SearchFileWords' is driven by Emacs and runs a dispatcher thread, so the tests make
one without its constructor and give it the state they need.  What is asked is the
matching: which words are worth indexing, how the case of a prefix is answered, the
order the candidates come in, and the fallback that searches the last segment of a
dashed or underscored prefix and puts the rest of it back.
"""

import unittest
from unittest import mock

from core.search_file_words import SearchFileWords


def words_action(files, max_number=20, fuzzy_match=False):
    """Return a search that indexes FILES and sends what it finds to Emacs."""
    search = SearchFileWords.__new__(SearchFileWords)
    search.files = files
    search.max_number = max_number
    search.fuzzy_match = fuzzy_match
    search.fuzzy_match_threshold = 0.0
    return search


def found(search, prefix):
    """Return the candidates the search sends to Emacs for PREFIX."""
    sent = []
    with mock.patch("core.search_file_words.eval_in_emacs",
                    side_effect=lambda *args: sent.append(args)):
        search.search_words_from_files(prefix)
    if not sent:
        return []
    return [item["label"] for item in sent[0][2]]


class FilterWordTest(unittest.TestCase):
    """Which words are worth keeping."""

    def test_a_word_of_four_characters_or_more_is_kept(self):
        search = words_action({})
        self.assertTrue(search.filter_word("word"))
        self.assertTrue(search.filter_word("a-very-long-name"))

    def test_short_words_are_skipped(self):
        search = words_action({})
        self.assertFalse(search.filter_word("the"))
        self.assertFalse(search.filter_word("a"))

    def test_numbers_are_skipped(self):
        search = words_action({})
        self.assertFalse(search.filter_word("1234"))
        self.assertTrue(search.filter_word("word2"))


class SearchWordTest(unittest.TestCase):
    """The matches of a prefix, without the fuzzy option."""

    def search(self, prefix, words):
        return words_action({}).search_word(prefix, set(words))

    def test_only_words_starting_with_the_prefix_match(self):
        self.assertEqual(self.search("my", ["myFunction", "other"]), ["myFunction"])

    def test_the_matching_is_case_insensitive(self):
        # A word written in another case is still found, whatever case was typed.
        self.assertEqual(self.search("My", ["myFunction"]), ["MyFunction"])
        self.assertEqual(self.search("mY", ["MyFunction"]), ["mYFunction"])

    def test_the_case_typed_is_kept_in_the_answer(self):
        # The word is completed with the prefix as it was typed, so what the user
        # typed is what stands in front of the rest of the word.
        self.assertEqual(self.search("My", ["myFunction"]), ["MyFunction"])

    def test_an_upper_case_prefix_answers_in_upper_case(self):
        self.assertEqual(self.search("MY", ["myFunction"]), ["MYFUNCTION"])

    def test_the_shortest_candidates_come_first(self):
        # Only the length is ordered on: candidates of the same length keep the
        # order they were found in, which is not something to pin down.
        self.assertEqual(self.search("my", ["myLongerName", "myB"]),
                         ["myB", "myLongerName"])


class SearchWordsFromFilesTest(unittest.TestCase):
    """What is sent to Emacs, including the fallback of a dashed prefix."""

    def test_the_words_of_every_file_are_searched(self):
        search = words_action({"/a.py": {"myFunction"}, "/b.py": {"myOther"}})
        self.assertEqual(sorted(found(search, "my")), ["myFunction", "myOther"])

    def test_the_number_sent_is_capped(self):
        search = words_action({"/a.py": {"myA", "myB", "myC"}}, max_number=2)
        self.assertEqual(len(found(search, "my")), 2)

    def test_a_dashed_prefix_falls_back_to_its_last_segment(self):
        # Nothing starts with `acm-backend-elisp', so `elisp' is searched and the
        # part in front of it is put back.
        search = words_action({"/a.el": {"elispCandidates", "backendThings"}})
        self.assertEqual(found(search, "acm-backend-elisp"), ["acm-backend-elispCandidates"])

    def test_an_underscored_prefix_falls_back_to_its_last_segment(self):
        search = words_action({"/a.py": {"wordsThings"}})
        self.assertEqual(found(search, "search_words"), ["search_wordsThings"])

    def test_a_prefix_ending_in_the_separator_has_no_fallback(self):
        search = words_action({"/a.el": {"elispCandidates"}})
        self.assertEqual(found(search, "acm-backend-"), [])

    def test_nothing_is_sent_when_nothing_matches(self):
        search = words_action({"/a.py": {"myFunction"}})
        self.assertEqual(found(search, "zzz"), [])


if __name__ == "__main__":
    unittest.main()
