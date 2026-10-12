"""Tests for the deep merge, which is a pure function over mappings.

The module merges a source mapping into a destination in place, one key at a time, and
what each key does depends on the two values and on the strategy asked for.  All of that is
data, so all of it is asked here; the only thing the tests have to remember is that `merge'
changes the mapping it is given and returns it.
"""

import unittest
from collections import Counter

from core.mergedeep import Strategy, merge


class ReplaceTest(unittest.TestCase):
    """The default strategy: a value from the source replaces the one in the destination."""

    def test_a_key_only_in_the_source_is_copied_in(self):
        destination = {"a": 1}
        self.assertEqual(merge(destination, {"b": 2}), {"a": 1, "b": 2})

    def test_a_key_in_both_takes_the_source_value(self):
        self.assertEqual(merge({"a": 1}, {"a": 2}), {"a": 2})

    def test_mappings_are_merged_key_by_key(self):
        self.assertEqual(merge({"a": {"x": 1}}, {"a": {"y": 2}}), {"a": {"x": 1, "y": 2}})

    def test_the_value_that_was_copied_is_not_shared_with_the_source(self):
        source = {"a": {"x": 1}}
        merged = merge({}, source)
        source["a"]["x"] = 99
        # A deep copy went in, so the two do not move together.
        self.assertEqual(merged["a"]["x"], 1)

    def test_several_sources_are_merged_in_the_order_given(self):
        self.assertEqual(merge({}, {"a": 1}, {"a": 2}, {"a": 3}), {"a": 3})

    def test_the_destination_is_the_object_that_is_returned(self):
        destination = {"a": 1}
        self.assertIs(merge(destination, {"b": 2}), destination)

    def test_two_counters_are_merged_like_mappings_rather_than_added(self):
        # The counts are taken from the source, key by key, not summed.
        self.assertEqual(merge({"c": Counter({"x": 1, "y": 1})},
                               {"c": Counter({"x": 5})}),
                         {"c": Counter({"x": 5, "y": 1})})


class AdditiveTest(unittest.TestCase):
    """The strategy that combines the two values into one collection."""

    def merge_additive(self, destination, source):
        """Merge SOURCE into DESTINATION with the additive strategy."""
        return merge(destination, source, strategy=Strategy.ADDITIVE)

    def test_lists_are_extended(self):
        self.assertEqual(self.merge_additive({"a": [1]}, {"a": [2, 3]}), {"a": [1, 2, 3]})

    def test_sets_are_updated(self):
        self.assertEqual(self.merge_additive({"a": {1}}, {"a": {2}}), {"a": {1, 2}})

    def test_tuples_are_concatenated(self):
        self.assertEqual(self.merge_additive({"a": (1,)}, {"a": (2,)}), {"a": (1, 2)})

    def test_counters_are_added_up(self):
        self.assertEqual(self.merge_additive({"c": Counter({"x": 1})}, {"c": Counter({"x": 2})}),
                         {"c": Counter({"x": 3})})

    def test_anything_else_is_replaced(self):
        self.assertEqual(self.merge_additive({"a": 1}, {"a": 2}), {"a": 2})


class TypesafeTest(unittest.TestCase):
    """The strategies that refuse to merge two values of different types."""

    def test_two_different_types_are_refused(self):
        with self.assertRaises(TypeError):
            merge({"a": 1}, {"a": "one"}, strategy=Strategy.TYPESAFE_REPLACE)
        with self.assertRaises(TypeError):
            merge({"a": [1]}, {"a": (1,)}, strategy=Strategy.TYPESAFE_ADDITIVE)

    def test_a_subclass_is_not_the_same_type(self):
        with self.assertRaises(TypeError):
            merge({"a": True}, {"a": 1}, strategy=Strategy.TYPESAFE_REPLACE)

    def test_the_same_type_merges(self):
        self.assertEqual(merge({"a": 1}, {"a": 2}, strategy=Strategy.TYPESAFE_REPLACE), {"a": 2})
        self.assertEqual(merge({"a": [1]}, {"a": [2]}, strategy=Strategy.TYPESAFE_ADDITIVE),
                         {"a": [1, 2]})

    def test_typesafe_is_the_alias_of_typesafe_replace(self):
        self.assertEqual(merge({"a": 1}, {"a": 2}, strategy=Strategy.TYPESAFE), {"a": 2})
        with self.assertRaises(TypeError):
            merge({"a": 1}, {"a": [1]}, strategy=Strategy.TYPESAFE)


if __name__ == "__main__":
    unittest.main()
