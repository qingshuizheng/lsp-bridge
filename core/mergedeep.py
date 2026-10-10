from collections import Counter
from collections.abc import Mapping
from copy import deepcopy
from enum import Enum
from functools import reduce, partial
from typing import MutableMapping


class Strategy(Enum):
    """Merge strategy for `merge`.

    REPLACE (the default) lets a value from a source replace the destination
    value, except that two Counters are merged like plain dicts.  ADDITIVE
    combines lists, sets, tuples and Counters into one collection.
    TYPESAFE_REPLACE and TYPESAFE_ADDITIVE raise TypeError when the two values
    have different types and merge otherwise; TYPESAFE is an alias of
    TYPESAFE_REPLACE.
    """
    # Replace `destination` item with one from `source` (default).
    REPLACE = 0
    # Combine `list`, `tuple`, `set`, or `Counter` types into one collection.
    ADDITIVE = 1
    # Alias to: `TYPESAFE_REPLACE`
    TYPESAFE = 2
    # Raise `TypeError` when `destination` and `source` types differ. Otherwise, perform a `REPLACE` merge.
    TYPESAFE_REPLACE = 3
    # Raise `TypeError` when `destination` and `source` types differ. Otherwise, perform a `ADDITIVE` merge.
    TYPESAFE_ADDITIVE = 4


def _handle_merge_replace(destination, source, key):
    """Replace DESTINATION[KEY] with a deep copy of SOURCE[KEY].

    Two Counters are the exception: they are merged like plain dicts instead.
    """
    if isinstance(destination[key], Counter) and isinstance(source[key], Counter):
        # Merge both destination and source `Counter` as if they were a standard dict.
        _deepmerge(destination[key], source[key])
    else:
        # If a key exists in both objects and the values are `different`, the value from the `source` object will be used.
        destination[key] = deepcopy(source[key])


def _handle_merge_additive(destination, source, key):
    """Combine DESTINATION[KEY] and SOURCE[KEY] into a single collection.

    Lists are extended, sets updated, tuples concatenated and Counters added
    up; any other type falls back to a REPLACE merge.
    """
    # Values are combined into one long collection.
    if isinstance(destination[key], list) and isinstance(source[key], list):
        # Extend destination if both destination and source are `list` type.
        destination[key].extend(deepcopy(source[key]))
    elif isinstance(destination[key], set) and isinstance(source[key], set):
        # Update destination if both destination and source are `set` type.
        destination[key].update(deepcopy(source[key]))
    elif isinstance(destination[key], tuple) and isinstance(source[key], tuple):
        # Update destination if both destination and source are `tuple` type.
        destination[key] = destination[key] + deepcopy(source[key])
    elif isinstance(destination[key], Counter) and isinstance(source[key], Counter):
        # Update destination if both destination and source are `Counter` type.
        destination[key].update(deepcopy(source[key]))
    else:
        _handle_merge[Strategy.REPLACE](destination, source, key)


def _handle_merge_typesafe(destination, source, key, strategy):
    """Refuse to merge DESTINATION[KEY] and SOURCE[KEY] of different types.

    Raises TypeError when the two values do not have exactly the same type,
    otherwise delegates to STRATEGY (REPLACE or ADDITIVE).  The TYPESAFE
    entries of `_handle_merge` bind STRATEGY through `functools.partial`.
    """
    # Raise a TypeError if the destination and source types differ.
    if type(destination[key]) is not type(source[key]):  # noqa: E721
        raise TypeError(
            f'destination type: {type(destination[key])} differs from source type: {type(source[key])} for key: "{key}"'
        )
    else:
        _handle_merge[strategy](destination, source, key)


_handle_merge = {
    Strategy.REPLACE: _handle_merge_replace,
    Strategy.ADDITIVE: _handle_merge_additive,
    Strategy.TYPESAFE: partial(_handle_merge_typesafe, strategy=Strategy.REPLACE),
    Strategy.TYPESAFE_REPLACE: partial(_handle_merge_typesafe, strategy=Strategy.REPLACE),
    Strategy.TYPESAFE_ADDITIVE: partial(_handle_merge_typesafe, strategy=Strategy.ADDITIVE),
}


def _is_recursive_merge(a, b):
    """Return True when A and B should be merged key by key.

    Two mappings recurse, but two Counters do not: those are merged as values.
    """
    both_mapping = isinstance(a, Mapping) and isinstance(b, Mapping)
    both_counter = isinstance(a, Counter) and isinstance(b, Counter)
    return both_mapping and not both_counter


def _deepmerge(dst, src, strategy=Strategy.REPLACE):
    """Merge SRC into DST in place and return DST.

    A key that only exists in SRC is deep-copied over; a key in both is merged
    recursively when the two values are mappings, left untouched when they are
    the same object, and handed to STRATEGY otherwise.
    """
    for key in src:
        if key in dst:
            if _is_recursive_merge(dst[key], src[key]):
                # If the key for both `dst` and `src` are both Mapping types (e.g. dict), then recurse.
                _deepmerge(dst[key], src[key], strategy)
            elif dst[key] is src[key]:
                # If a key exists in both objects and the values are `same`, the value from the `dst` object will be used.
                pass
            else:
                _handle_merge.get(strategy)(dst, src, key)
        else:
            # If the key exists only in `src`, the value from the `src` object will be used.
            dst[key] = deepcopy(src[key])
    return dst


def merge(destination: MutableMapping, *sources: Mapping, strategy: Strategy = Strategy.REPLACE) -> MutableMapping:
    """
    A deep merge function for 🐍.

    :param destination: The destination mapping.
    :param sources: The source mappings.
    :param strategy: The merge strategy.
    :return:
    """
    return reduce(partial(_deepmerge, strategy=strategy), sources, destination)
