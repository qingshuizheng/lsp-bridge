"""Text and position helpers for the handler fallbacks.

These work on the text of a file and its offsets only: they know nothing
about a language, so the Solidity, C# and TypeScript fallbacks can all use
them.  The names lose the leading underscore they had as private helpers of
the Solidity fallback, because they are shared now."""

import os

from core.utils import *

__all__ = [
    "count_top_level_items",
    "extract_receiver_segment_before_dot",
    "extract_word_at",
    "extract_word_span_at",
    "find_matching_paren",
    "get_symbol_at_cursor",
    "index_to_position",
    "line_start_offsets",
    "read_text_file",
]


def extract_word_at(line_text, char_pos):
    """Extract the identifier at the given character position in a line."""
    if not line_text or char_pos < 0:
        return None
    if char_pos >= len(line_text):
        char_pos = len(line_text) - 1
    if char_pos < 0:
        return None

    start = char_pos
    while start > 0 and (line_text[start - 1].isalnum() or line_text[start - 1] == '_'):
        start -= 1
    end = char_pos
    while end < len(line_text) and (line_text[end].isalnum() or line_text[end] == '_'):
        end += 1

    word = line_text[start:end]
    if not word or not (word[0].isalpha() or word[0] == '_'):
        return None
    return word


def extract_word_span_at(line_text, char_pos):
    """Extract (start, end) span of identifier at char_pos in line_text."""
    if not line_text or char_pos < 0:
        return None
    if char_pos >= len(line_text):
        char_pos = len(line_text) - 1
    if char_pos < 0:
        return None

    start = char_pos
    while start > 0 and (line_text[start - 1].isalnum() or line_text[start - 1] == '_'):
        start -= 1
    end = char_pos
    while end < len(line_text) and (line_text[end].isalnum() or line_text[end] == '_'):
        end += 1

    word = line_text[start:end]
    if not word or not (word[0].isalpha() or word[0] == '_'):
        return None
    return (start, end)


def read_text_file(filepath):
    """Read file content from file server, fallback to disk."""
    if not filepath or not os.path.isfile(filepath):
        return None
    content = get_file_content_from_file_server(filepath)
    if content:
        return content
    try:
        with open(filepath, encoding="utf-8", errors="ignore") as f:
            return f.read()
    except OSError:
        return None


def line_start_offsets(content):
    """Build line-start absolute offsets for content."""
    starts = [0]
    for i, ch in enumerate(content):
        if ch == "\n":
            starts.append(i + 1)
    return starts


def index_to_position(content, index):
    """Convert absolute index in content to LSP-style position."""
    if index < 0:
        return {"line": 0, "character": 0}
    line = content.count("\n", 0, index)
    line_start = content.rfind("\n", 0, index)
    char = index if line_start < 0 else index - line_start - 1
    return {"line": line, "character": char}


def find_matching_paren(content, open_idx):
    """Find matching ')' for the '(' at open_idx."""
    if open_idx < 0 or open_idx >= len(content) or content[open_idx] != "(":
        return None

    depth = 1
    i = open_idx + 1
    in_string = None
    escaped = False
    in_line_comment = False
    in_block_comment = False

    while i < len(content):
        ch = content[i]
        nxt = content[i + 1] if i + 1 < len(content) else ""

        if in_line_comment:
            if ch == "\n":
                in_line_comment = False
            i += 1
            continue

        if in_block_comment:
            if ch == "*" and nxt == "/":
                in_block_comment = False
                i += 2
                continue
            i += 1
            continue

        if in_string is not None:
            if ch == "\\" and not escaped:
                escaped = True
                i += 1
                continue
            if ch == in_string and not escaped:
                in_string = None
            escaped = False
            i += 1
            continue

        if ch == "/" and nxt == "/":
            in_line_comment = True
            i += 2
            continue
        if ch == "/" and nxt == "*":
            in_block_comment = True
            i += 2
            continue
        if ch == '"' or ch == "'":
            in_string = ch
            escaped = False
            i += 1
            continue

        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return i

        i += 1

    return None


def count_top_level_items(csv_like_text):
    """Count top-level comma-separated items in a Solidity list."""
    if not csv_like_text or not csv_like_text.strip():
        return 0

    depth_paren = 0
    depth_bracket = 0
    depth_brace = 0
    count = 1

    i = 0
    in_string = None
    escaped = False
    in_line_comment = False
    in_block_comment = False

    while i < len(csv_like_text):
        ch = csv_like_text[i]
        nxt = csv_like_text[i + 1] if i + 1 < len(csv_like_text) else ""

        if in_line_comment:
            if ch == "\n":
                in_line_comment = False
            i += 1
            continue

        if in_block_comment:
            if ch == "*" and nxt == "/":
                in_block_comment = False
                i += 2
                continue
            i += 1
            continue

        if in_string is not None:
            if ch == "\\" and not escaped:
                escaped = True
                i += 1
                continue
            if ch == in_string and not escaped:
                in_string = None
            escaped = False
            i += 1
            continue

        if ch == "/" and nxt == "/":
            in_line_comment = True
            i += 2
            continue
        if ch == "/" and nxt == "*":
            in_block_comment = True
            i += 2
            continue
        if ch == '"' or ch == "'":
            in_string = ch
            escaped = False
            i += 1
            continue

        if ch == "(":
            depth_paren += 1
        elif ch == ")":
            if depth_paren > 0:
                depth_paren -= 1
        elif ch == "[":
            depth_bracket += 1
        elif ch == "]":
            if depth_bracket > 0:
                depth_bracket -= 1
        elif ch == "{":
            depth_brace += 1
        elif ch == "}":
            if depth_brace > 0:
                depth_brace -= 1
        elif ch == "," and depth_paren == 0 and depth_bracket == 0 and depth_brace == 0:
            count += 1

        i += 1

    return count


def extract_receiver_segment_before_dot(line_text, dot_pos):
    """Extract receiver identifier segment before a dot; supports indexed forms."""
    pos = dot_pos - 1
    while pos >= 0 and line_text[pos] == ' ':
        pos -= 1
    if pos < 0:
        return None

    # Skip index expressions like foo[bar].baz
    while pos >= 0 and line_text[pos] == ']':
        bracket_depth = 1
        pos -= 1
        while pos >= 0 and bracket_depth > 0:
            if line_text[pos] == ']':
                bracket_depth += 1
            elif line_text[pos] == '[':
                bracket_depth -= 1
            pos -= 1
        while pos >= 0 and line_text[pos] == ' ':
            pos -= 1

    if pos < 0:
        return None

    seg_end = pos + 1
    while pos >= 0 and (line_text[pos].isalnum() or line_text[pos] == '_'):
        pos -= 1
    seg_start = pos + 1
    name = line_text[seg_start:seg_end]
    if not name or not (name[0].isalpha() or name[0] == '_'):
        return None
    return (name, seg_start, seg_end)


def get_symbol_at_cursor(current_file, cursor_pos):
    """Read the current file and extract the symbol name under the cursor."""
    if not current_file or not os.path.isfile(current_file):
        return None

    content = get_file_content_from_file_server(current_file)
    if not content:
        try:
            with open(current_file, encoding="utf-8", errors="ignore") as f:
                content = f.read()
        except OSError:
            return None

    lines = content.splitlines()
    line_num = cursor_pos.get("line", -1) if isinstance(cursor_pos, dict) else -1
    char_num = cursor_pos.get("character", 0) if isinstance(cursor_pos, dict) else 0
    if line_num < 0 or line_num >= len(lines):
        return None

    word = extract_word_at(lines[line_num], char_num)
    return word
