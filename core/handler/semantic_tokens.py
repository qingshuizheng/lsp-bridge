import time

from core.handler import Handler
from core.utils import *

class SemanticTokens(Handler):
    """Turn LSP semantic tokens into face runs for the visible buffer range.

    An instance caches the face names fetched from Emacs, the tokens rendered
    by the previous reply and the buffer range they were requested for, so
    successive replies can be diffed and only the changed runs re-rendered.
    """

    name = "semantic_tokens"
    method = "textDocument/semanticTokens/full"
    cancel_on_change = False
    send_document_uri = True
    provider = "semantic_tokens_provider"
    provider_message = "Current server not support semantic tokens."

    def __init__(self, *args, **kwargs):
        """Initialize the caches of face names, token data and last renders."""
        super().__init__(*args, **kwargs)
        self.buffer_name = None
        self.range_begin = None
        self.range_end = None

        self.pre_last_change = (time.time(), time.time())
        self.tokens = []
        self.render_tokens = set()

        self.type_face_names = None
        self.type_face_names_dict = dict()
        self.type_modifier_face_names = None
        self.ignore_modifier_limit_types = None

    def process_request(self, buffer_name, range_begin, range_end, use_cache):
        """Return empty params for the view of BUFFER_NAME to be painted.

        RANGE_BEGIN and RANGE_END are remembered as the lines to render, and
        the previous render is forgotten unless USE_CACHE is set.
        """
        self.buffer_name = buffer_name
        self.range_begin = range_begin
        self.range_end = range_end
        if not use_cache:
            self.render_tokens = set()
        return dict()

    def process_response(self, response):
        """Decode the token `data` in RESPONSE and render the new runs."""
        if response is None:
            return
        data = response.get("data")
        if data is None:
            return
        self.tokens = data
        self.update_tokens(self.tokens)

    def update_tokens(self, tokens):
        """Render the face runs described by the delta-encoded TOKENS.

        Each five-integer group is decoded into a line, column, length and
        face; runs inside the requested range are compared with the previously
        rendered set and only the added and removed ones are sent to
        `lsp-bridge-semantic-tokens--render`, after converting the added ones
        back to the relative form Emacs expects.
        """
        if tokens is None:
            return
        index = 0
        cur_line = 0
        start_character = 0
        render_tokens = set()
        while index < len(tokens):
            if tokens[index] != 0:
                start_character = 0
                cur_line += tokens[index]
            start_character += tokens[index + 1]
            if (cur_line >= self.range_begin["line"] and
                cur_line <= self.range_end["line"]):
                faces_index = self.get_faces_index(tokens[index + 3], tokens[index + 4])
                if faces_index is not None:
                    render_tokens.add((cur_line, start_character, tokens[index + 2], faces_index[0], faces_index[1]))
            index = index + 5

        (new_tokens, old_tokens) = self.calc_diff_tokens(self.render_tokens, render_tokens)

        if len(new_tokens) != 0 or len(old_tokens) != 0:
            eval_in_emacs("lsp-bridge-semantic-tokens--render", self.buffer_name, list(old_tokens), self.absolute_line_to_relative(new_tokens))
        self.render_tokens = render_tokens

    def get_faces_index(self, type_index, type_modifier_index):
        """Return the face indices for the legend TYPE_INDEX and TYPE_MODIFIER_INDEX.

        TYPE_INDEX and TYPE_MODIFIER_INDEX index the server's token legend;
        None is returned when the token has no modifiers worth rendering and
        the type is not allowed to ignore them, or when the type has no face.
        """
        lsp_server = self.file_action.get_match_lsp_servers("semantic_tokens")[0]
        type_name = lsp_server.semantic_tokens_provider["legend"]["tokenTypes"][type_index]
        ignore_modifier = self.is_ignore_modifier(type_name)
        if type_modifier_index == 0 and not ignore_modifier:
            return None
        type_face_index = self.get_type_face_index(type_name)
        if type_face_index is None:
            return None
        type_modifier_faces_index = self.get_type_modifier_faces_index(type_modifier_index)
        if len(type_modifier_faces_index) == 0 and not ignore_modifier:
            return None
        return (type_face_index, type_modifier_faces_index)

    def get_type_face_index(self, type_name):
        """Return the face index configured for the token TYPE_NAME.

        The `lsp-bridge-semantic-tokens-type-faces` variable is read once and
        cached in `self.type_face_names`; None means the type has no face, and
        an unset variable also disables the lookup.
        """
        if self.type_face_names is None:
            [type_faces] = get_emacs_vars(["lsp-bridge-semantic-tokens-type-faces"])
            if type_faces is not None:
                faces = type_faces.I
                self.type_face_names = dict()
                for index in range(len(faces)):
                    self.type_face_names[faces[index][0]] = index
            else:
                return None
        face_index = self.type_face_names.get(type_name)
        return face_index

    def get_type_modifier_faces_index(self, type_modifier_index):
        """Return the face indices of the modifier bits in TYPE_MODIFIER_INDEX.

        The `lsp-bridge-semantic-tokens-type-modifier-faces` variable is read
        once and cached in `self.type_modifier_face_names`; modifiers without a
        configured face are skipped, and an empty tuple means no face at all.
        """
        if self.type_modifier_face_names is None:
            [type_modifier_faces] = get_emacs_vars(["lsp-bridge-semantic-tokens-type-modifier-faces"])
            if type_modifier_faces is not None:
                faces = type_modifier_faces.I
                self.type_modifier_face_names = dict()
                for index in range(len(faces)):
                    self.type_modifier_face_names[faces[index][0]] = index
            else:
                return ()

        lsp_server = self.file_action.get_match_lsp_servers("semantic_tokens")[0]
        token_modifiers = lsp_server.semantic_tokens_provider["legend"]["tokenModifiers"]
        type_modifier_names = [token_modifiers[index] for index in self.find_ones(type_modifier_index)]
        type_modifier_faces_index = []
        for name in type_modifier_names:
            index = self.type_modifier_face_names.get(name)
            if index is not None:
                type_modifier_faces_index.append(index)
        return tuple(type_modifier_faces_index)

    def is_ignore_modifier(self, type_name):
        """Return True if TYPE_NAME may be rendered without its modifiers.

        The `lsp-bridge-semantic-tokens-ignore-modifier-limit-types` variable
        is read once and cached in `self.ignore_modifier_limit_types`.
        """
        if self.ignore_modifier_limit_types is None:
            [types] = get_emacs_vars(["lsp-bridge-semantic-tokens-ignore-modifier-limit-types"])
            types = types.I
            if types is not None:
                self.ignore_modifier_limit_types = dict()
                for index in range(len(types)):
                    self.ignore_modifier_limit_types[types[index]] = index

        return type_name in self.ignore_modifier_limit_types

    def calc_diff_tokens(self, pre_tokens, cur_tokens):
        """Return the (new, old) tokens that differ between the two sets.

        PRE_TOKENS are the runs rendered last time and CUR_TOKENS the ones
        found now; each list is sorted by line, and tokens present in both are
        dropped from the result.
        """
        common_tokens = cur_tokens & pre_tokens
        new_tokens = list(cur_tokens - common_tokens)
        old_tokens = list(pre_tokens - common_tokens)
        new_tokens.sort(key = lambda token : token[0])
        old_tokens.sort(key = lambda token : token[0])
        return (new_tokens, old_tokens)

    def absolute_line_to_relative(self, tokens):
        """Return TOKENS re-encoded with relative lines and columns.

        Absolute (line, column) pairs are turned into the delta form Emacs
        applies sequentially, keeping the length and both face indices.
        """
        relative_tokens = []
        cur_line = 0
        delta_line = 0
        start_character = 0
        delta_character = 0
        for token in tokens:
            delta_line = token[0] - cur_line
            if token[0] != cur_line:
                cur_line = token[0]
                start_character = 0
            delta_character = token[1] - start_character
            start_character = token[1]
            relative_tokens.append((delta_line, delta_character, token[2], token[3], token[4]))

        return relative_tokens

    def find_ones(self, num):
        """Return the sorted bit positions that are set in NUM.

        NUM is the bit set of token modifier indices from the LSP legend.
        """
        ones = []
        bit = 0
        while num > 0:
            if num & 1:
                ones.append(bit)
            num >>= 1
            bit += 1
        return ones
