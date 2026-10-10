from core.handler import Handler
from core.utils import *

class PrepareCallHierarchy(Handler):
    """Resolve the callable at the cursor before querying its callers.

    The subclass supplies the follow-up handler name in
    `method_call_hierarchy` and the direction in `response_prefix`; this class
    only forwards the first item of the reply to that follow-up call.
    """

    name = "_prepare_call_hierarchy"
    method = "textDocument/prepareCallHierarchy"
    send_document_uri = True

    method_call_hierarchy: str
    response_prefix: str

    def process_request(self, position) -> dict:
        """Return the prepareCallHierarchy params for POSITION."""
        return dict(position=position)

    def process_response(self, response: dict) -> None:
        """Start the incoming or outgoing query for the symbol in RESPONSE."""
        if response is None:
            message_emacs("No call hierarchies found")
        else:
            self.file_action.call(self.method_call_hierarchy, response[0], self.response_prefix)
            
class CallHierarchy(Handler):
    """Collect the incoming or outgoing calls of one callable.

    The starting callable and every call relation added by
    `add_call_content` accumulate in `call_contents`; once the reply has been
    turned into entries with a relative path, the whole list is passed to
    `lsp-bridge-call-hierarchy--popup` and the accumulator is reset.
    """

    name = "_call_hierarchy"
    send_document_uri = False

    response_prefix: str
    call_contents = []

    def process_request(self, item, prefix) -> dict:
        """Return params for ITEM and seed the list with it.

        PREFIX (`to` or `from`) selects the side of each call relation to
        follow and is remembered as `self.response_prefix`.
        """
        self.response_prefix = prefix
        # add current object to call_contents
        self.add_call_content(item, [])
        return dict(item=item)

    def add_call_content(self, item, from_ranges):
        """Append ITEM to `call_contents` as a popup entry.

        The entry keeps the callable's name, path, kind icon, range and the
        FROM_RANGES that reference it in the calling file.
        """
        self.call_contents.append({'name': item['name'],
                                   'path': uri_to_path(item['uri']),
                                   'icon': SYMBOL_MAP[item['kind']].lower(),
                                   'range': item['range'],
                                   'from_ranges': from_ranges})

    def process_response(self, response: dict) -> None:
        """Add the call relations in RESPONSE and popup the collected list."""
        for call_info in response:
            call = call_info[self.response_prefix]
            from_ranges = call_info['fromRanges']
            self.add_call_content(call, from_ranges)

        base_dir = os.path.dirname(self.file_action.filepath)
        for call in self.call_contents:
            rel_path = os.path.relpath(call['path'], base_dir)    # type: ignore
            if '../../../../' not in rel_path:    # type: ignore
                call['rel_path'] = rel_path
            else:
                call['rel_path'] = call['path']

        eval_in_emacs("lsp-bridge-call-hierarchy--popup", self.call_contents)
        
        self.call_contents = []

class PrepareCallHierarchyIncomingCalls(PrepareCallHierarchy, Handler):
    """Prepare a call hierarchy and then list the callers."""

    name = "prepare_call_hierarchy_incoming"
    method_call_hierarchy = "call_hierarchy_incoming"
    response_prefix = "from"

class PrepareCallHierarchyOutgoingCalls(PrepareCallHierarchy, Handler):
    """Prepare a call hierarchy and then list the callees."""

    name = "prepare_call_hierarchy_outgoing"
    method_call_hierarchy = "call_hierarchy_outgoing"
    response_prefix = "to"

class CallHierarchyIncomingCalls(CallHierarchy, Handler):
    """List the calls that reach the prepared callable."""

    name = "call_hierarchy_incoming"
    method = "callHierarchy/incomingCalls"

class CallHierarchyOutgoingCalls(CallHierarchy, Handler):
    """List the calls made by the prepared callable."""

    name = "call_hierarchy_outgoing"
    method = "callHierarchy/outgoingCalls"
    
