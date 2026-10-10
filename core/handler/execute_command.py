from core.handler import Handler

class ExecuteCommand(Handler):
    """Run a workspace command the server advertised.

    clojure-lsp rejects empty JSON objects in the arguments, so for that server
    every `{}` is first replaced by `null`; the reply itself is ignored.
    """

    name = "execute_command"
    method = "workspace/executeCommand"
    send_document_uri = False

    def fix_empty_objects(self, obj):
        """Return OBJ with every empty dict replaced by None, recursively."""
        if isinstance(obj, dict):
            # Use `null` replace `{}`, avoid "Internal error" from clojure-lsp
            if len(obj) == 0:
                return None
            return {k: self.fix_empty_objects(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [self.fix_empty_objects(item) for item in obj]
        else:
            return obj

    def process_request(self, server_name, command, arguments) -> dict:
        """Return params running COMMAND with ARGUMENTS on SERVER_NAME.

        The arguments are sanitized for clojure-lsp, which is the only server
        that needs `fix_empty_objects`.
        """
        if server_name == "clojure-lsp":
            arguments = self.fix_empty_objects(arguments)

        return dict(command=command, arguments=arguments)

    def process_response(self, response) -> None:
        """Ignore RESPONSE; commands report through other channels."""
        pass
