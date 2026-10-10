"""Tests for the Emacs <-> Python protocol handshake.

The handshake is deliberately split into pure builders that this unittest can
check without an EPC server: `build_handshake` answers the Emacs side and
`build_diagnose` answers the `lsp-bridge-diagnose` command.  The last test goes
through `scripts/check-protocol.py`, which is also what a commit runs, so a
version bump on one side without the other fails here as well as there.
"""

import importlib.util
import json
import pathlib
import unittest

import lsp_bridge as lb

BASE_DIR = pathlib.Path(__file__).resolve().parent.parent


def load_protocol_gate():
    """Return the `scripts/check-protocol.py` module."""
    spec = importlib.util.spec_from_file_location(
        "check_protocol", BASE_DIR / "scripts" / "check-protocol.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Handshake(unittest.TestCase):
    def test_matching_version_is_compatible(self):
        reply = lb.build_handshake(lb.PROTOCOL_VERSION)
        self.assertTrue(reply["compatible"])
        self.assertEqual(reply["protocol_version"], lb.PROTOCOL_VERSION)
        self.assertEqual(reply["capabilities"], lb.CAPABILITIES)
        self.assertIsInstance(reply["pid"], int)
        self.assertTrue(reply["python_version"])

    def test_another_version_is_not_compatible(self):
        self.assertFalse(lb.build_handshake(lb.PROTOCOL_VERSION + 1)["compatible"])
        self.assertFalse(lb.build_handshake(lb.PROTOCOL_VERSION - 1)["compatible"])

    def test_version_arrives_as_a_string_too(self):
        # Emacs numbers cross EPC as numbers, but a string has to work as well.
        self.assertTrue(lb.build_handshake(str(lb.PROTOCOL_VERSION))["compatible"])

    def test_capabilities_cover_what_the_emacs_side_calls(self):
        for capability in ("completion", "diagnose"):
            self.assertIn(capability, lb.CAPABILITIES)


class Diagnose(unittest.TestCase):
    def test_reports_the_runtime_facts(self):
        reply = lb.build_diagnose(3, 5)
        self.assertEqual(reply["lsp_server_count"], 3)
        self.assertEqual(reply["file_action_count"], 5)
        self.assertEqual(reply["protocol_version"], lb.PROTOCOL_VERSION)
        self.assertTrue(reply["capabilities"])
        self.assertIsInstance(reply["pid"], int)

    def test_replies_survive_json(self):
        # What the EPC methods return has to be readable by `json-parse-string'.
        for reply in (lb.build_handshake(lb.PROTOCOL_VERSION), lb.build_diagnose(0, 0)):
            self.assertEqual(json.loads(json.dumps(reply)), reply)


class ProtocolConstants(unittest.TestCase):
    def test_both_halves_declare_the_same_version(self):
        gate = load_protocol_gate()
        self.assertEqual(gate.elisp_protocol_version(), lb.PROTOCOL_VERSION)
        self.assertEqual(gate.python_protocol_version(), lb.PROTOCOL_VERSION)

    def test_the_capability_list_is_not_empty(self):
        self.assertTrue(load_protocol_gate().python_capabilities())


if __name__ == "__main__":
    unittest.main()
