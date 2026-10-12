"""Tests for the two helpers of the remote file daemon that stand alone.

The daemon itself is about a peer at the other end of a channel - SSH, docker,
heartbeats and the messages of the editor - and is not asked here.  Two of its
helpers are not about a peer at all: the list of addresses it remembers, and the
address it asks docker for, whose failure path is the one worth keeping.
"""

import os
import subprocess
import tempfile
import unittest
from unittest import mock

from core.remote_file import get_container_local_ip, save_ip_to_file


class SaveIpTest(unittest.TestCase):
    """The file of addresses the daemon keeps."""

    def setUp(self):
        handle, self.path = tempfile.mkstemp(prefix="lsp-bridge-ips")
        os.close(handle)
        with open(self.path, "w") as f:
            f.write("10.0.0.1\n10.0.0.2\n")

    def tearDown(self):
        os.unlink(self.path)

    def addresses(self):
        """Return the addresses in the file as a list."""
        with open(self.path) as f:
            return f.read().splitlines()

    def test_a_new_address_is_added_at_the_end(self):
        save_ip_to_file("10.0.0.3", self.path)
        self.assertEqual(self.addresses(), ["10.0.0.1", "10.0.0.2", "10.0.0.3"])

    def test_an_address_already_there_is_not_added_again(self):
        save_ip_to_file("10.0.0.2", self.path)
        self.assertEqual(self.addresses(), ["10.0.0.1", "10.0.0.2"])

    def test_the_same_address_twice_is_written_once(self):
        save_ip_to_file("10.0.0.3", self.path)
        save_ip_to_file("10.0.0.3", self.path)
        self.assertEqual(self.addresses(), ["10.0.0.1", "10.0.0.2", "10.0.0.3"])


class ContainerAddressTest(unittest.TestCase):
    """What is asked of docker, and what a failure comes back as."""

    def test_the_address_docker_prints_is_returned(self):
        with mock.patch("core.remote_file.subprocess.check_output",
                        return_value="172.17.0.2\n") as check:
            self.assertEqual(get_container_local_ip("a-container"), "172.17.0.2")
        # The command asks for the first network address of that container.
        self.assertIn("docker inspect", check.call_args[0][0])
        self.assertIn("a-container", check.call_args[0][0])

    def test_a_failing_docker_is_reported_and_not_raised(self):
        failure = subprocess.CalledProcessError(1, "docker inspect")
        with mock.patch("core.remote_file.subprocess.check_output", side_effect=failure), \
             mock.patch("core.remote_file.message_emacs") as reported:
            self.assertIsNone(get_container_local_ip("a-container"))
        self.assertTrue(reported.called)


if __name__ == "__main__":
    unittest.main()
