import unittest
from unittest.mock import patch, MagicMock
from mock import Mock
from io import StringIO

import sys
import input_mock

# class TestPatch(unittest.TestCase):
#
#     # def test1(self):
#     #     input_data = "1 2 3"
#     #     expected = "3 2 1"
#     #
#     #     # with patch.object(input_mock, "raw_input", create = True, return_value = expected):
#     #     with patch.object(input_mock, "input", create = True, return_value = expected) as input_stub:
#     #         # create = True is necessary since we're mocking a builtin function
#     #         input_mock.test_func()
#     #         actual = sys.stdout.readline()
#     #
#     #     self.assertEqual(expected, actual)
#
#
#     @patch('input_mock.raw_input', create = True, new = MagicMock(return_value = '1 2 3'))
#     @patch('input_mock.print', create = True, new = MagicMock())
#     def test2(self):
#         input_data = '1 2 3'
#         expected = '3 2 1'
#         input_mock.test_func()
#         input_mock.print.assert_called_with(expected)
#
#
#     def test3(self):
#         input_data = '1 2 3'
#         expected = '3 2 1'
#
#         with patch.object(input_mock, 'print', create = True, return_value = None) as print_mock, patch.object(input_mock, 'input', create = True, return_value = input_data) as input_stub:
#             input_mock.test_func()
#             input_stub.assert_called_once()
#             print_mock.assert_called_once_with(expected)


# class TestStdin(unittest.TestCase):
#     def setUp(self):
#         sys.stdin = StringIO()
#         sys.stdout = StringIO()
#
#     def tearDown(self):
#         sys.stdin = sys.__stdin__
#         sys.stdout = sys.__stdout__
#
#     def test1(self):
#         # Mock out stdin using sys
#
#         input_data = "1 2 3"
#         expected = "3 2 1"
#         sys.stdin = StringIO(input_data)
#
#         input_mock.test_func()
#         # actual = sys.stdout.readline()
#         actual = sys.stdout.readlines()
#         self.assertEqual(expected, actual)



class TestPatch(unittest.TestCase):

    @patch('input_mock.input', create = True, new = Mock(return_value = '1 2 3'))
    @patch('input_mock.print', create = True, new = Mock(return_value = None))
    def test1(self):
        expected = '3 2 1'

        input_mock.test_func()
        input_mock.print.assert_called_once_with(expected)



if __name__ == '__main__':
    unittest.main()
