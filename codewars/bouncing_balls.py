import unittest
import math


# def bouncingBall(h, bounce, window):
#     return math.log(window) / math.log(bounce * h)


def bouncingBall(h, bounce, window):
    height = h * bounce
    sightings = 1
    while height > window:
        sightings += 2
        height *= bounce
    return sightings


class TestBouncingBall(unittest.TestCase):
    def test1(self):
        self.assertEqual(bouncingBall(3, 0.66, 1.5), 3)

    def test2(self):
        self.assertEqual(bouncingBall(30, 0.66, 1.5), 15)



if __name__ == '__main__':
    unittest.main()
