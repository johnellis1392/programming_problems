# Problem 1: Multiples of Three or Five
# (AKA FizzBuzz)
def main():
    Sum = 0
    for i in range(1000):
        if i % 3 == 0 or i % 5 == 0:
            Sum += i
    return Sum

if __name__ == '__main__':
    print('Sum = %d' % main())