def readNumbers(path):
    with open(path, 'r') as f:
        return [int(l) for l in f.readlines()]

def splitPreamble(numbers):
    return (numbers[:25], numbers[25:])

class NumberState:
    sums = set()
    numbers = set()

    def loadPreamble(self, nums):
        for n in nums:
            self.addNext(n)

    def addNext(self, n):
        # print("adding: " ,n , self.sums, self.numbers)
        self.sums.update([n + prevN for prevN in self.numbers])
        self.numbers.add(n)

    def isValid(self, n):
        return n in self.sums

def getFirstInvalid(numbers):
    pre, body = splitPreamble(numbers)

    state = NumberState()
    state.loadPreamble(pre)

    for n in body:
        if state.isValid(n):
            state.addNext(n)
        else:
            return n
    return 0
