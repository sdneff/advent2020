def readNumbers(path):
    with open(path, 'r') as f:
        return [int(l) for l in f.readlines()]

def getDiffs(arr):
    return [j-i for i, j in zip(arr[:-1], arr[1:])]

def prepareSequence(arr):
    sequence = arr.copy()
    sequence.sort()
    return [0] + sequence + [max(sequence) + 3]
