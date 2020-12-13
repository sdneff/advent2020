from utils import *

def populatePaths(arr):
    # start [1, 0, 0, 0, ...]
    paths = [1] + ([0] * (len(arr) - 1))

    for i in range(0, len(arr)):
        # if current node has n paths, add n to all following nodes we can plug into
        for j in range(1,4): # only need to consider at most following 3 values
            if i + j < len(arr) and arr[i + j] - arr[i] < 4:
                paths[i+j] += paths[i]

    return paths


adapters = prepareSequence(readNumbers('adapters.txt'))

print('path count:', populatePaths(adapters)[-1])
