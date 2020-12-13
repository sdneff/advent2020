from utils import *

numbers = readNumbers('numbers.txt')

target = getFirstInvalid(numbers)
slice = numbers[:numbers.index(target)]

def findSumSlice(arr, sum):
    # track sums in-place: we can compute window=4 sums by building off window=3 sums, etc.
    sums = arr.copy() # start with copy (window = 1)
    for window in range(2, len(arr)): # increase window=2, 3, 4, ...
        for idx in range(window - 1, len(arr)): # offset of array (=slice end index)
            startIdx = idx - (window - 1) # slice start index
            n = arr[idx]
            s = n + sums[startIdx]
            if (s == sum):
                return arr[startIdx:startIdx + window]
            else:
                sums[startIdx] = s # store sum
    raise Exception('cannot find sum: ', sum)

slice = findSumSlice(numbers, target)

print('sum of min & max in slice:', min(slice) + max(slice))
