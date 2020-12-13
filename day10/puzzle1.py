from utils import *

adapters = prepareSequence(readNumbers('adapters.txt'))

# considering:
# - adapters must connect to lower-joltage adapter => sequence must be monotonically increasing
# - we are asked to use all adapters
# - THEN our sequence is just a sort
# (this probably doesn't set us up for puzzle 2, but let's not overthink it)

deltas = getDiffs(adapters)

ones = deltas.count(1)
threes = deltas.count(3)

print('ones times threes:', ones * threes)
