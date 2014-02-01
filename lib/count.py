
from collections import defaultdict
import sys

dict = defaultdict(lambda: 0)

for line in open(sys.argv[1]):
    for word in line.rstrip().split(' '):
        dict[word] += 1

print len(dict.keys())
# for w, c in dict.items():
#     print w, c 

