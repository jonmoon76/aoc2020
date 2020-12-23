#!/usr/bin/python3
import fileinput
from pprint import pprint
from typing import List

mem = {}
for line in fileinput.input():
    l, r = line.strip().split(" = ")
    if l == "mask":
        and_mask = int(r.replace("X", "1"), base=2)
        or_mask = int(r.replace("X", "0"), base=2)
    else:
        value = int(r)
        value &= and_mask
        value |= or_mask
        mem[int(l[4:-1])] = value
        print("set[{}] {}: {}".format(int(l[4:-1]), int(r), value))

print(sum(mem.values()))

def recurse(s: str) -> List[str]:
    x = s.find("X")
    if x == -1:
        return [s]

    s0 = s[:x] + '0' + s[x + 1:]
    s1 = s[:x] + '1' + s[x + 1:]

    return [*recurse(s0), *recurse(s1)]

mem = {}
for line in fileinput.input():
    l, r = line.strip().split(" = ")

    if l == "mask":
        mask = r
        or_mask = int(r.replace("X", "0"), base=2)

        floatings = recurse(r.replace("0", "_").replace("1", "_"))
        and_masks = [int(v.replace("_", "1"), 2) for v in floatings]
        or_masks = [int(v.replace("_", "0"), 2) for v in floatings]

        assert(len(and_masks) == 2 ** r.count("X"))
        assert(len(or_masks) == 2 ** r.count("X"))
    else:
        orig_address = int(l[4:-1])
        orig_address |= or_mask

        for (and_, or_) in zip(and_masks, or_masks):
            address = orig_address
            address &= and_
            address |= or_
            mem[address] = int(r)
            assert(address >= 0)

assert(not [k for k in mem.keys() if k < 0])
print(sum(mem.values()))
