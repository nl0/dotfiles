#!/usr/bin/env python2

import argparse
import sys
from collections import deque

class StoreColors(argparse.Action):
    def __call__(self, parser, namespace, values, option_string=None):
        res = {}
        for v in values:
            val, col = v.split(':')
            res[int(val)] = col
        setattr(namespace, self.dest, res)

parser = argparse.ArgumentParser(description='Graph for dzen2.')
parser.add_argument('-p', '--prefix', default='')
parser.add_argument('-s', '--suffix', default='')
parser.add_argument('-n', '--segment-count', type=int, default=50)
parser.add_argument('-H', '--height', type=int, default=13)
parser.add_argument('-w', '--segment-width', type=int, default=1)
parser.add_argument('-c', '--colors', action=StoreColors, nargs='+',
    default={0: '#30c030', 50: '#c0c030', 75: '#c03030'})
parser.add_argument('-m', '--max', type=float, default=100.0)


def format(seq, opts):
    res = ''
    for i in q:
        h = round(i/opts.max*opts.height)
        for k, v in opts.colors.items():
            if i>=k: c = v
        res+='^fg(%s)^pa(;%d)^r(%dx%d)'%(
            c, opts.height-h+1, opts.segment_width, h)
    return res

if __name__=='__main__':
    opts = parser.parse_args()

    q = deque([0]*opts.segment_count)

    while 1:
        l = sys.stdin.readline()
        if not l: break
        q.append(int(l))
        q.popleft()
        sys.stdout.write('^fg()^pa()'+opts.prefix+format(q, opts)+
            '^fg()^pa()'+opts.suffix+'\n')
        sys.stdout.flush()

