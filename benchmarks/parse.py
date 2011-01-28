#!/usr/bin/env python

import json, sys, time

count = int(sys.argv[1])

for n in sys.argv[2:]:
    print '%s:' % n
    start = time.time()
    fp = open(n)
    for i in xrange(count):
        fp.seek(0)
        val = json.load(fp)
    end = time.time()
    print ' ', end - start
