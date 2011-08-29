#!/usr/bin/env python

import json, sys, time

count = int(sys.argv[1])

for n in sys.argv[2:]:
    print '%s:' % n
    obj = json.load(open(n))
    start = time.time()
    for i in xrange(count):
        json.dumps(obj)
    end = time.time()
    print ' ', end - start
