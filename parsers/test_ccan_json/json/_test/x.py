#!/usr/bin/env python

import os

with open("test-strings") as f:
    for (i,l) in enumerate(f):
        l = l.rstrip()
        if l.startswith("valid"):
            filename = "y_%s.json" % i
        else:
            filename = "n_%s.json" % i

        try:
            l = l[l.index(" ")+1:]
                
            f = open(os.path.sep.join(["nst_files", filename]), 'wb+')
            f.write(l)
            f.close()
        except:
            pass
        