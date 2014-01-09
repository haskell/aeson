#!/usr/bin/env python

import os, re, subprocess, sys

result_re = re.compile(r'^\s*(\d+) good, (\d+\.\d+)s$', re.M)

if len(sys.argv) > 1:
    parser_exe = sys.argv[1]
else:
    parser_exe = ('dist/build/aeson-benchmark-aeson-parse/' +
                  'aeson-benchmark-aeson-parse')

def run(count, filename):
    print '    %s :: %s times' % (filename, count)
    p = subprocess.Popen([parser_exe, '65536', str(count), filename],
                         stdout=subprocess.PIPE)
    output = p.stdout.read()
    p.wait()
    m = result_re.search(output)
    if not m:
        print >> sys.stderr, 'run gave confusing output!?'
        sys.stderr.write(output)
        return
    else:
        #sys.stdout.write(output)
        pass
    good, elapsed = m.groups()
    good, elapsed = int(good), float(elapsed)
    st = os.stat(filename)
    parses_per_second = good / elapsed
    mb_per_second = st.st_size * parses_per_second / 1048576
    print ('      %.3f seconds, %d parses/sec, %.3f MB/sec' %
           (elapsed, parses_per_second, mb_per_second))
    return parses_per_second, mb_per_second, st.st_size, elapsed

def runtimes(count, filename, times=1):
    for i in xrange(times):
        yield run(count, filename)

info = '''
json-data/twitter1.json   60000
json-data/twitter10.json  13000
json-data/twitter20.json   7500
json-data/twitter50.json   2500
json-data/twitter100.json  1000
json-data/jp10.json        4000
json-data/jp50.json        1200
json-data/jp100.json        700
'''

for i in info.strip().splitlines():
    name, count = i.split()
    best = sorted(runtimes(int(count), name, times=3), reverse=True)[0]
    parses_per_second, mb_per_second, size, elapsed = best
    print ('%.1f KB: %d msg\\/sec (%.1f MB\\/sec)' %
           (size / 1024.0, int(round(parses_per_second)), mb_per_second))
