#!/usr/bin/env python

import os
import subprocess
import sys

jq_paths = ["/usr/local/bin/jq", "/Users/nst/bin/jq"]
dir_path = "/Users/nst/Projects/dropbox/JSON/test_cases/"

existing_jq_paths = [p for p in jq_paths if os.path.exists(p)]

if len(existing_jq_paths) == 0:
    print "-- cannot find jq"
    sys.exit(1)
jq = existing_jq_paths[0]

for root, dirs, files in os.walk(dir_path):
    json_files = (f for f in files if f.endswith(".json"))
    for filename in json_files:
        path = os.path.join(root, filename)
        
        print "*"*80
        print path
        
        parsing_success = subprocess.call([jq, ".", path]) == 0
        
        if filename.startswith("y_") and parsing_success == False:
            print "jq\tSHOULD_HAVE_PASSED\t%s" % (filename)
        elif filename.startswith("n_") and parsing_success == True:
            print "jq\tSHOULD_HAVE_FAILED\t%s" % (filename)
        elif filename.startswith("i_") and parsing_success == True:
            print "jq\tIMPLEMENTATION_PASS\t%s" % (filename)
        elif filename.startswith("i_") and parsing_success == False:
            print "jq\tIMPLEMENTATION_FAIL\t%s" % (filename)
