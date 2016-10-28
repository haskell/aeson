#!/usr/bin/env python3

import os
import subprocess
import sys

from os import listdir
from time import strftime

BASE_DIR = os.path.dirname(os.path.realpath(__file__))
PARSERS_DIR = os.path.join(BASE_DIR, "parsers")
TEST_CASES_DIR_PATH = os.path.join(BASE_DIR, "test_parsing")
LOGS_DIR_PATH = os.path.join(BASE_DIR, "results")
LOG_FILENAME = "logs.txt"
LOG_FILE_PATH = os.path.join(LOGS_DIR_PATH, LOG_FILENAME)

programs = {
    "Bash JSON.sh 2016-08-12":
        {
            "url":"https://github.com/dominictarr/JSON.sh",
            "commands":[os.path.join(PARSERS_DIR, "test_Bash_JSON/JSON.sh")],
            "use_stdin":True
        },
    "R rjson":
        {
            "url":"",
            "commands":["/usr/local/bin/RScript", os.path.join(PARSERS_DIR, "test_rjson.r")]
        },
    "R jsonlite":
        {
            "url":"",
            "commands":["/usr/local/bin/RScript", os.path.join(PARSERS_DIR, "test_jsonlite.r")]
        },
   "Obj-C JSONKit":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_JSONKit/bin/test-JSONKit")]
       },
   "Obj-C Apple NSJSONSerialization":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_ObjCNSJSONSerializer/bin/test_ObjCNSJSONSerializer")]
       },
   "Obj-C TouchJSON":
       {
           "url":"https://github.com/TouchCode/TouchJSON",
           "commands":[os.path.join(PARSERS_DIR, "test_TouchJSON/bin/test_TouchJSON")]
       },
   "Obj-C SBJSON 4.0.3":
       {
           "url":"https://github.com/stig/json-framework",
           "commands":[os.path.join(PARSERS_DIR, "test_sbjson/bin/test_sbjson")]
       },
   "Go 1.7.1":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_go/test_json")]
       },
   "Lua JSON 20160916.19":
       {
           "url":"http://regex.info/blog/lua/json",
           "commands":["/usr/local/bin/lua", os.path.join(PARSERS_DIR, "test_Lua_JSON/test_JSON.lua")]
       },
   "Lua dkjson":
       {
           "url":"http://dkolf.de/src/dkjson-lua.fsl/home",
           "commands":["/usr/local/bin/lua", os.path.join(PARSERS_DIR, "test_dkjson.lua")]
       },
   "Ruby":
       {
           "url":"",
           "commands":["/usr/bin/env", "ruby", os.path.join(PARSERS_DIR, "test_json.rb")]
       },
   "Ruby regex":
       {
           "url":"",
           "commands":["/usr/bin/env", "ruby", os.path.join(PARSERS_DIR, "test_json_re.rb")]
       },
   "Ruby Yajl":
       {
           "url":"https://github.com/brianmario/yajl-ruby",
           "commands":["/usr/bin/env", "ruby", os.path.join(PARSERS_DIR, "test_yajl.rb")]
       },
   "Ruby Oj (strict mode)":
       {
           "url":"https://github.com/ohler55/oj",
           "commands":["/usr/bin/env", "ruby", os.path.join(PARSERS_DIR, "test_oj_strict.rb")]
       },
   "Ruby Oj (compat mode)":
       {
           "url":"https://github.com/ohler55/oj",
           "commands":["/usr/bin/env", "ruby", os.path.join(PARSERS_DIR, "test_oj_compat.rb")]
       },
   "Crystal":
       {
           "url":"https://github.com/crystal-lang/crystal",
           "commands":[os.path.join(PARSERS_DIR, "test_json_cr")]
       },
   "JavaScript":
       {
           "url":"",
           "commands":["/usr/local/bin/node", os.path.join(PARSERS_DIR, "test_json.js")]
       },
   "Python 2.7.10":
       {
           "url":"",
           "commands":["/usr/bin/python", os.path.join(PARSERS_DIR, "test_json.py")]
       },
   "Perl JSON":
       {
           "url":"",
           "commands":["/usr/bin/perl", os.path.join(PARSERS_DIR, "test_json.pl")]
       },
   "Perl JSON::XS":
       {
           "url":"http://search.cpan.org/dist/JSON-XS/XS.pm",
           "commands":["/usr/bin/perl", os.path.join(PARSERS_DIR, "test_json_xs.pl")]
       },
   "PHP 5.5.36":
       {
           "url":"",
           "commands":["/usr/bin/php", os.path.join(PARSERS_DIR, "test_json.php")]
       },
   "Swift Freddy 2.1.0":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_Freddy_2_1_0/bin/test_Freddy_2_1_0")]
       },
   "Swift Freddy 20160830":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_Freddy_20160830/bin/test_Freddy")]
       },
   "Swift Freddy 20161018":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_Freddy_20161018/bin/test_Freddy")]
       },
   "Swift PMJSON 1.1":
       {
           "url":"https://github.com/postmates/PMJSON",
           "commands":[os.path.join(PARSERS_DIR, "test_PMJSON_1_1_0/bin/test_PMJSON")]
       },
   "Swift PMJSON 1.2":
       {
           "url":"https://github.com/postmates/PMJSON",
           "commands":[os.path.join(PARSERS_DIR, "test_PMJSON_1_2_0/bin/test_PMJSON")]
       },
   "Swift STJSON":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_STJSON/bin/STJSON")]
       },
   "Swift Apple JSONSerialization":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test-AppleJSONSerialization/bin/test-AppleJSONSerialization")]
       },
   "C jsmn":
       {
           "url":"https://github.com/zserge/jsmn",
           "commands":[os.path.join(PARSERS_DIR, "test_jsmn/bin/test_jsmn")]
       },
   "C jansson":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_jansson/bin/test_jansson")]
       },
   "C JSON Checker":
       {
           "url":"http://www.json.org/JSON_checker/",
           "commands":[os.path.join(PARSERS_DIR, "test_jsonChecker/bin/jsonChecker")]
       },
   "C JSON Checker 2":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_jsonChecker2/bin/jsonChecker2")]
       },
   "C ccan":
       {
           "url":"",
           "commands":[os.path.join(PARSERS_DIR, "test_ccan_json/bin/test_ccan")]
       },
   "C cJSON":
       {
           "url":"https://github.com/DaveGamble/cJSON",
           "commands":[os.path.join(PARSERS_DIR, "test_cJSON/bin/test-cJSON")]
       },
   "C JSON Parser by udp":
       {
           "url":"https://github.com/udp/json-parser",
           "commands":[os.path.join(PARSERS_DIR, "test_json-parser/bin/test_json-parser")]
       },
   "Rust json-rust":
       {
           "url":"https://github.com/maciejhirsz/json-rust",
           "commands":[os.path.join(PARSERS_DIR, "test_json-rust/target/debug/tj")]
       },
   "Rust rustc_serialize::json":
       {
           "url":"https://doc.rust-lang.org/rustc-serialize/rustc_serialize/json/index.html",
           "commands":[os.path.join(PARSERS_DIR, "test_json-rustc_serialize/rj/target/debug/rj")]
       },
   "Rust serde_json":
       {
           "url":"https://github.com/serde-rs/json",
           "commands":[os.path.join(PARSERS_DIR, "test_json-rust-serde_json/rj/target/debug/rj")]
       },
   "Java json-simple 1.1.1":
       {
           "url":"",
           "commands":["/usr/bin/java", "-jar", os.path.join(PARSERS_DIR, "test_java_simple_json_1_1_1/TestJSONParsing.jar")]
       },
   "Java gson 2.7":
       {
           "url":"",
           "commands":["/usr/bin/java", "-jar", os.path.join(PARSERS_DIR, "test_java_gson_2_7/TestJSONParsing.jar")]
       },
   "Java com.leastfixedpoint.json 1.0":
       {
           "url":"",
           "commands":["/usr/bin/java", "-jar", os.path.join(PARSERS_DIR, "test_java_com_leastfixedpoint_json_1_0/TestJSONParsing.jar")]
       },
   "Java Jackson 2.8.4":
       {
           "url":"",
           "commands":["/usr/bin/java", "-jar", os.path.join(PARSERS_DIR, "test_java_jackson_2_8_4/TestJSONParsing.jar")]
       },
   "Java nanojson 1.0":
       {
           "url":"",
           "commands":["/usr/bin/java", "-jar", os.path.join(PARSERS_DIR, "test_java_nanojson_1_0/TestJSONParsing.jar")]
       },
   "Haskell Aeson 0.11.2.1":
       {
           "url":"https://github.com/bos/aeson",
           "commands":[os.path.join(PARSERS_DIR, "test_haskell-aeson/testaeson")]
       },
    "Qt JSON":
        {
            "url":"",
            "commands":[os.path.join(PARSERS_DIR, "test_qt/test_qt")]
        },
    "Squeak JSON-tonyg":
        {
            "url":"http://www.squeaksource.com/JSON.html",
            "commands":[
                    os.path.join(PARSERS_DIR, "test_Squeak_JSON_tonyg/Squeak.app/Contents/MacOS/Squeak"),
                    "-headless", #<--optional
                    os.path.join(PARSERS_DIR, "test_Squeak_JSON_tonyg/Squeak5.1-16549-32bit.image"),
                    "test_JSON.st"
            ]
        },
   "Json.NET 9.0.1":
       {
           "url":"http://www.newtonsoft.com/json",
           "commands":["/usr/local/share/dotnet/dotnet", os.path.join(PARSERS_DIR, "test_Json.NET/bin/Release/netcoreapp1.0/publish/test_Json.NET.dll")]
       },
}

def run_tests(restrict_to_path=None):
    
    FNULL = open(os.devnull, 'w')
    log_file = open(LOG_FILE_PATH, 'w')
    
    prog_names = list(programs.keys())
    prog_names.sort()
        
    for prog_name in prog_names:
        d = programs[prog_name]
        
        url = d["url"]
        commands = d["commands"]

        if not os.path.exists(commands[0]):
            print("-- skip non-existing", commands[0])
            continue
        
        for root, dirs, files in os.walk(TEST_CASES_DIR_PATH):
            json_files = (f for f in files if f.endswith(".json"))
            for filename in json_files:
            
                if restrict_to_path:
                    restrict_to_filename = os.path.basename(restrict_to_path)
                    if filename != restrict_to_filename:
                        continue
            
                file_path = os.path.join(root, filename)
                
                my_stdin = FNULL
                
                use_stdin = "use_stdin" in d and d["use_stdin"]
                if use_stdin:
                    my_stdin = open(file_path, "rb")
                    a = commands
                else:
                    a = commands + [file_path]

                #print("->", a)
                print("--", " ".join(a))
                
                try:
                    status = subprocess.call(
                        a,
                        stdin=my_stdin,
                        stdout=FNULL,
                        stderr=subprocess.STDOUT,
                        timeout=5
                    )
                    #print("-->", status)
                except subprocess.TimeoutExpired:
                    print("timeout expired")
                    s = "%s\tTIMEOUT\t%s" % (prog_name, filename)
                    log_file.write("%s\n" % s)
                    print("RESULT:", result)
                    continue

                if use_stdin:
                    my_stdin.close()
                                                
                result = None
                if status == 0:
                    result = "PASS"
                elif status == 1:
                    result == "FAIL"
                else:
                    result = "CRASH"
                
                s = None
                if result == "CRASH":
                    s = "%s\tCRASH\t%s" % (prog_name, filename)
                elif filename.startswith("y_") and result != "PASS":
                    s = "%s\tSHOULD_HAVE_PASSED\t%s" % (prog_name, filename)
                elif filename.startswith("n_") and result == "PASS":
                    s = "%s\tSHOULD_HAVE_FAILED\t%s" % (prog_name, filename)
                elif filename.startswith("i_") and result == "PASS":
                    s = "%s\tIMPLEMENTATION_PASS\t%s" % (prog_name, filename)
                elif filename.startswith("i_") and result != "PASS":
                    s = "%s\tIMPLEMENTATION_FAIL\t%s" % (prog_name, filename)

                if s != None:
                    print(s)
                    log_file.write("%s\n" % s)
    
    FNULL.close()
    log_file.close()

def f_underline_non_printable_bytes(bytes):
	
    html = ""

    has_non_printable_characters = False
    
    for b in bytes:
        
        is_not_printable = b < 0x20 or b > 0x7E
        
        has_non_printable_characters |= is_not_printable
        
        if is_not_printable:
            html += "<U>%02X</U>" % b
        else:
            html += "%c" % b
    
    if has_non_printable_characters:
        try:
            html += " <=> %s" % bytes.decode("utf-8", errors='ignore')
        except:
            pass
        
    if len(bytes) > 36:
        return "%s(...)" % html[:36]
    
    return html
    
def f_status_for_lib_for_file(json_dir, results_dir):

    txt_filenames = [f for f in listdir(results_dir) if f.endswith(".txt")]
    
    # comment to ignore some tests
    statuses = [
        "SHOULD_HAVE_FAILED",

        "SHOULD_HAVE_PASSED",
        "CRASH",

        "IMPLEMENTATION_FAIL",
        "IMPLEMENTATION_PASS",
        
        "TIMEOUT"
    ]
    
    d = {}
    libs = []
        
    for filename in txt_filenames:
        path = os.path.join(results_dir, filename)
        
        with open(path) as f:
            for l in f:
                comps = l.split("\t")
                if len(comps) != 3:
                    continue

                if comps[1] not in statuses:
                    print("-- unhandled status:", comps[1])

                (lib, status, json_filename) = (comps[0], comps[1], comps[2].rstrip())
                
                if lib not in libs:
                    libs.append(lib)
                
                json_path = os.path.join(TEST_CASES_DIR_PATH, json_filename)
                
                if json_path not in d:
                    d[json_path] = {}
                                    
                d[json_path][lib] = status
    
    return d, libs

def f_status_for_path_for_lib(json_dir, results_dir):
    
    txt_filenames = [f for f in listdir(results_dir) if f.endswith(".txt")]
    
    # comment to ignore some tests
    statuses = [
        "SHOULD_HAVE_FAILED",

        "SHOULD_HAVE_PASSED",
        "CRASH",

        "IMPLEMENTATION_FAIL",
        "IMPLEMENTATION_PASS",
        
        "TIMEOUT"

    ]
    
    d = {} # d['lib']['file'] = status
    
    for filename in txt_filenames:
        path = os.path.join(results_dir, filename)
        
        with open(path) as f:
            for l in f:
                comps = l.split("\t")
                if len(comps) != 3:
                    continue
                
                if comps[1] not in statuses:
                    #print "-- unhandled status:", comps[1]
                    continue
                
                (lib, status, json_filename) = (comps[0], comps[1], comps[2].rstrip())
                
                if lib not in d:
                    d[lib] = {}
                
                json_path = os.path.join(TEST_CASES_DIR_PATH, json_filename)

                d[lib][json_path] = status
    
    return d

def f_tests_with_same_results(libs, status_for_lib_for_file):

    tests_with_same_results = {} #{ {lib1:status, lib2:status, lib3:status} : { filenames } }

    files = list(status_for_lib_for_file.keys())
    files.sort()
    
    for f in files:
        prefix = os.path.basename(f)[:1]
        lib_status_for_file = []
        for l in libs:
            if l in status_for_lib_for_file[f]:
                status = status_for_lib_for_file[f][l]
                lib_status = "%s_%s" % (status, l)
                lib_status_for_file.append(lib_status)
        results = " || ".join(lib_status_for_file)
        if results not in tests_with_same_results:
            tests_with_same_results[results] = set()
        tests_with_same_results[results].add(f)
    
    r = []
    for k,v in tests_with_same_results.items():
        r.append((k,v))
    r.sort()
    
    return r

def generate_report(report_path, keep_only_first_result_in_set = False):

    (status_for_lib_for_file, libs) = f_status_for_lib_for_file(TEST_CASES_DIR_PATH, LOGS_DIR_PATH)
    
    status_for_path_for_lib = f_status_for_path_for_lib(TEST_CASES_DIR_PATH, LOGS_DIR_PATH)
    
    tests_with_same_results = f_tests_with_same_results(libs, status_for_lib_for_file)
        
    with open(report_path, 'w') as f:
    
        f.write("""<!DOCTYPE html>
        
        <HTML>
        
        <HEAD>
            <TITLE>JSON Parsing Tests</TITLE>
            <LINK rel="stylesheet" type="text/css" href="style.css">
            <META charset="UTF-8">
        </HEAD>
        
        <BODY>
        """)

        prog_names = list(programs.keys())
        prog_names.sort()
        
        libs = list(status_for_path_for_lib.keys())
        libs.sort()

        title = "JSON Parsing Tests"
        if keep_only_first_result_in_set:
            title += ", Prunned"
        else:
            title += ", Full"
        f.write("<H1>%s</H1>\n" % title)
        f.write('<P>Appendix to: seriot.ch <A HREF="http://www.seriot.ch/parsing_json.php">Parsing JSON is a Minefield</A> http://www.seriot.ch/parsing_json.php</P>\n')
        f.write("<PRE>%s</PRE>\n" % strftime("%Y-%m-%d %H:%M:%S"))

        f.write("""<H4>Contents</H4>
        <OL>
        <LI><A HREF="#color_scheme">Color Scheme</A>
        <LI><A HREF="#all_results">Full Results</A>
        <LI><A HREF="#results_by_parser">Results by Parser</A>""")
        f.write("<UL>\n")
        for i, prog in enumerate(prog_names):
            f.write('    <LI><A HREF="#%d">%s</A>\n' % (i, prog))
        f.write("</OL>\n")

        f.write("""
        <A NAME="color_scheme"></A>
        <H4>1. Color scheme:</H4>
        <TABLE>
            <TR><TD class="EXPECTED_RESULT">expected result</TD><TR>
            <TR><TD class="SHOULD_HAVE_PASSED">parsing should have succeeded but failed</TD><TR>
            <TR><TD class="SHOULD_HAVE_FAILED">parsing should have failed but succeeded</TD><TR>
            <TR><TD class="IMPLEMENTATION_PASS">result undefined, parsing succeeded</TD><TR>
            <TR><TD class="IMPLEMENTATION_FAIL">result undefined, parsing failed</TD><TR>
            <TR><TD class="CRASH">parser crashed</TD><TR>
            <TR><TD class="TIMEOUT">timeout</TD><TR>
        </TABLE>
        """)
        
        ###
        
        f.write('<A NAME="all_results"></A>\n')
        f.write("<H4>2. Full Results</H4>\n")
        f.write("<TABLE>\n")
        
        f.write("    <TR>\n")
        f.write("        <TH></TH>\n")
        for lib in libs:
            f.write('        <TH class="vertical"><DIV>%s</DIV></TH>\n' % lib)
        f.write("        <TH></TH>\n")
        f.write("    </TR>\n")
        
        for (k, file_set) in tests_with_same_results:
            
            ordered_file_set = list(file_set)
            ordered_file_set.sort()
            
            if keep_only_first_result_in_set:
                ordered_file_set = [ordered_file_set[0]]
            
            for path in [path for path in ordered_file_set if os.path.exists(path)]:
            
                f.write("    <TR>\n")
                f.write('        <TD>%s</TD>' % os.path.basename(path))
                
                status_for_lib = status_for_lib_for_file[path]
                bytes = open(path, "rb").read()
            
                for lib in libs:
                    if lib in status_for_lib:
                        status = status_for_lib[lib]
                        f.write('        <TD class="%s">%s</TD>' % (status, ""))
                    else:
                        f.write('        <TD class="EXPECTED_RESULT"></TD>')
                f.write('        <TD>%s</TD>' % f_underline_non_printable_bytes(bytes))
                f.write("    </TR>")
        
        f.write("</TABLE>\n")
        
        
        ###
        
        f.write('<A NAME="results_by_parser"></A>\n')
        f.write("<H4>3. Results by Parser</H4>")
        for i, prog in enumerate(prog_names):
            url = programs[prog]["url"]
            f.write("<P>\n")
            f.write('<A NAME="%d"></A>' % i)
            if len(url) > 0:
                f.write('<H4><A HREF="%s">%s</A></H4>\n' % (url, prog))
            else:
                f.write('<H4>%s</H4>\n' % prog)

            ###
            
            if prog not in status_for_path_for_lib:
                continue
            status_for_path = status_for_path_for_lib[prog]

            paths = list(status_for_path.keys())
            paths.sort()

            f.write('<TABLE>\n')

            f.write("    <TR>\n")
            f.write("        <TH></TH>\n")
            f.write('        <TH class="space"><DIV></DIV></TH>\n')
            f.write("        <TH></TH>\n")
            f.write("    </TR>\n")
            
            for path in paths:
                    
                f.write("    <TR>\n")
                f.write("        <TD>%s</TD>" % os.path.basename(path))
                
                status_for_lib = status_for_lib_for_file[path]
                if os.path.exists(path):
                    bytes = open(path, "rb").read()
                else:
                    bytes = [ord(x) for x in "(MISSING FILE)"]

                if prog in status_for_lib:
                    status = status_for_lib[prog]
                    f.write('        <TD class="%s">%s</TD>' % (status, ""))
                else:
                    f.write("        <TD></TD>")
                f.write("        <TD>%s</TD>" % f_underline_non_printable_bytes(bytes))
                f.write("    </TR>")
    
            f.write('</TABLE>\n')
            f.write("</P>\n")
        
        ###

        f.write("""
        
        </BODY>
        
        </HTML>
        """)
    
    os.system('/usr/bin/open "%s"' % report_path)

###

if __name__ == '__main__':
    
    restrict_to_path = None
    if len(sys.argv) == 2:
        restrict_to_path = os.path.join(BASE_DIR, sys.argv[1])
        if not os.path.exists(restrict_to_path):
            print("-- file does not exist:", restrict_to_path)
            sys.exit(-1)
    
    run_tests(restrict_to_path)
    
    generate_report(os.path.join(BASE_DIR, "results/parsing.html"), keep_only_first_result_in_set = False)
    generate_report(os.path.join(BASE_DIR, "results/parsing_pruned.html"), keep_only_first_result_in_set = True)
