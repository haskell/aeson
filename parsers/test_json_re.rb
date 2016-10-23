# encoding: utf-8

f = ARGV[0]

if f.end_with? "n_structure_open_array_object.json" # would hang the regex
    exit 2
end

# http://stackoverflow.com/questions/2583472/regex-to-validate-json
JSON_VALIDATOR_RE = /(
    # define subtypes and build up the json syntax, BNF-grammar-style
    # The {0} is a hack to simply define them as named groups here but not match on them yet
    # I added some atomic grouping to prevent catastrophic backtracking on invalid inputs
    (?<number>  -?(?=[1-9]|0(?!\d))\d+(\.\d+)?([eE][+-]?\d+)?){0}
    (?<boolean> true | false | null ){0}
    (?<string>  " (?>[^"\\\\]* | \\\\ ["\\\\bfnrt\/] | \\\\ u [0-9a-f]{4} )* " ){0}
    (?<array>   \[ (?> \g<json> (?: , \g<json> )* )? \s* \] ){0}
    (?<pair>    \s* \g<string> \s* : \g<json> ){0}
    (?<object>  \{ (?> \g<pair> (?: , \g<pair> )* )? \s* \} ){0}
    (?<json>    \s* (?> \g<number> | \g<boolean> | \g<string> | \g<array> | \g<object> ) \s* ){0}
    )
    \A \g<json> \Z
    /uix

begin
    data = File.read(f)
    puts(data)
    if data.match(JSON_VALIDATOR_RE)
        exit 0
    else
        exit 1
    end
end
