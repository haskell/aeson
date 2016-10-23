# JSON Parsing Test Suite
A comprehensive test suite for RFC 7159 compliant JSON parsers

This repository was created as an appendix to the article [Parsing JSON is a Minefield 💣](http://seriot.ch/parsing_json.php).

**/parsers/**

This directory contains several parsers and tiny wrappers to turn the parsers into JSON validators, by returning a specific value.

- `0` the parser did accept the content
- `1` the parser did reject the content
- `>=1` the process did crash
- `timeout` happens after 5 seconds

**/test\_parsing/**

The name of these files tell if their contents should be accepted or rejected.

- `y_` content must be accepted by parsers
- `n_` content must be rejected by parsers
- `i_` parsers are free to accept or reject content

**/test\_transform/**

These files contain weird structures and characters that parsers may understand differently, eg:

- huge numbers
- dictionaries with similar keys
- NULL characters
- escaped invalid strings

These files were used to produce `results/transform.html`.

**/run_tests.py**

Run all parsers with all files:

    $ python3 run_tests.py

Run all parsers with a specific file:

    $ python3 run_tests.py file.json

The script writes logs in `results/logs.txt`.

The script then reads `logs.txt` and generates `results/parsing.html`.

**/results/**

<img src="results/pruned_results.png" alt="JSON Parsing Tests" />
