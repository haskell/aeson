# Benchmarks

The benchmarks in this directory are generally very simple. Just run
`make` to build them, and then each one takes the same arguments:

    ./BenchmarkName NumberOfIterations FileToParse

For instance:

    ./AesonParse 1000 json-data/twitter100.json

In each instance, the command should print the amount of time it took.
