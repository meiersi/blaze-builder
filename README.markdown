blaze-builder
=============

TODO: Update this description.

This is a string builder in Haskell. The goal is to support fast string
concatenation. This builder is optimized for the generation of HTML, meaning:

- it is optimized for appending many small parts;
- it natively supports UTF-8 encoding;
- it natively supports HTML entity escaping.

This builder was created for the blaze-html templating system.

Run the benchmarks:

    make benchmark

Run the test suite:

    make test
