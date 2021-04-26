OCZip
=====

Welcome to the documentation for OCZip, a file compression tool written in OCaml.

This project was written by Gavin Gray for CS 6963 at the University of Utah.

Building
--------

This project depends on the following libraries

* core

* batteries
  > *only for BatVect.t* 

* async

* stdint

run `dune build`

User Guide
----------

```
OCZip, file compression in OCaml

  oczip.exe SUBCOMMAND

=== subcommands ===

  -unzip   Unpack zip archive
  -zip     Concurrent file compression to ZIP archive
  version  print version information
  help     explain a given subcommand (perhaps recursively)
```

A quick test for seeing results is:

`make stress`

Some files included in the stress test contain highly compressible data and others
demonstrate the pitfalls of the current OCZip state. These drawbacks are mentioned
in the [future work](#future-work) section.

Testing
-------

I have written a few (emphasis on *a few*) tests to make sure some internal things are working
well. To force run all of the tests use `dune test -f`.

Future Work
-----------
:exclamation: Currently a few features are lacking and I'm hoping that this will change in the coming months:

* DEFLATE : my current implementation of the deflate algorithm strictly uses static huffman codes.
In the future, I hope to implement usage of dynamic huffman codes then even later breaking up the 
input stream into blocks and using the best technique for each block.

* DEFLATE : my current implementation of the deflate algorithm is *slow* on large files (and can
cause a stack overflow on others). This is clearly a problem.

* UNZIP : there is no support for unzipping archives. This choice was to better focus on some other
features (e.g. making the deflate faster and using the `Deferred.t` monad to incorporate concurrency)
rather than worry about reading a zip file (which is largely stateful and uninteresting for this course).

* TESTS : write more (and better) tests.

* UTILITY : allow users to add a file to an existing zip archive.

* etc ...

Cheers! :beers:

