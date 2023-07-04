# Test environments
* local debian 12 bookworm install, R 4.3.1
* win-builder (devel, release and oldrel)
* Fedora 36 (devel + clang)

# R CMD check results
* There are no ERRORs or WARNINGs.

# Downstream dependencies
There are currently no downstream dependencies for this package.

# previous issues
* changed unit test (tests/testthat/test-engine.R) of comparing two digests to
  comparing two character vectors.
* the above test had failed for flavors r-devel-linux-x86_64-fedora-clang/gcc,
  the failure was due to an incorrect detection of html output when using the
  knir engine to render the test document into plain markdown under Fedora 36,
  this has been corrected by a more robust detection.
* added small executable examples for each exported function - provided 'Maxima' is installed.
