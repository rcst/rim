# Test environments
* local debian 12 bookworm install, R 4.3.1
* win-builder (devel, release and oldrel)
* Fedora 36 (devel + clang)
* Fedora 38 (devel)

# R CMD check results
* There are no ERRORs or WARNINGs.

# Downstream dependencies
There are currently no downstream dependencies for this package.

# previous issues
* removed path normalization, disabled throwing an error when including Maxima 
  image file paths and reverted to relative file paths for the same when running 
  Maxima code chunks in a RMarkdown document.
* the above test had failed for flavors r-devel-linux-x86_64-fedora-clang/gcc,
  the failure was due to Maxima returning a file path to an image that isn't
  guaranteed to exist at the same time the path is returned (and for the test 
  case under the above flavor doesn't).
