# Test environments
* local debian 10 buster install, R 4.0.5
* win-builder (devel and release)

# R CMD check results
There are no ERRORs or WARNINGs.

There was 1 NOTE:

   "installed size is  7.3Mb
   sub-directories of 1Mb or more:
   libs   7.1Mb"

This is expected, since this package installs a small library for child process communication.

# Downstream dependencies
There are currently no downstream dependencies for this package.

