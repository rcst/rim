# Test environments
* local debian 11 bullseye install, R 4.2.0
* win-builder (devel, release and oldrel)
* Fedora 34 Virtual Machine

# R CMD check results
* There are no ERRORs or WARNINGs.

# Downstream dependencies
There are currently no downstream dependencies for this package.

## Resubmission
This is a resubmission. In this version I have:

* fixed NOTE about comparison of returned value from class() to a character vector, now using function isa()
