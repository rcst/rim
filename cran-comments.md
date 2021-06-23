# Test environments
* local debian 10 buster install, R 4.1.0
* Windows 10 Virtual Machine
* win-builder (devel and release)

# R CMD check results
* There are no ERRORs or WARNINGs.

# Downstream dependencies
There are currently no downstream dependencies for this package.

## Resubmission
This is a resubmission. In this version I have:

* single-quoted Maxima other software in the DESCRIPTION file
* written tests that are skipped if Maxima is not installed
* added the required the required version of Maxima (with which I have been testing)
* added forwarding of warning messages from Maxima to R, including a test that is also skipped if Maxima is not installed
