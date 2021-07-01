# Test environments
* local debian 10 buster install, R 4.1.0
* Windows 10 Virtual Machine
* win-builder (devel, release and oldrel)
* Fedora 34 Virtual Machine

# R CMD check results
* There are no ERRORs or WARNINGs.

# Downstream dependencies
There are currently no downstream dependencies for this package.

## Resubmission
This is a resubmission. In this version I have:

* fixed installation error that occured under Fedora (this should also fix the same installation error under Solaris)
* added more specific information on SystemRequirements field in DESCRIPTION
* added checks for SystemRequirements in .onAttach()
