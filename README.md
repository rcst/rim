# rmaxima
Development version of the yet-to-be-released rmaxima CRAN package. rmaxima provides an interface to the powerful and fairly complete maxima computer algebra system

This repository is a fork from the original version created by Kseniia Shumelchyk and Hans W. Borcher [shumelchyk/rmaxima](https://github.com/shumelchyk/rmaxima), which is currently not maintained.

# Installation

## Requirements
- Maxima
- Boost library (>=1.67.0) 

# TODO
## General
- If command terminates with `\$` then this causes a segmentation fault, which kills the R process. The cause being that maxima returns immediatley with the next input prompt
- dealing with documentation/ help functions
	- write corresponding class functions
	- Help functions in maxima:
		- `example(...)`
		- `describe(...)` and variants `? ...` aka describe(..., exact) and `?? ...` aka describe(..., inexact)
		- `apropos("...")`
		- `demo("...")`
	- catch exceptions
		- `?? ...` return message that ?? ... or descripe(..., inexact) are not supporte since it enters a user dialog and where user needs to choose different option and this is not compatible with using it in documents


## Usability
- Handle asksign and similar feedback interuptions
- enable replacing latex math delimiters "$$" by more space-consistent variants, e.g. "\[" or `align` environment
- Provide an interface to plot2d() and plot3d() enableling storing of graphics and usage in RMarkdown documents
- Workflow for working with TeX and MathML output:
	- enable: dynamically change output display: 1d, tex, mathml
- Add documentation
- implement maxima return type class (S3, S4 or RC?)
	- maxima returns always a class of this type
	- the type stores different format types of output strings, i.e. linear, latex, MathML,
	- the type also stores a reference output label
	- this would be a nice way to handle surpressed outputs from maxima
	- maybe be implement certain operations on thos classes
		- insert parameter values and return calculated function values
		- enable to reinsert objects of this time back into latex

## Portability
- Remove third party linking entirely
	- remove dependency from boost::filesystem: use R functions instead
- fix and test for windows, test for MacOS

## Efficiency
- Remove unneccassary variable conversions from `std::string` to `boost::filesystem::path` and vice versa

## Documentation
