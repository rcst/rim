# rmaxima
Development version of the yet-to-be-released rmaxima CRAN package. rmaxima provides an interface to the powerful and fairly complete maxima computer algebra system

This repository is a fork from the original version created by Kseniia Shumelchyk and Hans W. Borcher [shumelchyk/rmaxima](https://github.com/shumelchyk/rmaxima), which is currently not maintained.

# Installation

## Requirements
- Maxima
- Boost library (>=1.67.0) 

# TODO
## General
- dealing with documentation/ help functions
	- write corresponding class functions
	- Help functions in maxima:
		- `example(...)`
		- `describe(...)` and variants `? ...` aka describe(..., exact) and `?? ...` aka describe(..., inexact)
		- `apropos("...")`
		- `demo("...")`
	- catch exceptions
		- `?? ...` return message that ?? ... or descripe(..., inexact) are not supporte since it enters a user dialog and where user needs to choose different option and this is not compatible with using it in documents

- If command terminates with `\$` then this causes a segmentation fault, which kills the R process. The cause being that maxima returns immediatley with the next input prompt

## Usability
- enable replacing latex math delimiters "$$" by more space-consistent variants, e.g. "\[" or `align` environment
- executing help functions (?, decribe, apropos, ...) crashes the current R session via segmentation fault
- Provide an interface to plot2d() and plot3d() enableling storing of graphics and usage in RMarkdown documents
- Handle asksign and similar feedback interuptions
- Workflow for working with TeX and MathML output:
	- enable: dynamically change output display: 1d, tex, mathml
- Add documentation

## Portability
- Remove third party linking entirely
	- remove dependency from boost::filesystem: use R functions instead
- fix and test for windows, test for MacOS

## Efficiency
- fix failing call to `system.file()` inside Rmaxima class constructor when using `devtools::load_all()`
- Remove unneccassary variable conversions from `std::string` to `boost::filesystem::path` and vice versa

## Documentation
- Add roxygen2 documentation to C++-classes or wrap c++ calls in more userfriendly R-function calls

# Version History

## 26.02.2021
- Add test files

## 22.02.2021
- implemented output display as tex (not yet user-friendly)
- added initialization files

## 19.02.2021
- added function void `loadModule(const std::string &s)`

## 18.02.2021
- Removed boost::regex linking dependency


## 16.02.2021
- fixed pipe stream synchronisation

## Initial Commit

- MaximaChain.h: changed signature of `MaximaChain::Reply::Reply(std::istream &in)` --> `MaximaChain::Reply::Reply(bp::pistream &in)`
- MaximaChain.cpp changed constructor `MaximaChain::MaximaChain(...)`: `bp::child(maximaPath, args, ctx)`
- MaximaChain.cpp changed all `process->get_stdout()` to is
- Rmaxima.cpp : changed `std::string` `maxpath = bp::find_executable_in_path(maxima");` to `std::string maxpath = bp::search_path("maxima").string();`
- MaximaChain.h: changed class vaiable `size_t pid` to conform with bp::child's: `pid_t pid;`
- MaximaChain.cpp: changed class for setting pid: `pid = process->get_id();` to `pid = process->id()`; --> CAUTION: Still need to fix Windows version!
- MaximaChain.h/cpp: changed `MaximaChain::getId()` signature to return `type bp::pid_t` and `boost::process::pid_t`, respectively
- MaximaChain.h/.cpp: Added a default constructor `Reply::Reply()` cause it is needed and causes "no matching function call `MaximaChain::Reply::Reply()` errror
- MaximaChain.cpp: Substituted all exceptions throws with shortcut Rcpp::stop()-calls
- Rmaxima.cpp: Changed function call `execute`, removed try-catch block
