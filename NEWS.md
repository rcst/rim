# rmaxima 0.0.0.9000

- Added interface to maxima's apropos()-function
- Fixed: If command terminates with `\$` then this causes a segmentation fault, which kills the R process. The cause being that maxima returns immediatley with the next input prompt
- added roxygen2 documentation
- fixed `system.file` call inside constructor to work with `devtools::load_all()`
- Added a `NEWS.md` file to track changes to the package.
- Add test files
- implemented output display as tex (not yet user-friendly)
- added initialization files
- added function void `loadModule(const std::string &s)`
- Removed boost::regex linking dependency
- fixed pipe stream synchronisation
