# rmaxima 0.4.0/ rim 0.4.0
- added `maxima.engine.format()` function that can be used to change the output format of the knitr engine
- fixed issue that arises when Maxima takes longer to start (which previously caused the interface to freez)
- added functions to record the version number of Maxima that is being used
- renamed package to "rim" to avoid confusion with maxima's "rmaxima"
- communiction is now implemented using sockets and the processing is handles by two nested R6 classes
- return type is now a S3 class of type maxima. There are two methods `iprint()` and `oprint()` for printing an maxima S3 object: printing the input command and output respectively, including reference labels. 
- removed external C++ library dependencies (now using sockets for communicating with Maxima)
- knitr engine now prints output after each input line, it also prints the input reference label infront of the command

# rmaxima 0.0.0.9000

- removed depency from boost (switched to libexecstream)
- maxima.get() returns character vector with added attributes for reference labels, format and it's originating command
- fixed Maxima error (message) forwarding to R
- implement knitr-engine for maxima using this interface
- Implement stop function to end maxima child process for debugging purposes
- Handles asksign and similar feedback interuptions
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
