# rmaxima
Automatically exported from code.google.com/p/rmaxima

== RMaxima Project Documentation ==

  |   *Authors*: Kseniia Shumelchyk and Hans W. Borchers <br>
  |   *Date*:    August 20, 2012
  |   *Copyright*:    2012 - 2015  

=== Introduction ===

The goal of the project RMaxima is to create a package that allows to use from within R the capabilities of Maxima, a Computer Algebra System that is more powerful than the symbolic algebra systems available for R right now. The package should add new functionality to R for using symbolic expressions, such as equation solving, differentiation, integration, three-dimensional plotting, or multiple-precision arithmetics.

=== Project Description ===

At the beginning of the project, existing approaches to communicate with the Maxima process were explored and analyzed  Two ways of interaction were identified: The first one based on TCP sockets, and the second one based on using the mechanism of pipes. Similar API BSD sockets are available in most modern operating systems, but some separate methods are required to start the Maxima process.

In case of using unnamed pipes portability is provided automatically, but will depend on additional libraries. Based on some prior know-how it was natural to look at Boost.Process which is associated with, but not officially part of, the well-known C++ [http://www.boost.org Boost] library. Possible alternatives were, among others, [http://libexecstream.sourceforge.net/ Libexecstream] or [http://pstreams.sourceforge.net/ PStreams].

For the development of this package unnamed pipes were chosen as a way of allowing R to interact with the Maxima process. After connecting to Maxima there is the challenge of processing the output of Maxima. The communication protocol via Maxima console is in principle quite simple: The user repeatedly enters expressions and Maxima produces some result and returns it as a string. This message exchange pattern may be described as 'request-response'.

To simplify the processing, Maxima output is controlled by an option file (`display.lisp`). It configures Maxima output in such way that allows to identify different parts of the output coming from Maxima. Result expressions are marked in a special way and displayed in the one-dimensional, i.e. non-graphical mode. Parsing the output is realized through regular expressions provided by the library Boost.Regex.

All this has been implemented in a demo system at the end of the first coding phase, except that the communication between R and Maxima consoles is more batch-like and not yet interactive. During the second coding phase, a true request-response interaction between the R console and the Maxima process has been realized with the help of the `Rcpp` package.

The original project plan, taken from the project proposal, is enclosed here.

*Project Plan (first phase)*

|| *Week* || *Timeline* || *Activity* || 
|| 1 || April 23 - May 10   || Reading the documentation of Maxima and R, getting familiar with the project.  || 
|| 2 || May 11 - May 20     || Discussing a project architecture with a mentor.  || 
|| 3 || May 21 - June 3     || Creating a simple Maxima "communication manager" with unit tests.  || 
|| 4 || June 4 - June 10    || Code review of the managers code; testing and bugfixes  || 
|| 5 || June 11 - June 19   || Extending Maxima communication manager on Windows and Mac OS  || 
|| 6 || June 20 - June 22   || Testing and bugfix  || 
|| 7 || June 23 - July 2    || Adding ability to send commands from R project interpreter to maxima and getting the result  || 
|| 8 || July 3 - July 4     || Code review and making changes  || 
|| 9 || July 5 - July 8     || Testing and bugfix  || 

*Project Plan (second phase)*

|| *Week* || *Timeline* || *Activity* || 
|| 10 || July 16 - July 20 || Controlling Maxima (start, stop) and user interactivity ||
|| 11 || Juli 23 - July 27 || Handling Maxima error messages and Maxima plots ||
|| 12 || July 30 - August 3 || Returning Maxima results to R; filtering commands ||
|| 13 || August 6 - August 10 || Using Rcpp for keeping C++ pointers ||
|| 14 || August 13 - August 17 || RMaxima Vignette; code documentation ||


=== Project Results (1st Coding Phase) ===

The result of the first coding phase of the project is a demo program proving the feasibility of the approach described above. When all tools needed are installed (Boost, Boost process library, Maxima) as listed below, the demo library can be compiled as a shared library for R and then dynamically loaded into R with the `dyn.load()` function.

The demo program will automatically load the RMaxima shared library and then send some simple commands to Maxima. The result strings returned from Maxima will be displayed on the R command terminal. Here is an example, with the symbolic commands `float`, `sin + cos`, `plot2d`, and `diff` send to Maxima:

<code language="r">
    > source("maximaexe.R")
    # Calling C function
    >>> float(1/3);
    0.33333333333333
    # Returning to R

    # Calling C function
    >>> sin(%pi/2) + cos(%pi/3);
    3/2
    # Returning to R

    # Calling C function
    >>> plot2d(x^2 - x + 3, [x, -10, 10]);
    ""
    # Returning to R

    # Calling C function
    >>> diff(sin(x), x);
    cos(x)
    # Returning to R
</code>

The graphics window, opened by Maxima through the `plot2d` command, will only shortly pop up and then disappear again. It has to be seen how graphical output from Maxima can be kept open until the user closes the window..

The other commands return correct results from Maxima, at the moment available only as strings. To call Maxima interactively from the R command line will be one of the next tasks.


=== Project Results (2nd Coding Phase) ===

The result of the second coding phase of the project is a program providing mechanism of interaction with CAS Maxima. When all tools needed are installed (Boost, Boost process library, Maxima, Rcpp package) as listed below, the library can be compiled as a shared library for R and then used from R as Rcpp module.

The program requires some initial R command that allows further using Maxima in interactive mode.

<code language="r">
    require(Rcpp)
    m <- Module("Maxima", dyn.load("Rmaxima.so"))
    mx.start <- function() {
        mxm <<- new(m$RMaxima)
    }
    mx.exec <- function(x) {
        mxm$execute(as.character(x))
    }
</code>

Before executing Maxima we need to start Maxima using command:

<code language="r">
    mx.start()</code>

After Maxima is started we can send any commands to Maxima. The result strings returned from Maxima will be displayed on the R command terminal. Also we can keep results obtained as a strings in R variables.

Below is an example, with the symbolic commands float, sin + cos,plot2d, diff send to Maxima. Also RMaxima allows to refer to the latest result through the % character, and to any previous input or output by its respective prompted %i (input) or %o (output). For example:

<code language="r">
    > mx.exec("9+7")
    [1] "16"
    > mx.exec("% - 10")
    [1] "6"
    > mx.exec("%o1 * 2")
    [1] "32"
    > mx.exec("float(1/3)")
    [1] "0.33333333333333"
    > mx.exec("sin(%pi/2)+cos(%pi/3)")
    [1] "3/2"
    > result = mx.exec("diff(sin(x),x)")
    > result
    [1] "cos(x)"
</code>

The graphics window, opened by Maxima through the plot2d or plot3d commands, is seen how graphical output from Maxima and stay open until the user closes it manually. A new plot command will overwrite the old plot.
Here is the examples of executing plot commands:

<code language="r">
    > mx.exec("plot2d(x^2-x+3,[x,-10,10])")
    [1] "\"\""
    > mx.exec("f(x,y):= sin(x)+cos(y)")
    [1] "f(x,y):=sin(x)+cos(y)"
    > mx.exec("plot3d(f(x,y),[x,-5,5],[y,-5,5])")
    [1] "false"
</code>

Maxima calls Gnuplot for plotting, so you will see the following graphics windows. The 3d graph can be zoomed and rotated in three-dimensional space by the user.

http://rmaxima.googlecode.com/files/plot2d.png

http://rmaxima.googlecode.com/files/plot3d.png

The other commands return correct results from Maxima, at the moment available only as strings.

RMaxima handle error messages generated by Maxima as well as errors that are generated by when the calls to Maxima returns an error:

<code language="r">
    > mx.exec("2+2;;")
    Error in mxm$execute(as.character(x)) : 
      Bad expression: only one ;|$ terminated expression at a time is allowed
    > mx.exec("asd+-*;")
    Error in mxm$execute(as.character(x)) : 
      Maxima error: incorrect syntax: * is not a prefix operator
    asd+-*;
</code>

Created program allows to call Maxima commands interactively, one command after the other, defining variables that are still alive during the next user command.


=== Problems Encountered ===

==== Maxima ====

Automatic processing of Maxima console output is quite complex: it's difficult to impose a general structure on Maxima output that will be valid for all kinds of requests sent to Maxima. According to documentation of the Maxima distribution the system has a number of variables that control the allocation of significant fragments of console output, which can significantly simplify the automatic processing of Maxima output. Here are the variables we used and their meanings:

   (1) `prompt-prefix` - the string is printed before each input request;<br>
   (2) `prompt-suffix` - the string is printed after each input request;<br>
   (3) `alt-display2d` - a function that replaces the standard Maxima in two-dimensional mode when printing expressions.

Setting these variables happens in the file `display.lisp`, written in Common Lisp (the programming language the kernel of Maxima is written in), and the path to that file is provided with the option `.p <File Path>`. The content of this file is loaded during Maxima booting.

==== Boost ====

Also there were problems with using the Boost.Process library. For correctly working functions from Boost.Process required the Boost.Filesystem version 2, but latest Boost versions by default use version 3. So for using Boost.Process these things had to be fixed manually, and the fixes are included in the Boost Process files distributed with RMaxima.

The [http://www.highscore.de/boost/process/ Boost Process] library was rejected by the Boost core team in March 2011 and has since then not been included into Boost officially. A consequence is that it must be downloaded and installed separately. That poses the problem of how to make RMaxima become a package distributed on CRAN. By the way, we were in contact with the developer of Boost.Process and he indicated he is still working on the library at times.

[Remark: As Dirk Edelbuettel told us, the CRAN platform for Linux is "Debian testing" which includes 'boostlib', the standard Boost library. For Windows he provides Uwe Ligges with files and Boost headers such that the compilation process works on that platform too. (Mac OS X ?)]

==== Interactive Mode ====

There were several different ideas about implementing interactive mode. The main question was how to correctly store C++ object in R.

The first version of interactive mode (RMaxima-0.2) based on creating mx.exec() function that call C++ function, execute Maxima command and get result. But it was not really interactive because Maxima process was alive only for each C++ command called from R.

The second approach was based on supporting Asynchronous input/output operations for Maxima process. But after research this approach was rejected. At first, Boost.Process library don’t have appropriate mechanism and uses Boost.Asio library for Asynchronous I/O. At second, this approach is also not guarantee interactive mode.

After some additional survey we decide to go forward with Rcpp package that allows store C++ objects in R and expose C++ classes in R through Rcpp modules (RMaxima-0.3). For our further goals using Rcpp would be a very helpful, for example in using and keeping Maxima results in R.


=== Next Steps ===

The RMaxima GSoC project has reached its aim to construct an interprocess communication between R and another process running Maxima. Command strings can be sent from R to Maxima, and returned results will be displayd in the R command terminal.

The following goals appear as natural follow-ups for a later continuation of the project:

  * Filtering commands, e.g. `mx.integrate()` for symbolic integration, or `mx.plot()` for plots; how will a set of commands be combined; what about errors and error mesages generated by Maxima, shall they be proprocessed.

  * Extend to Windows, i.e. what is needed to generate a binary Windows version, even if this cannot be provided as a binary package through CRAN. Some steps in the direction of making it a package, for instance, how can the Boost process library be distributed with the package.

  * Use Maxima results in R, i.e. use floating point numbers returned from Maxima in R calculations, or convert function expressions returned from Maxima into R functions. How can high-accuracy numbers returned from Maxima be utilized in R through the Rmpfr package.


=== Installation ===

This installation procedure for RMaxima works only on Linux systems and has been tested on Ubuntu, Mint, and Debian Linux ("squeeze"). It is assumed that a newer version of R (>= 2.15.1) is installed as usual, and that the normal C++ development environment with 'gcc' etc. is available on the Linux system. The installed Rcpp package should be >= 0.9.13

   * Install the Boost libraries (>= 1.42), the preferred way is to use the Synaptic package manager, you can also try<br>`sudo apt-get install libboost-dev`<br><br>Make sure that the Boost libraries `boost_filesystem` and `boost_regex` are included with the installation.

   * Install the Boost process library (but not the new beta version 0.5). The file provided is `BoostProcess.tar.gz` that needs to be expanded. The subdirectory `boost/process` shall be copied to the Boost directory on the Linux system, i.e. `/usr/include/boost/process/`, the file `process.hpp` into `/usr/include/boost/` <br><br>As administrator rights required, for this action you could apply the `sudo nautilus` command.<br>Don't touch the `libs` directory, it is meant for installing on Windows.

   * Install Maxima (>= 5.21) for the Linux system. Usually this is best be done through the Synaptic package manager, or  with `sudo apt-get install maxima`. Exporting the `MAXIMA_PATH` is no longer necessary.

We are now ready to install and run the RMaxima demo program:

  * Unpack the files included in the file `RMaxima-0.3.1` into a directory, would be best to call that `RMaxima` too. The directory will contain the following files:
{{{
        display.lisp    MaximaChain.cpp     MaximaChain.h
        Rmaxima.cpp     Rmaxima.R
}}}

 * Go to this directory (using a command terminal) and compile the RMaxima library with the following command:
{{{
    RCPP_CXXFLAGS=`Rscript -e 'Rcpp:::CxxFlags()'` \
    RCPP_LIBS=`Rscript -e 'Rcpp:::LdFlags()'` \
    PKG_CPPFLAGS="${RCPP_CXXFLAGS}" \
    PKG_LIBS="-lboost_filesystem -lboost_system -lboost_regex ${RCPP_LIBS}" \
    R CMD SHLIB imaxima.cpp MaximaChain.cpp
}}}
If no error occurs, this command will, among others, generate a shared library called `maximaexe.so`.

 * Start R (with the `R` command) in this directory and source the R script file that calls Maxima with some fixed demo commands  and returns Maxima's results:
{{{
        source("Rmaxima.R")
}}}
This will load the shared library and display the interaction with Maxima through fixed commands, as described in section "Project Results" above.

If starting RMaxima by hand, use the following commands (or put them in a starting procedure of your own):

<code language="r">
    > library("Rcpp")
    > m <- Module("Maxima", dyn.load("Rmaxima.so"))
    > mx.start <- function() mxm <<- new(m$RMaxima)
    > mx.exec <- function(s) mxm$execute(as.character(s))
    > mx.start()
    > mx.exec("...")  # valid Maxima command
</code>

After Maxima started, you can try to execute Maxima commands through the `mx.exec("...")` where "..." shall be a syntactically valid Maxima command. See the examples in the 'Project Results' section above.


=== Project Details ==

Interaction with the Maxima has been implemented as a single class called `MaximaChain` that allows to separate the functionality that is responsible for interaction with the Maxima process and the code that provides integration with R. For creating a new connection to the Maxima that’s enough to create an instance of the `MaximaChain` class.

`MaximaChain` class represents a proxy to an instance of a Maxima process.`MaximaChain` allows interaction with the Maxima at more higher level than approach with the sockets. User just can send a command and receive a result rather than write crude bytes to a socket or a pipe connected to the Maxima process's input/output streams. To kill a Maxima process you should delete `MaximaChain` object.

The list of general tasks is:

  # Launching Maxima process and keeping it for sequential user input;
  # Ability to send command to Maxima and return result in convenient format;
  # Handling errors messages generated by Maxima and errors that occurs after command execution (when call to Maxima returns an error);
  # Checking the validity of input expressions;
  # Correctly stopping Maxima process and removing appropriate object without memory and resource leaks. 

The main functions and methods implemented in `MaximaChain` class that provides listed requirements are

1. `MaximaChain` constructor.<br>
Creates an object to interact with Maxima and launches Maxima as a child process.<br>
Have three parameters: full path to Maxima executable file, path to directory for output files, path to directory that contains supporting files.

2. `MaximaChain` destructor.<br>
Only sending to Maxima process command “quit()” not enough to destroy MaximaChain object because Windows cleans up resources only when all handles are closed. MaximaChain destructor closes all handles and waits for child process termination.

3. `executeCommand` function.<br>
Sends command to Maxima and returns Maxima output after last command and ending just before the prompt starts.

4. `executeCommandList` function.<br>
Passes its argument as the following command of Maxima process and returns the entire response to this command Maxima, including errors.

5. `sendCommand` function.<br>
Just send a command to the Maxima. Command must end with ';' or '$'. In another case we append ';' character.

6. `checkInput function`.<br>
Check if the input expression is valid. Initialized by a zero value.<br>
Have two parameters: next expression character to process and current checker state.
Return new checker state. The final state is STATE_END.

7. `Struct` Reply.<br>
Reads Maxima output until prompt is found and then parses it. Have one input parameter - input stream where Maxima writes its output.

For manipulating C++ objects in R were used Rcpp modules. This approach allows create instances of C++ classes, retrieve/set data members and call methods. External pointers are also useful for that but Rcpp modules wraps them in a nice to use abstraction.

The module is created in a C++ file using the RCPP_MODULE macro, which then contains declarative code of what the module exposes to R.

Since C++ does not have reflection capabilities, modules need declarations of what to expose: constructors, fields or properties, methods or finalizers.

See "Exposing C++ functions and classes with Rcpp modules", Dirk Eddelbuettel and Romain Francois, July 9, 2010.

For using MaximaChain instance and handling errors was created suitable interface in RMaxima C++ class and Rcpp module called Maxima for exposing this functionality to R.

<code language="cpp">
    RCPP_MODULE(Maxima)
    {
        class_<RMaxima>( "RMaxima")
        .constructor()
        .method("execute", &RMaxima::execute)
        .finalizer(&rmaxima_finalizer) ;
    } 
</code>

When the R reference object that wraps the internal C++ object goes out of scope, it becomes candidate for GC. When it is GC’ed, the destructor of target class is called. Finalizers allows to add behavior right before the destructor is called.

For using Maxima process in R were implemented following functions:

  * mx.start() that creates an instance of the Maxima module and launches the Maxima process;
  * mx.exec(character(x)) that executes a Maxima command and returns the result to R.

Stopping Maxima manually and integrating code into an R package will be done in one of the next versions.
