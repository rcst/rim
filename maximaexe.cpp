#include <R.h>

#include "MaximaChain.h"
#include "boost/process.hpp"
#include "boost/filesystem.hpp"
#include <boost/scoped_ptr.hpp>

extern "C"
{

namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;

void maximaexe(char** command)
{
    Rprintf("Calling C function\n");
    std::string maxpath = bp::find_executable_in_path("maxima");
    std::string workDir = fs::current_path().string();
    std::string utilsDir = fs::current_path().string();
    boost::scoped_ptr<Maxima::MaximaChain> maxima(new Maxima::MaximaChain(maxpath, workDir, utilsDir));

//    while (true)
//    {
        try
        {
            Rprintf("> ");
            std::string cmd(*command);
            Rprintf("%s", *command);
            Rprintf("\n");
          //  if (cmd == "q")
              //  break;
            std::string result = maxima->executeCommand(cmd);

            char* resptr = new char[result.size()+1];
            strcpy (resptr, result.c_str());
            Rprintf("%s", resptr);
            Rprintf("\n");
            delete[] resptr;
        }

        catch (const std::exception &ex)
        {
            Rprintf("Comand is wrong!\n");
            //std::cout<<"Exception: "<<ex.what()<<std::endl;
        }
 //   }
    Rprintf("Returning to R\n");
}

}  // extern "C"
