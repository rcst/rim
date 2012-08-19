#include <Rcpp.h>

#include "MaximaChain.h"
#include "boost/process.hpp"
#include "boost/filesystem.hpp"


extern "C"
{
using namespace Rcpp;


namespace bp = ::boost::process;
namespace fs = ::boost::filesystem;

class RMaxima 
{
  public:
    RMaxima()
    {
        std::string maxpath = bp::find_executable_in_path("maxima");
        std::string workDir = fs::current_path().string();
        std::string utilsDir = fs::current_path().string();
        myMaxima = new Maxima::MaximaChain(maxpath, workDir, utilsDir);
    }

    ~RMaxima()
    {
        delete myMaxima;
    }

    std::string execute(std::string command)
    {
        try
        {
            std::string result = myMaxima->executeCommand(command);
            return result;
        }

        catch (const std::exception &ex)
        {
            forward_exception_to_r(ex);
        }
    }

  private:
    Maxima::MaximaChain* myMaxima;

};

static void rmaxima_finalizer(RMaxima* ptr)
{
    if (ptr)
    { 
        delete ptr;
    }
}

RCPP_MODULE(Maxima)
{
    class_<RMaxima>( "RMaxima")
    .constructor()
    .method("execute", &RMaxima::execute)
    .finalizer(&rmaxima_finalizer) ;
} 

}  // extern "C"
